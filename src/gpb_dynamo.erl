-module(gpb_dynamo).

-behaviour(gen_server).

-export([start_link/0]).
-export([set/3, get/1, create_table_if_missing/0, clear_table/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-include_lib("erlcloud/include/erlcloud_aws.hrl").
-include_lib("erlcloud/include/erlcloud_ddb2.hrl").

% Maximum number of failed delete attempts per item before clear_table gives up.
-define(MAX_ITEM_RETRIES, 3).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

set(Key, EncBlob, EncKey) when is_binary(Key), is_binary(EncBlob), is_binary(EncKey) ->
    gen_server:call(?MODULE, {set, Key, EncBlob, EncKey}).

get(Key) when is_binary(Key) ->
    gen_server:call(?MODULE, {get, Key}).

create_table_if_missing() ->
    gen_server:call(?MODULE, create_table_if_missing).

% Scan the table and delete every item. The table itself is preserved.
% Called from the CT suite's end_per_suite so each test run starts clean.
clear_table() ->
    gen_server:call(?MODULE, clear_table).

init([]) ->
    Required = [
        "DYNAMO_TABLE",
        "DYNAMO_ENDPOINT",
        "AWS_ACCESS_KEY_ID",
        "AWS_SECRET_ACCESS_KEY",
        "AWS_DEFAULT_REGION"
    ],
    case gpb_env:read_env(Required) of
        {error, Reason} ->
            {stop, Reason};
        {ok, Env} ->
            {Scheme, Host, Port} = gpb_env:parse_endpoint(maps:get("DYNAMO_ENDPOINT", Env)),
            Config = #aws_config{
                access_key_id     = maps:get("AWS_ACCESS_KEY_ID", Env),
                secret_access_key = maps:get("AWS_SECRET_ACCESS_KEY", Env),
                ddb_host          = Host,
                ddb_port          = Port,
                ddb_scheme        = Scheme
            },
            Table = list_to_binary(maps:get("DYNAMO_TABLE", Env)),
            {ok, #{table => Table, config => Config}}
    end.

handle_call({set, Key, EncBlob, EncKey}, _From, State = #{table := Table, config := Config}) ->
    Item   = [
        {<<"key">>,                {s, Key}},
        {<<"encrypted_value">>,    {b, EncBlob}},
        {<<"encrypted_data_key">>, {b, EncKey}}
    ],
    Result = case erlcloud_ddb2:put_item(Table, Item, [], Config) of
        {ok, _}         -> ok;
        {error, Reason} -> {error, Reason}
    end,
    {reply, Result, State};

handle_call({get, Key}, _From, State = #{table := Table, config := Config}) ->
    KeySpec = [{<<"key">>, {s, Key}}],
    Result  = case erlcloud_ddb2:get_item(Table, KeySpec, [], Config) of
        {ok, []}   -> {error, not_found};
        {ok, Item} ->
            EncBlob = proplists:get_value(<<"encrypted_value">>,    Item),
            EncKey  = proplists:get_value(<<"encrypted_data_key">>, Item),
            {ok, {EncBlob, EncKey}};
        {error, Reason} -> {error, Reason}
    end,
    {reply, Result, State};

handle_call(create_table_if_missing, _From, State = #{table := Table, config := Config}) ->
    Result = ensure_table(Table, Config),
    {reply, Result, State};

handle_call(clear_table, _From, State = #{table := Table, config := Config}) ->
    Result = do_clear_table(Table, Config),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

% Scan all items in the table and delete each one by its key.
% DynamoDB has no truncate operation: scan + per-item delete is the only way.
%
% Retries failed deletes on each subsequent scan pass. FailureCounts tracks
% how many times each key has failed; once a key hits MAX_ITEM_RETRIES the
% function gives up and returns an error rather than looping forever.
%
% scan_all/2 handles DynamoDB pagination: a single Scan call returns at most
% 1 MB of results. We follow LastEvaluatedKey until it is absent, collecting
% all items before attempting any deletes.
do_clear_table(Table, Config) ->
    do_clear_table(Table, Config, #{}).

do_clear_table(Table, Config, FailureCounts) ->
    case scan_all(Table, Config) of
        {error, Reason} ->
            {error, Reason};
        {ok, []} ->
            ok;
        {ok, Items} ->
            case delete_items(Items, Table, Config, FailureCounts) of
                {ok, NewCounts}  -> do_clear_table(Table, Config, NewCounts);
                {error, _} = Err -> Err
            end
    end.

scan_all(Table, Config) ->
    scan_all(Table, Config, undefined, []).

scan_all(Table, Config, StartKey, Acc) ->
    Opts = [{out, record}] ++ case StartKey of
        undefined -> [];
        Key       -> [{exclusive_start_key, Key}]
    end,
    case erlcloud_ddb2:scan(Table, Opts, Config) of
        {error, Reason} ->
            {error, Reason};
        {ok, #ddb2_scan{items = PageItems, last_evaluated_key = NextKey}} ->
            Items  = case PageItems of undefined -> []; _ -> PageItems end,
            NewAcc = Acc ++ Items,
            case NextKey of
                undefined -> {ok, NewAcc};
                _         -> scan_all(Table, Config, NextKey, NewAcc)
            end
    end.

% Attempt to delete every item in the list, updating the per-key failure count
% for each one that fails. Returns {ok, NewCounts} so the caller can re-scan
% and retry, or {error, Reason} if a key has hit MAX_ITEM_RETRIES.
delete_items(Items, Table, Config, FailureCounts) ->
    lists:foldl(fun
        (Item, {ok, Counts}) ->
            Key = proplists:get_value(<<"key">>, Item),
            case erlcloud_ddb2:delete_item(Table, [{<<"key">>, {s, Key}}], [], Config) of
                {ok, _} ->
                    {ok, maps:remove(Key, Counts)};
                {error, Reason} ->
                    NewCount = maps:get(Key, Counts, 0) + 1,
                    if NewCount >= ?MAX_ITEM_RETRIES ->
                        {error, {max_retries_exceeded, Key, Reason}};
                    true ->
                        {ok, Counts#{Key => NewCount}}
                    end
            end;
        (_Item, Error) ->
            Error
    end, {ok, FailureCounts}, Items).

ensure_table(Table, Config) ->
    case erlcloud_ddb2:describe_table(Table, [], Config) of
        {ok, _} ->
            ok;
        {error, {<<"ResourceNotFoundException">>, _}} ->
            AttrDefs = [{<<"key">>, s}],
            KeySchema = <<"key">>,
            case erlcloud_ddb2:create_table(Table, AttrDefs, KeySchema, 5, 5, [], Config) of
                {ok, _}         -> ok;
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.
