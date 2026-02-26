-module(gpb_dynamo).

-behaviour(gen_server).

-export([start_link/0]).
-export([set/2, get/1, create_table_if_missing/0, clear_table/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-include_lib("erlcloud/include/erlcloud_aws.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

set(Key, Value) when is_binary(Key), is_binary(Value) ->
    gen_server:call(?MODULE, {set, Key, Value}).

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
    case read_env(Required) of
        {error, Reason} ->
            {stop, Reason};
        {ok, Env} ->
            {Scheme, Host, Port} = parse_endpoint(maps:get("DYNAMO_ENDPOINT", Env)),
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

handle_call({set, Key, Value}, _From, State = #{table := Table, config := Config}) ->
    Item   = [{<<"key">>, {s, Key}}, {<<"value">>, {s, Value}}],
    Result = case erlcloud_ddb2:put_item(Table, Item, [], Config) of
        {ok, _}         -> ok;
        {error, Reason} -> {error, Reason}
    end,
    {reply, Result, State};

handle_call({get, Key}, _From, State = #{table := Table, config := Config}) ->
    KeySpec = [{<<"key">>, {s, Key}}],
    Result  = case erlcloud_ddb2:get_item(Table, KeySpec, [], Config) of
        {ok, []}        -> {error, not_found};
        {ok, Item}      -> {ok, proplists:get_value(<<"value">>, Item)};
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

read_env(Vars) ->
    read_env(Vars, #{}).

read_env([], Acc) ->
    {ok, Acc};
read_env([Var | Rest], Acc) ->
    case os:getenv(Var) of
        false -> {error, {missing_env, Var}};
        Value -> read_env(Rest, Acc#{Var => Value})
    end.

parse_endpoint(Url) ->
    Map    = uri_string:parse(Url),
    Scheme = maps:get(scheme, Map),
    Host   = maps:get(host, Map),
    Port   = maps:get(port, Map, default_port(Scheme)),
    % aws_config expects the scheme with the "://" suffix included.
    {Scheme ++ "://", Host, Port}.

default_port("https") -> 443;
default_port("http")  -> 80.

% Scan all items in the table and delete each one by its key.
% DynamoDB has no truncate operation — scan + per-item delete is the only way.
% For test data volumes this is fine; it would be slow on a large dataset.
do_clear_table(Table, Config) ->
    case erlcloud_ddb2:scan(Table, [], Config) of
        {ok, Items} ->
            lists:foreach(fun(Item) ->
                Key = proplists:get_value(<<"key">>, Item),
                erlcloud_ddb2:delete_item(Table, [{<<"key">>, {s, Key}}], [], Config)
            end, Items),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

ensure_table(Table, Config) ->
    case erlcloud_ddb2:describe_table(Table, [], Config) of
        {ok, _} ->
            ok;
        {error, _} ->
            % Table does not exist — create it.
            % Provisioned capacity values are required by the API but are not
            % enforced by DynamoDB Local.
            AttrDefs = [{<<"key">>, s}],
            KeySchema = <<"key">>,
            {ok, _} = erlcloud_ddb2:create_table(Table, AttrDefs, KeySchema, 5, 5, [], Config),
            ok
    end.
