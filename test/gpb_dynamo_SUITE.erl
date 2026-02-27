-module(gpb_dynamo_SUITE).

-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([set_and_get/1, get_missing_key/1]).

all() ->
    [set_and_get, get_missing_key].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(gpb_over_tcp_erl),
    ok = gpb_dynamo:create_table_if_missing(),
    Config.

end_per_suite(_Config) ->
    ok = gpb_dynamo:clear_table(),
    application:stop(gpb_over_tcp_erl),
    ok.

set_and_get(_Config) ->
    Key     = <<"test-key-1">>,
    EncBlob = <<"fake-encrypted-blob">>,
    EncKey  = <<"fake-encrypted-key">>,
    ok                      = gpb_dynamo:set(Key, EncBlob, EncKey),
    {ok, {EncBlob, EncKey}} = gpb_dynamo:get(Key).

get_missing_key(_Config) ->
    {error, not_found} = gpb_dynamo:get(<<"no-such-key">>).
