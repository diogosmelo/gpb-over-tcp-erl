-module(gpb_kv_SUITE).

-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([set_ok/1, set_kms_error/1,
         get_ok/1, get_not_found/1, get_kms_error/1]).

all() ->
    [set_ok, set_kms_error, get_ok, get_not_found, get_kms_error].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    meck:unload(),
    ok.

set_ok(_Config) ->
    Key   = <<"k">>,
    Value = <<"v">>,
    Blob  = <<"blob">>,
    EKey  = <<"ekey">>,
    meck:new(gpb_kms,    []),
    meck:new(gpb_dynamo, []),
    meck:expect(gpb_kms,    encrypt, fun(_)       -> {ok, {Blob, EKey}} end),
    meck:expect(gpb_dynamo, set,     fun(_, _, _) -> ok end),
    ok   = gpb_kv:set(Key, Value),
    true = meck:called(gpb_kms,    encrypt, [Value]),
    true = meck:called(gpb_dynamo, set,     [Key, Blob, EKey]).

set_kms_error(_Config) ->
    Key   = <<"k">>,
    Value = <<"v">>,
    meck:new(gpb_kms,    []),
    meck:new(gpb_dynamo, []),
    meck:expect(gpb_kms, encrypt, fun(_) -> {error, kms_unavailable} end),
    {error, kms_unavailable} = gpb_kv:set(Key, Value).

get_ok(_Config) ->
    Key   = <<"k">>,
    Value = <<"v">>,
    Blob  = <<"blob">>,
    EKey  = <<"ekey">>,
    meck:new(gpb_dynamo, []),
    meck:new(gpb_kms,    []),
    meck:expect(gpb_dynamo, get,     fun(_)    -> {ok, {Blob, EKey}} end),
    meck:expect(gpb_kms,    decrypt, fun(_, _) -> {ok, Value} end),
    {ok, Value} = gpb_kv:get(Key),
    true = meck:called(gpb_dynamo, get,     [Key]),
    true = meck:called(gpb_kms,    decrypt, [Blob, EKey]).

get_not_found(_Config) ->
    Key = <<"k">>,
    meck:new(gpb_dynamo, []),
    meck:new(gpb_kms,    []),
    meck:expect(gpb_dynamo, get, fun(_) -> {error, not_found} end),
    {error, not_found} = gpb_kv:get(Key).

get_kms_error(_Config) ->
    Key  = <<"k">>,
    Blob = <<"blob">>,
    EKey = <<"ekey">>,
    meck:new(gpb_dynamo, []),
    meck:new(gpb_kms,    []),
    meck:expect(gpb_dynamo, get,     fun(_)    -> {ok, {Blob, EKey}} end),
    meck:expect(gpb_kms,    decrypt, fun(_, _) -> {error, decryption_failed} end),
    {error, decryption_failed} = gpb_kv:get(Key).
