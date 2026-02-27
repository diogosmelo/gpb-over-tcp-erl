-module(gpb_kms_SUITE).

-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([encrypt_decrypt_roundtrip/1]).

all() ->
    [encrypt_decrypt_roundtrip].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(gpb_over_tcp_erl),
    Config.

end_per_suite(_Config) ->
    application:stop(gpb_over_tcp_erl),
    ok.

encrypt_decrypt_roundtrip(_Config) ->
    Value = <<"hello, encrypted world">>,

    {ok, {EncryptedBlob, EncryptedDataKey}} = gpb_kms:encrypt(Value),

    true = is_binary(EncryptedBlob),
    true = is_binary(EncryptedDataKey),
    true = byte_size(EncryptedBlob)    > 0,
    true = byte_size(EncryptedDataKey) > 0,

    true = EncryptedBlob =/= Value,

    {ok, Value} = gpb_kms:decrypt(EncryptedBlob, EncryptedDataKey).
