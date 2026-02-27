-module(gpb_kms).

-include_lib("erlcloud/include/erlcloud_aws.hrl").

-export([encrypt/1, decrypt/2]).

%% Encrypts Value using envelope encryption with AWS KMS (Key Management Service).
encrypt(Value) when is_binary(Value) ->
    Required = [
        "KMS_ENDPOINT",
        "KMS_KEY_ARN",
        "AWS_ACCESS_KEY_ID",
        "AWS_SECRET_ACCESS_KEY",
        "AWS_DEFAULT_REGION"
    ],
    case read_env(Required) of
        {error, Reason} ->
            {error, Reason};
        {ok, Env} ->
            Config = build_config(Env),
            KeyArn = list_to_binary(maps:get("KMS_KEY_ARN", Env)),
            case erlcloud_kms:generate_data_key(KeyArn, [{key_spec, <<"AES_256">>}], Config) of
                {error, Reason} ->
                    {error, Reason};
                {ok, Response} ->
                    PlaintextKey     = base64:decode(proplists:get_value(<<"Plaintext">>,     Response)),
                    EncryptedDataKey = base64:decode(proplists:get_value(<<"CiphertextBlob">>, Response)),
                    EncryptedBlob    = aes_gcm_encrypt(Value, PlaintextKey),
                    {ok, {EncryptedBlob, EncryptedDataKey}}
            end
    end.

%% Decrypts an encrypted blob produced by encrypt/1, using the provided encrypted data key.
decrypt(EncryptedBlob, EncryptedDataKey)
        when is_binary(EncryptedBlob), is_binary(EncryptedDataKey) ->
    Required = [
        "KMS_ENDPOINT",
        "AWS_ACCESS_KEY_ID",
        "AWS_SECRET_ACCESS_KEY",
        "AWS_DEFAULT_REGION"
    ],
    case read_env(Required) of
        {error, Reason} ->
            {error, Reason};
        {ok, Env} ->
            Config = build_config(Env),
            case erlcloud_kms:decrypt(base64:encode(EncryptedDataKey), [], Config) of
                {error, Reason} ->
                    {error, Reason};
                {ok, Response} ->
                    PlaintextKey = base64:decode(proplists:get_value(<<"Plaintext">>, Response)),
                    case aes_gcm_decrypt(EncryptedBlob, PlaintextKey) of
                        error     -> {error, decryption_failed};
                        Plaintext -> {ok, Plaintext}
                    end
            end
    end.

%%%-------------------------------------------------------------------
%%% Internal — AES-256-GCM (Advanced Encryption Standard 256-bit
%%%             Galois/Counter Mode) local crypto
%%%-------------------------------------------------------------------

%% Encrypts Plaintext with the given 32-byte Key.
%%
%% A fresh random 12-byte IV (Initialization Vector) is generated for every
%% call. Reusing an IV with the same key would break GCM security, so we
%% never hardcode or cache it.
%%
%% The result is a single binary: <<IV:12, Tag:16, Ciphertext/binary>>.
%% The IV and authentication Tag are prepended at fixed sizes so that
%% aes_gcm_decrypt/2 can split them back out unambiguously.
aes_gcm_encrypt(Plaintext, Key) ->
    IV = crypto:strong_rand_bytes(12),
    {Ciphertext, Tag} = crypto:crypto_one_time_aead(
        aes_256_gcm, Key, IV, Plaintext,
        <<>>,   
        16,     
        true    
    ),
    <<IV:12/binary, Tag:16/binary, Ciphertext/binary>>.

%% Decrypts a blob produced by aes_gcm_encrypt/2.
%%
%% Splits the blob into IV, Tag, and Ciphertext, then decrypts.
%% Returns the plaintext on success.
%% Returns the atom `error` if the blob is too short to be valid, or if
%% GCM authentication fails (meaning the data was tampered with or the
%% wrong key was used). The caller in decrypt/2 converts this to
%% {error, decryption_failed}.
aes_gcm_decrypt(Blob, _Key) when byte_size(Blob) < 28 ->
    % 12 bytes IV + 16 bytes Tag = 28 bytes minimum. A shorter blob is corrupted.
    error;
aes_gcm_decrypt(<<IV:12/binary, Tag:16/binary, Ciphertext/binary>>, Key) ->
    crypto:crypto_one_time_aead(
        aes_256_gcm, Key, IV, Ciphertext,
        <<>>,   
        Tag,    
        false  
    ).

read_env(Vars) ->
    read_env(Vars, #{}).

read_env([], Acc) ->
    {ok, Acc};
read_env([Var | Rest], Acc) ->
    case os:getenv(Var) of
        false -> {error, {missing_env, Var}};
        Value -> read_env(Rest, Acc#{Var => Value})
    end.

build_config(Env) ->
    {Scheme, Host, Port} = parse_endpoint(maps:get("KMS_ENDPOINT", Env)),
    #aws_config{
        access_key_id     = maps:get("AWS_ACCESS_KEY_ID",     Env),
        secret_access_key = maps:get("AWS_SECRET_ACCESS_KEY", Env),
        kms_host          = Host,
        kms_port          = Port,
        kms_scheme        = Scheme
    }.

parse_endpoint(Url) ->
    Map    = uri_string:parse(Url),
    Scheme = maps:get(scheme, Map),
    Host   = maps:get(host, Map),
    Port   = maps:get(port, Map, default_port(Scheme)),
    % aws_config expects the scheme with the "://" suffix included.
    {Scheme ++ "://", Host, Port}.

default_port("https") -> 443;
default_port("http")  -> 80.
