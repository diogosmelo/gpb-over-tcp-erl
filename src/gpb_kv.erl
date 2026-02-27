-module(gpb_kv).

-export([set/2, get/1]).

% Encrypt the value and store the ciphertext + encrypted data key in DynamoDB.
% The key is stored as plaintext — it is the DynamoDB primary key used for lookup.
% The plaintext data key is never stored; only the KMS-encrypted copy goes to DynamoDB.
set(Key, Value) when is_binary(Key), is_binary(Value) ->
    case gpb_kms:encrypt(Value) of
        {error, Reason}         -> {error, Reason};
        {ok, {EncBlob, EncKey}} -> gpb_dynamo:set(Key, EncBlob, EncKey)
    end.

% Fetch the ciphertext + encrypted data key from DynamoDB, then decrypt with KMS.
get(Key) when is_binary(Key) ->
    case gpb_dynamo:get(Key) of
        {error, Reason}         -> {error, Reason};
        {ok, {EncBlob, EncKey}} -> gpb_kms:decrypt(EncBlob, EncKey)
    end.
