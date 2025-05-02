-module(safebox_crypto).
-export([encrypt/1, decrypt/1]).

%% Encrypt a binary using Base64 encoding
encrypt(PlainText) when is_binary(PlainText) ->
    base64:encode(PlainText).

%% Decrypt a Base64-encoded binary
decrypt(EncodedText) when is_binary(EncodedText) ->
    case catch base64:decode(EncodedText) of
        {'EXIT', _} -> {error, invalid_base64};
        Decoded -> {ok, Decoded}
    end.
