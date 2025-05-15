-module(safebox_crypto).
-export([encrypt/1, decrypt/1]).

encrypt(PlainText) when is_binary(PlainText) ->
    base64:encode(PlainText).

decrypt(EncodedText) when is_binary(EncodedText) ->
    try
        {ok, base64:decode(EncodedText)}
    catch
        _ -> {error, invalid_base64}
    end.