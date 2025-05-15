-module(safebox_crypto).
-export([encrypt/1, decrypt/1]).

-define(KEY, <<16#00,16#01,16#02,16#03,16#04,16#05,16#06,16#07,
               16#08,16#09,16#0A,16#0B,16#0C,16#0D,16#0E,16#0F,
               16#10,16#11,16#12,16#13,16#14,16#15,16#16,16#17,
               16#18,16#19,16#1A,16#1B,16#1C,16#1D,16#1E,16#1F>>). % 32 bytes = 256-bit
-define(IV,  <<0:128>>). % 128-bit IV (all zero for simplicity)

encrypt(PlainBin) when is_binary(PlainBin) ->
    Padded = pad(PlainBin),
    Cipher = crypto:crypto_one_time(aes_256_cbc, ?KEY, ?IV, Padded, true),
    base64:encode(Cipher).

decrypt(Base64Cipher) when is_binary(Base64Cipher) ->
    try
        Cipher = base64:decode(Base64Cipher),
        PlainPadded = crypto:crypto_one_time(aes_256_cbc, ?KEY, ?IV, Cipher, false),
        {ok, unpad(PlainPadded)}
    catch
        _ -> {error, invalid_decryption}
    end.

%% Padding (PKCS#7)
pad(Bin) ->
    BlockSize = 16,
    PadLen = BlockSize - (byte_size(Bin) rem BlockSize),
    Padding = binary:copy(<<PadLen>>, PadLen),
    <<Bin/binary, Padding/binary>>.

unpad(Bin) ->
    PadLen = binary:last(Bin),
    BinLen = byte_size(Bin),
    <<Content: (BinLen - PadLen)/binary, _/binary>> = Bin,
    Content.
