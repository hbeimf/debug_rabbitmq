-module(glib_test).
-compile(export_all).
-include_lib("glib/include/log.hrl").

encode(Bin) ->
    Res = zlib:compress(glib:to_binary(Bin)),
    base64:encode(Res).


decode(Bin) ->
    zlib:uncompress(base64:decode(Bin)).


% test 
% ====================================================
% 字符串越大， 压缩越明显 ， 字符串越小， 压缩没什么 用， 
tt1() -> 
    From = <<"test string!">>,
    From1 = <<From/binary, From/binary,From/binary,From/binary, From/binary,From/binary,From/binary, From/binary,From/binary,From/binary, From/binary,From/binary,From/binary>>,
    test(From1).


tt2() ->
    From = glib:to_binary(glib:uid()),
    From1 = <<From/binary, From/binary,From/binary,From/binary, From/binary,From/binary,From/binary, From/binary,From/binary,From/binary, From/binary,From/binary,From/binary>>,
    test(From1).


tt3() -> 
    From = glib:to_binary(glib:uid()),
    % From1 = <<From/binary, From/binary,From/binary,From/binary, From/binary,From/binary,From/binary, From/binary,From/binary,From/binary, From/binary,From/binary,From/binary>>,
    test(From).


test(From1) -> 
    ?LOG({from, byte_size(From1), From1}),

    Encode = encode(From1),
    ?LOG({encode, byte_size(Encode), Encode}),

    % Base64 = base64:encode(Encode),
    % ?LOG({base64, byte_size(Base64), Base64}),

    Decode = decode(Encode),
    ?LOG({decode, byte_size(Decode), Decode}),

    ok.



