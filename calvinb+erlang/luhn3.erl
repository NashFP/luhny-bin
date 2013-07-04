% In this version I started using binaries instead of strings
% and relied much more heavily on pattern matching across multiple characters.
% The guard clauses are hideous and repetitive. They are nearly half the code.
% Should they be moved into a function?
% I thought this version would at least be faster, but it's slightly slower than
% my previous, list-oriented efforts.

-module(luhn3).
-export([main/1,start/0,process_line/1]).

main(_) -> 
    start().

start() -> 
    io:setopts([{binary, true}]),
    process_line(io:get_line("")).

process_line(<<"\n">>) ->
    ok;
process_line(Line) ->
    io:fwrite("~s", [transform(Line, {0, <<"">>})]),
    process_line(io:get_line("")).

% empty input
transform(<<"\n">>, {_, Result}) ->
    <<Result/binary, "\n">>;
% 16 digits
transform(
    <<A:8, B:8, C:8, D:8, E:8, F:8, G:8, H:8, I:8, J:8, K:8, L:8, M:8, N:8, O:8, P:8, Rest/binary>>, 
    {MaskCount, Result})
when A >= $0 andalso A =< $9
andalso B >= $0 andalso B =< $9
andalso C >= $0 andalso C =< $9
andalso D >= $0 andalso D =< $9
andalso E >= $0 andalso E =< $9
andalso F >= $0 andalso F =< $9
andalso G >= $0 andalso G =< $9
andalso H >= $0 andalso H =< $9
andalso I >= $0 andalso I =< $9
andalso J >= $0 andalso J =< $9
andalso K >= $0 andalso K =< $9
andalso L >= $0 andalso L =< $9
andalso M >= $0 andalso M =< $9
andalso N >= $0 andalso N =< $9
andalso O >= $0 andalso O =< $9
andalso P >= $0 andalso P =< $9 ->
    NextChunk = <<B:8, C:8, D:8, E:8, F:8, G:8, H:8, I:8, J:8, K:8, L:8, M:8, N:8, O:8, P:8, Rest/binary>>,
    Chars = [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P],
    transform(NextChunk, new_mask_count_and_result(Chars, A, MaskCount, Result));
% 16 digits with spaces
transform(
    <<A:8, B:8, C:8, D:8, Delim, E:8, F:8, G:8, H:8, Delim, I:8, J:8, K:8, L:8, Delim, M:8, N:8, O:8, P:8, Rest/binary>>, 
    {MaskCount, Result})
when A >= $0 andalso A =< $9
andalso B >= $0 andalso B =< $9
andalso C >= $0 andalso C =< $9
andalso D >= $0 andalso D =< $9
andalso E >= $0 andalso E =< $9
andalso F >= $0 andalso F =< $9
andalso G >= $0 andalso G =< $9
andalso H >= $0 andalso H =< $9
andalso I >= $0 andalso I =< $9
andalso J >= $0 andalso J =< $9
andalso K >= $0 andalso K =< $9
andalso L >= $0 andalso L =< $9
andalso M >= $0 andalso M =< $9
andalso N >= $0 andalso N =< $9
andalso O >= $0 andalso O =< $9
andalso P >= $0 andalso P =< $9
andalso (Delim =:= 32 orelse Delim =:= 45) ->
    NextChunk = <<B:8, C:8, D:8, Delim, E:8, F:8, G:8, H:8, Delim, I:8, J:8, K:8, L:8, Delim, M:8, N:8, O:8, P:8, Rest/binary>>,
    Chars = [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P],
    transform(NextChunk, new_mask_count_and_result(Chars, A, MaskCount, Result));
transform(
    <<A:8, B:8, C:8, D:8, E:8, F:8, G:8, H:8, I:8, J:8, K:8, L:8, M:8, N:8, O:8, Rest/binary>>, 
    {MaskCount, Result})
when A >= $0 andalso A =< $9
andalso B >= $0 andalso B =< $9
andalso C >= $0 andalso C =< $9
andalso D >= $0 andalso D =< $9
andalso E >= $0 andalso E =< $9
andalso F >= $0 andalso F =< $9
andalso G >= $0 andalso G =< $9
andalso H >= $0 andalso H =< $9
andalso I >= $0 andalso I =< $9
andalso J >= $0 andalso J =< $9
andalso K >= $0 andalso K =< $9
andalso L >= $0 andalso L =< $9
andalso M >= $0 andalso M =< $9
andalso N >= $0 andalso N =< $9
andalso O >= $0 andalso O =< $9 ->
    NextChunk = <<B:8, C:8, D:8, E:8, F:8, G:8, H:8, I:8, J:8, K:8, L:8, M:8, N:8, O:8, Rest/binary>>,
    Chars = [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O],
    transform(NextChunk, new_mask_count_and_result(Chars, A, MaskCount, Result));
transform(
    <<A:8, B:8, C:8, D:8, E:8, F:8, G:8, H:8, I:8, J:8, K:8, L:8, M:8, N:8, Rest/binary>>, 
    {MaskCount, Result})
when A >= $0 andalso A =< $9
andalso B >= $0 andalso B =< $9
andalso C >= $0 andalso C =< $9
andalso D >= $0 andalso D =< $9
andalso E >= $0 andalso E =< $9
andalso F >= $0 andalso F =< $9
andalso G >= $0 andalso G =< $9
andalso H >= $0 andalso H =< $9
andalso I >= $0 andalso I =< $9
andalso J >= $0 andalso J =< $9
andalso K >= $0 andalso K =< $9
andalso L >= $0 andalso L =< $9
andalso M >= $0 andalso M =< $9
andalso N >= $0 andalso N =< $9 ->
    NextChunk = <<B:8, C:8, D:8, E:8, F:8, G:8, H:8, I:8, J:8, K:8, L:8, M:8, N:8, Rest/binary>>,
    Chars = [A,B,C,D,E,F,G,H,I,J,K,L,M,N],
    transform(NextChunk, new_mask_count_and_result(Chars, A, MaskCount, Result));
transform(<<A:8, Rest/binary>>, {MaskCount, Result})
when A >= $0 andalso A =< $9 ->
    Char = result_char(A, MaskCount),
    transform(Rest, {MaskCount - 1, <<Result/binary, Char/binary>>});
transform(<<A:8, Rest/binary>>, {MaskCount, Result}) ->
    transform(Rest, {MaskCount, <<Result/binary, A:8>>}).

get_mod(Digits) ->
    {Sum, _} = lists:foldr(
        fun(X, {Sum, false}) ->
                {Sum + X, true};
            (X, {Sum, true}) ->
                X2 = X * 2,
                {Sum + (X2 div 10) + (X2 rem 10), false}
        end,
        {0, false},
        lists:map(fun(X) -> X - $0 end, Digits)),
    Sum rem 10.

result_char(Char, MaskCount) ->
    case MaskCount > 0 of
        true ->
            <<"X">>;
        _ ->
            <<Char:8>>
    end.

new_mask_count_and_result(Chars, A, MaskCount, Result) ->
    case get_mod(Chars) of
        0 ->
            {
                max(length(Chars) - 1, MaskCount - 1),
                <<Result/binary, "X">>
            };
        _ ->
            Char = result_char(A, MaskCount),
            {
                MaskCount - 1,
                <<Result/binary, Char/binary>>
            }
    end.
