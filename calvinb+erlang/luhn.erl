-module(luhn).
-mode(compile).
-export([main/1,start/0,transform_line/1]).
-define(MinLength, 14).
-define(MaxLength, 16).

main(_) -> start().

start() -> process_line(io:get_line("")).

process_line([10|_]) -> ok;
process_line(Line) ->
    io:fwrite("~s", [transform_line(Line)]),
    start().

is_digit($0) -> true;
is_digit($1) -> true;
is_digit($2) -> true;
is_digit($3) -> true;
is_digit($4) -> true;
is_digit($5) -> true;
is_digit($6) -> true;
is_digit($7) -> true;
is_digit($8) -> true;
is_digit($9) -> true;
is_digit(_) -> false.

% To start, call split_on_digit and toss a chunk straight to the result.
% Then, from the first digit, call max_luhn_length on the line.
% Use the greater of this call and the accumulator max.
% If it's zero, send the first character through to the result, and recurse the rest.
% If it's non-zero, send an X to the result and recurse the rest with max - 1.

transform_line(Line) -> transform_line(Line, 0, "").

transform_line("", _, Result) -> lists:reverse(Result);
transform_line([Head|Tail], MaskCount, Result) -> 
    transform_line([Head|Tail], MaskCount, Result, is_digit(Head)).

transform_line(List, MaskCount, Result, true) ->
    transform_line_from_digit(List, MaskCount, Result);
transform_line([Head|Tail], MaskCount, Result, false) ->
    transform_line(Tail, MaskCount, [Head|Result]).

transform_line_from_digit([Digit|Rest], MaskCount, Result) ->
    {NewMaskCount, ResultChar} = get_new_mask_and_result_char([Digit|Rest], MaskCount),
    transform_line(Rest, NewMaskCount, [ResultChar|Result]).

get_new_mask_and_result_char([Char|Rest], MaskCount) ->
    max_if_nonzero(max(max_luhn_length([Char|Rest]), MaskCount), Char).

max_if_nonzero(0, Char) -> {0, Char};
max_if_nonzero(ThisMaskCount, _) -> {ThisMaskCount - 1, $X}.

max_luhn_length(String) ->
    first_or_zero(lists:filter(fun(X) -> is_luhn(String, X) end, reverse_range())).

first_or_zero([]) -> 0;
first_or_zero([Max|_]) -> Max.

reverse_range() -> lists:reverse(lists:seq(?MinLength, ?MaxLength)).

is_luhn(String, DigitCount) -> is_luhn(find_digits(String, DigitCount)).

is_luhn(String) -> ok_and_divisible(string_digit_sum(String)).

ok_and_divisible({ok, Sum}) -> Sum rem 10 =:= 0;
ok_and_divisible(_) -> false.

string_digit_sum(String) -> sum_digit_list(lists:filter(fun is_digit/1, String)).

sum_digit_list("") -> no_digits;
sum_digit_list(Digits) ->
    Values = lists:map(fun(D) -> D - $0 end, Digits),
    {Sum, _} = lists:foldr(fun digit_sum_fold/2, {0, false}, Values),
    {ok, Sum}.

digit_sum_fold(Elem, {Sum, Double}) -> {Sum + digit_sum(Elem, Double), not(Double)}.

digit_sum(X, true) -> (X * 2 - 1) rem 9 + 1;
digit_sum(X, false) -> X.

find_digits(Line, Count) -> find_digits(Line, Count, "").

find_digits(_, 0, Result) -> lists:reverse(Result);
find_digits("", _, _) -> "";
find_digits([Head|Tail], Count, Result) -> find_digits([Head|Tail], Count, Result, is_digit(Head)).

find_digits([Head|Tail], Count, Result, false) ->
    find_digits(Tail, Count, [Head|Result]);
find_digits([Head|Tail], Count, Result, true) ->
    find_digits(Tail, Count - 1, [Head|Result]).
