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

transform_line(Line) -> transform_line(eval_digits(Line), 0, "").

eval_digits(List) ->
    lists:map(fun maybe_eval_digit/1, List).

maybe_eval_digit(Char) when Char > $9 orelse Char < $0 ->
    Char;
maybe_eval_digit(Char) ->
    {Char, Char - $0}.

transform_line([], _, Result) -> lists:reverse(Result);
transform_line([{Digit, Value}|Tail], MaskCount, Result) -> 
    {NewMaskCount, ResultChar} = get_new_mask_and_result_char([{Digit, Value}|Tail], MaskCount),
    transform_line(Tail, NewMaskCount, [ResultChar|Result]);
transform_line([Head|Tail], MaskCount, Result) -> 
    transform_line(Tail, MaskCount, [Head|Result]).

get_new_mask_and_result_char([{Digit, Value}|Tail], MaskCount) ->
    max_if_nonzero(max(max_luhn_length([{Digit, Value}|Tail]), MaskCount), Digit).

max_if_nonzero(0, Char) -> {0, Char};
max_if_nonzero(ThisMaskCount, _) -> {ThisMaskCount - 1, $X}.

max_luhn_length(List) ->
    first_or_zero(lists:filter(fun(X) -> is_luhn(List, X) end, reverse_range())).

first_or_zero([]) -> 0;
first_or_zero([Max|_]) -> Max.

reverse_range() -> lists:reverse(lists:seq(?MinLength, ?MaxLength)).

is_luhn(List, DigitCount) -> is_luhn(find_digits(List, DigitCount)).

is_luhn(List) -> ok_and_divisible(string_digit_sum(List)).

ok_and_divisible({ok, Sum}) -> Sum rem 10 =:= 0;
ok_and_divisible(_) -> false.

string_digit_sum(List) -> 
    sum_digit_list(lists:filter(fun is_digit/1, List)).

is_digit({_, _}) -> true;
is_digit(_) -> false.

sum_digit_list([]) -> no_digits;
sum_digit_list(List) ->
    Values = lists:map(fun({_, Value}) -> Value end, List),
    {Sum, _} = lists:foldr(fun digit_sum_fold/2, {0, false}, Values),
    {ok, Sum}.

digit_sum_fold(Elem, {Sum, Double}) -> 
    {Sum + digit_sum(Elem, Double), not(Double)}.

digit_sum(X, true) -> (X * 2 - 1) rem 9 + 1;
digit_sum(X, false) -> X.

find_digits(List, Count) -> find_digits(List, Count, []).

find_digits(_, 0, Result) -> lists:reverse(Result);
find_digits([], _, _) -> "";
find_digits([{Digit, Value}|Tail], Count, Result) -> 
    find_digits(Tail, Count - 1, [{Digit, Value}|Result]);
find_digits([Head|Tail], Count, Result) -> 
    find_digits(Tail, Count, [Head|Result]).
