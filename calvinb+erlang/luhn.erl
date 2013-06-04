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
    {NewMaskCount, ResultChar} = 
        get_new_mask_count_and_result_char([{Digit, Value}|Tail], MaskCount),
    transform_line(Tail, NewMaskCount, [ResultChar|Result]);
transform_line([Head|Tail], MaskCount, Result) -> 
    transform_line(Tail, MaskCount, [Head|Result]).

get_new_mask_count_and_result_char([{Digit, Value}|Tail], MaskCount) ->
    case possible_new_mask_count([{Digit, Value}|Tail], MaskCount) of
        0 -> {0, Digit};
        NewMaskCount -> {NewMaskCount - 1, $X}
    end.

possible_new_mask_count(List, MaskCount) ->
    case find_digit_values(List) of
        {_, 0} -> 
            MaskCount;
        {Values, ValueCount} ->
            max_if_div_by_10(Values, ValueCount, MaskCount)
    end.

max_if_div_by_10(_, 0, MaskCount) ->
    MaskCount;
max_if_div_by_10(Values, ValueCount, MaskCount) ->
    Sum = digit_sum(Values),
    case Sum rem 10 of
        0 -> max(ValueCount, MaskCount);
        _ -> MaskCount
    end.

digit_sum_fold(Elem, {Sum, Double}) -> 
    {Sum + digit_sum(Elem, Double), not(Double)}.

digit_sum(List) ->
    {Result, _} = lists:foldl(fun digit_sum_fold/2, {0, false}, List),
    Result.

digit_sum(X, false) -> 
    X;
digit_sum(X, true) -> 
    Y = X * 2,
    (Y div 10) + (Y rem 10).

find_digit_values(List) -> find_digit_values(List, {[], 0}).

find_digit_values(_, {Values, ?MaxLength}) -> 
    {Values, ?MaxLength};
find_digit_values([], {_, FoundCount}) when FoundCount < ?MinLength ->
    {[], 0};
find_digit_values([], {Values, FoundCount}) ->
    {Values, FoundCount};
find_digit_values([{_, Value}|Tail], {Values, FoundCount}) -> 
    find_digit_values(Tail, {[Value|Values], FoundCount + 1});
find_digit_values([_|Tail], {Values, FoundCount}) -> 
    find_digit_values(Tail, {Values, FoundCount}).
