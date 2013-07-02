-module(luhn2).
-export([main/1,start/0,transform/1]).
-define(MinLength, 14).
-define(MaxLength, 16).

% Took more of a state machine kind of approach with this one,
% trying to conserve computations in pursuit of performance,
% but it runs in the same time as the first version.

main(_) -> start().

start() -> process_line(io:get_line("")).

process_line([10|_]) -> ok;

process_line(Line) ->
    io:fwrite("~s", [transform(Line)]),
    start().

eval_digit(Char) when Char < $0 ->
    Char;
eval_digit(Char) when Char > $9 ->
    Char;
eval_digit(Digit) -> 
    {Digit, Digit - $0}.

transform(Line) ->
    Reverse = lists:reverse(lists:map(fun eval_digit/1, Line)),
    find_min_digits({Reverse, Reverse, [], [], 0, 0, 0, false, 0}).

% non-digit
find_min_digits({Start, [Head|Tail], Buffer, Result, DigitCount, ThisSum, OtherSum, Double, MaskCount})
    when is_integer(Head) ->
        find_min_digits({
                Start, Tail, [Head|Buffer], Result, DigitCount, ThisSum, OtherSum, Double, MaskCount
            });
% found min
find_min_digits({Start, End, Buffer, Result, ?MinLength, ThisSum, OtherSum, Double, MaskCount}) ->
    check_sum({Start, End, Buffer, Result, ?MinLength, ThisSum, OtherSum, Double, MaskCount});
% no characters
find_min_digits({Start, [], Buffer, Result, DigitCount, ThisSum, OtherSum, Double, MaskCount}) ->
    next_start({Start, [], Buffer, Result, DigitCount, ThisSum, OtherSum, Double, MaskCount});
% digit less than min
find_min_digits(
    {Start, [{Digit, Value}|Tail], Buffer, Result, DigitCount, ThisSum, OtherSum, Double, MaskCount}
) ->
    find_min_digits({
        Start,
        Tail,
        [{Digit, Value}|Buffer],
        Result,
        DigitCount + 1,
        ThisSum + maybe_double_digit_sum(Double, Value),
        OtherSum + maybe_double_digit_sum(not(Double), Value),
        not(Double),
        MaskCount
    }).

check_sum({Start, End, Buffer, Result, DigitCount, ThisSum, OtherSum, Double, MaskCount}) ->
    next_end({
        Start, 
        End,
        Buffer, 
        Result, 
        DigitCount, 
        ThisSum,
        OtherSum,
        Double, 
        new_mask_count(MaskCount, DigitCount, (ThisSum rem 10) =:= 0)
    }).

% no more characters
next_end({Start, [], Buffer, Result, DigitCount, ThisSum, OtherSum, Double, MaskCount}) ->
    next_start({Start, [], Buffer, Result, DigitCount, ThisSum, OtherSum, Double, MaskCount});
% already at max
next_end({Start, End, Buffer, Result, ?MaxLength, ThisSum, OtherSum, Double, MaskCount}) ->
    next_start({Start, End, Buffer, Result, ?MaxLength, ThisSum, OtherSum, Double, MaskCount});
% non-digit
next_end({Start, [Head|Tail], Buffer, Result, DigitCount, ThisSum, OtherSum, Double, MaskCount})
when is_integer(Head) ->
    next_end({Start, Tail, [Head|Buffer], Result, DigitCount, ThisSum, OtherSum, Double, MaskCount});
% digit
next_end({Start, [{Digit, Value}|Tail], Buffer, Result, DigitCount, ThisSum, OtherSum, Double, MaskCount}) ->
    check_sum({
        Start, 
        Tail, 
        [{Digit, Value}|Buffer], 
        Result, 
        DigitCount + 1, 
        ThisSum + maybe_double_digit_sum(Double, Value), 
        OtherSum + maybe_double_digit_sum(not(Double), Value), 
        not(Double), 
        MaskCount
    }).


% no more characters
next_start({[], _End, _Buffer, Result, _DigitCount, _ThisSum, _OtherSum, _Double, _MaskCount}) ->
    Result;
% non-digit
next_start({[Head|Tail], End, Buffer, Result, DigitCount, ThisSum, OtherSum, Double, MaskCount})
when is_integer(Head) ->
    next_start({Tail, End, Buffer, [Head|Result], DigitCount, ThisSum, OtherSum, Double, MaskCount});
% not enough digits to check sum
next_start({[{Digit, Value}|Tail], End, Buffer, Result, DigitCount, ThisSum, OtherSum, _Double, MaskCount})
when DigitCount =< ?MinLength ->
    next_start({
        Tail, 
        End, 
        Buffer, 
        [maybe_mask(MaskCount > 0, Digit)|Result], 
        DigitCount - 1, 
        OtherSum - maybe_double_digit_sum(false, Value),
        ThisSum - maybe_double_digit_sum(true, Value), 
        false, 
        MaskCount - 1
    });
% digit
next_start({[{Digit, Value}|Tail], End, Buffer, Result, DigitCount, ThisSum, OtherSum, Double, MaskCount}) ->
    down_to_min({
        Tail, 
        End, 
        Buffer, 
        [maybe_mask(MaskCount > 0, Digit)|Result], 
        DigitCount - 1, 
        OtherSum - maybe_double_digit_sum(true, Value), 
        ThisSum - maybe_double_digit_sum(false, Value), 
        not(Double), 
        MaskCount - 1
    }).

maybe_mask(false, Digit) ->
    Digit;
maybe_mask(true, _) ->
    $X.

% non-digit
down_to_min({List, End, [Head|Tail], Result, DigitCount, ThisSum, OtherSum, Double, MaskCount})
when is_integer(Head) ->
    down_to_min({List, [Head|End], Tail, Result, DigitCount, ThisSum, OtherSum, Double, MaskCount});
% found min
down_to_min({List, End, Buffer, Result, ?MinLength, ThisSum, OtherSum, Double, MaskCount}) ->
    check_sum({List, End, Buffer, Result, ?MinLength, ThisSum, OtherSum, Double, MaskCount});
% digit
down_to_min({List, End, [{Digit, Value}|Tail], Result, DigitCount, ThisSum, OtherSum, Double, MaskCount}) ->
    down_to_min({
        List, [{Digit, Value}|End], Tail, Result, DigitCount - 1, 
        ThisSum - maybe_double_digit_sum(not(Double), Value), 
        OtherSum - maybe_double_digit_sum(Double, Value), 
        not(Double), 
        MaskCount
    }).

new_mask_count(MaskCount, _, false) ->
    MaskCount;
new_mask_count(MaskCount, PossibleNewMaskCount, true) ->
    max(MaskCount, PossibleNewMaskCount).

maybe_double_digit_sum(false, Value) -> Value;
maybe_double_digit_sum(true, Value) ->
   Double = 2 * Value,
   (Double div 10) + (Double rem 10).
