-module(profile).
-export([main/1]).

main(_) ->
    eprof:start(),
    eprof:start_profiling([self()]),
    luhn:transform_line("aksjhdlfuqyw8374612h35172698371692312kjhksd"),
    eprof:stop_profiling(),
    eprof:analyze(total),
    eprof:stop().
