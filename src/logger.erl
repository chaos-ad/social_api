-module(logger).

-export([format/5]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

format(Module, Lvl, Process, Pattern, Args) ->
    Time = format_time(erlang:now()),
    Level = string:to_upper(atom_to_list(Lvl)),
    Message = lists:flatten( io_lib:format(Pattern, Args) ),
    io:format("~s ~-5s [~p][~p]~s~n", [Time, Level, Process, Module, Message]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

format_time({{Year,Month,Day},{Hour,Min,Sec}}) ->
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
        [Year, Month, Day, Hour, Min, Sec]);

format_time(Now) ->
    {_, _, Micro} = Now,
    {{Year,Month,Day},{Hour,Min,Sec}} = calendar:now_to_local_time(Now),
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B:~6.10.0B",
        [Year, Month, Day, Hour, Min, Sec, Micro]).

