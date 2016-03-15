#!/usr/bin/env escript

main([Filename]) ->
    try
        ok = envx_lib_application:check_config(Filename)
    catch
        _ExcType:{Line, erl_parse, Message} ->
            ok = io:format(
                   "~nError in Erlang config file '~s'. Line ~w: ~s~n",
                   [Filename, Line, Message]),
            halt(1);
        _ExcType:ExcReason ->
            ok = io:format(
                   "~nError in config file '~s':~n\t~p~n",
                   [Filename, ExcReason]),
            halt(1)
    end.
