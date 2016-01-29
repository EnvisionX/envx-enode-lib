#!/usr/bin/env escript

main([Filename]) ->
    try
        ok = envx_lib_application:check_config(Filename)
    catch
        _ExcType:ExcReason ->
            ok = io:format(
                   "~nError in config file '~s':~n\t~p~n",
                   [Filename, ExcReason]),
            halt(1)
    end.
