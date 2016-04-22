-module(remsh_evaluator).

-export([do/1]).

-spec do([atom(), ...]) -> no_return().
do([Node, ErlangCode0]) ->
    try
        ErlangCode = atom_to_list(ErlangCode0),
        {ok, Tokens0, _EndLocation} = erl_scan:string(ErlangCode),
        Tokens =
            case lists:reverse(Tokens0) of
                [{dot, 1} | _] ->
                    Tokens0;
                WithoutFinalDot ->
                    lists:reverse([{dot, 1} | WithoutFinalDot])
            end,
        {ok, Expressions} = erl_parse:parse_exprs(Tokens),
        {value, Value, _NewBindings} =
            rpc:call(Node,
                     erl_eval, exprs, [Expressions, _Bindings = []]),
        ok = io:format("~p~n", [Value]),
        halt(0)
    catch
        ExcType:ExcReason ->
            io:format(
              "CRASHED.~n"
              "  type=~p~n"
              "  reason=~p~n"
              "  stacktrace=~p~n",
              [ExcType, ExcReason, erlang:get_stacktrace()]),
            halt(1)
    end.
