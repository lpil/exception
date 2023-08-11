-module(exception_ffi).

-export([rescue/1, defer/2]).

rescue(F) ->
    try {ok, F()}
    catch Tag:Term -> {error, {Tag, Term}}
    end.

defer(Cleanup, Body) ->
    try Body()
    after Cleanup()
    end.
