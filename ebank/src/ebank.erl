%% @doc ebank.

-module(ebank).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the ebank server.
start() ->
    ebank_deps:ensure(),
    ensure_started(crypto),
    application:start(ebank).


%% @spec stop() -> ok
%% @doc Stop the ebank server.
stop() ->
    application:stop(ebank).
