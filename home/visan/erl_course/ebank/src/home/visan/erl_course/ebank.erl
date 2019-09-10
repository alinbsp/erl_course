%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc /home/visan/erl_course/ebank.

-module(/home/visan/erl_course/ebank).
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
%% @doc Start the /home/visan/erl_course/ebank server.
start() ->
    /home/visan/erl_course/ebank_deps:ensure(),
    ensure_started(crypto),
    application:start(/home/visan/erl_course/ebank).


%% @spec stop() -> ok
%% @doc Stop the /home/visan/erl_course/ebank server.
stop() ->
    application:stop(/home/visan/erl_course/ebank).
