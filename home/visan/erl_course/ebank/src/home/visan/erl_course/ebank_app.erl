%% @author Mochi Media <dev@mochimedia.com>
%% @copyright /home/visan/erl_course/ebank Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the /home/visan/erl_course/ebank application.

-module(/home/visan/erl_course/ebank_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for /home/visan/erl_course/ebank.
start(_Type, _StartArgs) ->
    /home/visan/erl_course/ebank_deps:ensure(),
    /home/visan/erl_course/ebank_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for /home/visan/erl_course/ebank.
stop(_State) ->
    ok.
