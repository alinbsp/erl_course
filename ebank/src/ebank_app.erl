
%% @doc Callbacks for the ebank application.

-module(ebank_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for ebank.
start(_Type, _StartArgs) ->
    ebank_deps:ensure(),
    ebank_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for ebank.
stop(_State) ->
    ok.
