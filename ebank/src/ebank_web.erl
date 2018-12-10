
%% @doc Web server for ebank.

-module(ebank_web).
-author("Mochi Media <dev@mochimedia.com>").

-compile(tuple_calls).

-export([start/1, stop/0, loop/2]).

%% External API


-record(accountDetails, {name, balance, pin}).
-record(account, {id, details=accountDetails#{}}).

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    try
        case Req:get(method) of
            Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                case Path of
                  "hello_world" ->
                    Req:respond({200, [{"Content-Type", "text/plain"}], "Hello world!\n"});
		   "getBalance" ->
	            Accounts = mochiglobal:get(accounts),
		    Req:respond({200, [{"Content-Type", "text/plain"}], "{\"Balance\": 100}\n"});
        "check" ->
        QueryData = Req:parse_qs(),
        QueryKeys = proplists:get_keys(QueryData),
        Id = lists:map(fun(X) -> proplists:get_value(X, QueryData) end, QueryKeys),
        
        Idint = list_to_integer(hd(Id)),
       
        % Account1 = #account{id=1,details=#accountDetails{name=account1,balance=1234,pin=100}},
        
        % Account2 = #account{id=2,details=#accountDetails{name=account2,balance=1234,pin=200}},
        % Accounts = [Account1, Account2],
         Accounts = mochiglobal:get(accounts),
        
        io:format("Before filter: ~p", [is_list(Accounts)]),
        % io:format("Id-ul primului account: ~p", [is_tuple(hd(Accounts))]),
        % io:format("Id-ul paramateru: ~p", [Idint]),

        Account = lists:filter(fun(X) -> list_to_integer(element(2, X)) =:= Idint end,  Accounts),
        
        io:format("++++++++~p", [Account]),
       
       
       % io:format(io_lib:format("~p", [element(2, element(3,(hd(Account))))])),
       
       
        Body = "{\n\t\"Name\":\"" ++ io_lib:format("~p", [element(2, element(3,(hd(Account))))]) ++ "\",\n" ++  "\t\"Balance\":\"" 
                ++ io_lib:format("~p", [element(3, element(3,(hd(Account))))]) ++ "\"\n}",
      Req:respond({200, [{"Content-Type", "text/plain"}], Body});


            _ ->
                        Req:serve_file(Path, DocRoot)
                end;
            'POST' ->
                case Path of
		   "create" ->
		   	QueryData = Req:parse_qs(),
			%[{id, Id}, {name, Name}, {pin, Pin}, {balance, Balance}] = QueryData,
			QueryKeys = lists:sort(proplists:get_keys(QueryData)),
      io:format("+++++++++++~p", [QueryData]),
			[Balance, Id, Name, Pin] = lists:map(fun(X) -> proplists:get_value(X, QueryData) end, QueryKeys),
      io:format("PIn: ~p", [Pin]),
      io:format("Balance: ~p", [Balance]),
      io:format("Name: ~p", [Name]),
      % io:format("++++++++++++~p", [QueryData]),
		   	Adet = #accountDetails{name=Name, balance=Balance, pin=Pin},
                   	Account = #account{id=Id, details=Adet},
			Accounts = mochiglobal:get(accounts),
      io:format("Before Case: ~p", [Accounts]),
      case Accounts of
        undefined  -> NewAccounts = [Account],
        mochiglobal:put(accounts, NewAccounts),
         % io:format("UNDEFINED:~p",[mochiglobal:get(accounts)]),
         Body = "{\"Name\":\"" ++ Adet#accountDetails.name ++ "\n\"Balance\":\"" ++ Adet#accountDetails.balance ++ "\"}",
      Req:respond({200, [{"Content-Type", "text/plain"}], Body});
        %Req:respond({200, [{"Content-Type", "text/plain"}], "undefined"});
         _ -> NewAccounts = [Account|Accounts],
			mochiglobal:put(accounts, NewAccounts),
      io:format("+++~p",[mochiglobal:get(accounts)]),
      Body = "{\"Name\":\"" ++ Adet#accountDetails.name ++ "\n\"Balance\":\"" ++ Adet#accountDetails.balance ++ "\"}",
      Req:respond({200, [{"Content-Type", "text/plain"}], Body})
    end;
                    _ ->
                        Req:not_found()
                end;
            _ ->
                Req:respond({501, [], []})
        end
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

you_should_write_a_test() ->
    ?assertEqual(
       "No, but I will!",
       "Have you written any tests?"),
    ok.

-endif.

