
%% @doc Web server for ebank.

-module(ebank_web).
-author("Mochi Media <dev@mochimedia.com>").
-define(TEST, true).

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
               Accounts = mochiglobal:get(accounts),
               case Accounts of 
                  undefined ->
                     Body = "\n\tList of accounts is empty!";
                  _ ->
                     Account = lists:filter(fun(X) -> list_to_integer(element(2, X)) =:= Idint end,  Accounts),
                     case length(Account) of 
                        0 ->
                           Body =  "\n\tAccount not found!";
                        _ -> 
                           io:format("Account: ~p", [Account]),  
                           Body = "{\n\t\"Name\":" ++ io_lib:format("~p", [element(2, element(3,(hd(Account))))]) ++ ",\n" ++  "\t\"Balance\":" 
                                                   ++ io_lib:format("~p", [element(3, element(3,(hd(Account))))]) ++ "\n}"
                     end
               end,
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
      			[Balance, Id, Name, Pin] = lists:map(fun(X) -> proplists:get_value(X, QueryData) end, QueryKeys),
      	   	Adet = #accountDetails{name=Name, balance=Balance, pin=Pin},
               Account = #account{id=Id, details=Adet},
			      Accounts = mochiglobal:get(accounts),
               case Accounts of
                  undefined  -> 
                     NewAccounts = [Account],
                     mochiglobal:put(accounts, NewAccounts),
                     % Body = "{\n\t\"Name\":\"" ++ Adet#accountDetails.name ++ "\",\n" ++  "\t\"Balance\":\"" ++ Adet#accountDetails.balance ++ "\"\n}";
                     Body = generate_body(Account);
                  _ -> 
                     % SearchAccount = lists:filter(fun(X) -> X#account.id =:= Id end,  Accounts),
                     SearchAccount = search_account_byId(Id, Accounts),
                     
                     case length(SearchAccount) of
                        0 ->
                           NewAccounts = [Account|Accounts],
			                  mochiglobal:put(accounts, NewAccounts),
                           % Body = "{\n\t\"Name\":\"" ++ Adet#accountDetails.name ++ "\",\n" ++  "\t\"Balance\":\"" ++ Adet#accountDetails.balance ++ "\"\n}";
                           Body = generate_body(Account);
                        _ ->
                           Body = "\n\t An account with this id already exists!"
                     end
               end,
               Req:respond({200, [{"Content-Type", "text/plain"}], Body});
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
      Req:respond({500, [{"Content-Type", "text/plain"}], "request failed, sorry\n"})
   end. 

%% Internal API
get_option(Option, Options) ->
   {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

generate_body(#account{id=Id, details=#accountDetails{name = Name, balance = Balance,  pin = Pin}}) ->
   Body = "{\n\t\"Name\":\"" ++ Name ++ "\",\n" ++  "\t\"Balance\":\"" ++ Balance ++ "\"\n}".

search_account_byId(Id, Accounts) -> 
   SearchAccount = lists:filter(fun(X) -> X#account.id =:= Id end, Accounts).

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

generate_body_test_() ->
Account = #account{id = 1, details=#accountDetails{name="account1", balance="1000", pin=12345}},
Body = "{\n\t\"Name\":" ++ "\"account1\"" ++ ",\n" ++  "\t\"Balance\":" ++ "\"1000\"" ++ "\n}",
?_assertEqual(Body, generate_body(Account)).

searc_account_byId_test_() ->
Account1 = #account{id = 1, details=#accountDetails{name="account1", balance="1000", pin=12345}},
Account2 = #account{id = 2, details=#accountDetails{name="account2", balance="200", pin=12}},
Accounts = [Account1, Account2],
Id = 1,
?_assertEqual([Account1], search_account_byId(Id, Accounts)).

-endif.
