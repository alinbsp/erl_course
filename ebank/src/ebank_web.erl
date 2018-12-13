
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

handle_get("hello_world", Req, _) ->
   Req:respond({200, [{"Content-Type", "text/plain"}], "Hello world!\n"});

handle_get("getBalance", Req, _) ->
   QueryData = Req:parse_qs(),
   QueryKeys = lists:sort(proplists:get_keys(QueryData)),
   [Name, Pin] = lists:map(fun(X) -> proplists:get_value(X, QueryData) end, QueryKeys),
   Accounts = mochiglobal:get(accounts),
   case Accounts of
      undefined ->
         Body = "\n\tList of accounts is empty!";
      _ ->
         ListofAccountsrecord = convert_tuples_to_records(Accounts),
         SearchAccountforBalance = get_balance(Name, Pin, ListofAccountsrecord),
         case length(SearchAccountforBalance) of
            0 ->
               Body = "\n\tAccount not found!";
            _ ->
               Body = generate_body(hd(SearchAccountforBalance))
            end
      end,
   Req:respond({200, [{"Content-Type", "text/plain"}], Body});

handle_get("check", Req, _) ->
   QueryData = Req:parse_qs(),
   QueryKeys = proplists:get_keys(QueryData),
   Id = lists:map(fun(X) -> proplists:get_value(X, QueryData) end, QueryKeys),
   Accounts = mochiglobal:get(accounts),
   case Accounts of
      undefined ->
         Body = "\n\tList of accounts is empty!";
      _ ->
         ListofAccountsrecord = convert_tuples_to_records(Accounts),
         IsAccount = lists:map(fun(X) ->  io:format("Account record format: ~p", [X#account.details#accountDetails.name])end, ListofAccountsrecord),
         Account = search_account_byId(hd(Id), ListofAccountsrecord),
         case length(Account) of
            0 ->
               Body =  "\n\tAccount not found!";
            _ ->
               Body = generate_body(hd(Account))
         end
   end,
   Req:respond({200, [{"Content-Type", "text/plain"}], Body});

handle_get(Path, Req, DocRoot) ->
   Req:serve_file(Path, DocRoot).

handle_post("create", Req, _) ->
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
         Body = generate_body(Account);
      _ ->
         SearchAccount = search_account_byId(Id, Accounts),

         case length(SearchAccount) of
            0 ->
               NewAccounts = [Account|Accounts],
               mochiglobal:put(accounts, NewAccounts),
               Body = generate_body(Account);
            _ ->
               Body = "\n\t An account with this id already exists!"
         end
   end,
   Req:respond({200, [{"Content-Type", "text/plain"}], Body});

handle_post(_Path, Req, _DocRoot) ->
   Req:not_found().

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    try
      case Req:get(method) of
         Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            handle_get(Path, Req, DocRoot);
         'POST' ->
            handle_post(Path, Req, DocRoot);
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

generate_body(#account{id = _Id, details=#accountDetails{name = Name, balance = Balance,  pin = _Pin}}) ->
   "{\n\t\"Name\":\"" ++ Name ++ "\",\n" ++  "\t\"Balance\":\"" ++ Balance ++ "\"\n}".

search_account_byId(Id, Accounts) ->
   lists:filter(fun(X) -> X#account.id =:= Id end, Accounts).

convert_tuples_to_records(Accounts) ->
   lists:map(fun(X) -> #account{id = element(2,X), details =
      #accountDetails{name=element(2,element(3,X)), balance =element(3,element(3,X)), pin = element(4,element(3,X))}} end, Accounts).

get_balance(Name, Pin, Accounts) ->
   lists:filter(fun(X) -> X#account.details#accountDetails.name =:= Name andalso X#account.details#accountDetails.pin =:= Pin end, Accounts).

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

convert_tuples_to_records_test_() ->
Account1 = #account{id = 1, details=#accountDetails{name="account1", balance="1000", pin=12345}},
Account2 = #account{id = 2, details=#accountDetails{name="account2", balance="200", pin=12}},
Accounts = [Account1, Account2],
Accounttuple1 = {"account", 1, {"accountDetails", "account1", "1000", 12345}},
Accounttuple2 = {"account", 2, {"accountDetails", "account2", "200", 12}},
Accountstuples = [Accounttuple1, Accounttuple2],
?_assertEqual(Accounts, convert_tuples_to_records(Accountstuples)).

get_balance_test_() ->
Account1 = #account{id = 1, details=#accountDetails{name="account1", balance="1000", pin=12345}},
Account2 = #account{id = 2, details=#accountDetails{name="account2", balance="200", pin=12}},
Accounts = [Account1, Account2],
Name = "account1",
Pin = 12345,
?_assertEqual([Account1], get_balance(Name, Pin, Accounts)).

-endif.
