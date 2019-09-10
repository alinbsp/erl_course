
%% @doc Web server for ebank.

-module(ebank_web).
-author("Mochi Media <dev@mochimedia.com>").
-define(TESTING, true).

-compile(tuple_calls).
-include_lib("kernel/include/logger.hrl").

-export([start/1, stop/0, loop/2, generate_body/1]).

%% External API


-record(accountDetails, {name, balance, pin}).
-record(account, {id, details=accountDetails#{}}).
-record(test, {id, accountDetails=accountDetails#{}}).

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

handle_get("hello_world", Req, _) ->
   error_logger:info_msg("[GET]: Hello World"),
   Body = rec2json:msg2json(msg, "Hello World!"),
   Req:respond({200, [{"Content-Type", "text/plain"}], Body});

handle_get("getBalance", Req, _) ->
   error_logger:info_msg("[GET]: get balance"),
   [Name, Pin] = get_params(Req),
   Accounts = mochiglobal:get(accounts),
   Body = case Accounts of
      undefined ->
         rec2json:msg2json(error, "List of accounts is empty!");
      _ ->
         ListofAccountsrecord = convert_tuples_to_records(Accounts),
         SearchAccountforBalance = get_balance(Name, Pin, ListofAccountsrecord),
         generate_body(SearchAccountforBalance)
      end,
   Req:respond({200, [{"Content-Type", "text/plain"}], Body});

handle_get("check", Req, _) ->
   error_logger:info_msg("[GET]: check"),
   Id = get_params(Req),
   Accounts = mochiglobal:get(accounts),
   Body = case Accounts of
      undefined ->
         rec2json:msg2json(error, "User not found!");
      _ ->
         ListofAccountsrecord = convert_tuples_to_records(Accounts),
         IsAccount = lists:map(fun(X) ->  io:format("Account record format: ~p\n", [X#account.details#accountDetails.name])end, ListofAccountsrecord),
         Account = search_account_byId(hd(Id), ListofAccountsrecord),
         generate_body(Account)
   end,
   Req:respond({200, [{"Content-Type", "text/plain"}], Body});

handle_get(Path, Req, DocRoot) ->
   error_logger:info_msg("[GET]: Ohter"),
   Req:serve_file(Path, DocRoot).

handle_post("create", Req, _) ->
   error_logger:info_msg("[POST]: Create"),
   [Balance, Id, Name, Pin] = get_params(Req),
   Account = #account{id=Id, details=#accountDetails{name=Name, balance=Balance, pin=Pin}},
   Accounts = mochiglobal:get(accounts),
   Body = create_account(Accounts, Account),
   Req:respond({200, [{"Content-Type", "text/plain"}], Body});

handle_post(_Path, Req, _DocRoot) ->
   error_logger:info_msg("[POST]: Other"),
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


%% -----------------------------------------------------------------------------------------------------------------

%% Internal API
get_option(Option, Options) ->
   {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

get_params(Req) ->
   QueryData = Req:parse_qs(),
   QueryKeys = lists:sort(proplists:get_keys(QueryData)),
   lists:map(fun(X) -> proplists:get_value(X, QueryData) end, QueryKeys).

create_account(undefined, Account) ->
   NewAccounts = [Account],
   mochiglobal:put(accounts, NewAccounts),
   generate_body(Account);

create_account(Accounts, Account=#account{id=Id, details=_Details}) ->
   SearchAccount = search_account_byId(Id, Accounts),
   case length(SearchAccount) of
   0 ->
      NewAccounts = [Account|Accounts],
      mochiglobal:put(accounts, NewAccounts),
      generate_body(Account);
   _ ->
      rec2json:msg2json(error, "An account with this id already exists!")
   end.

%% -----------------------------------------------------------------------------------------------------------------
generate_body(undefined) ->
   ok;
generate_body([]) ->
   rec2json:msg2json(error, "Account not found!");
generate_body(#account{id = _Id, details=#accountDetails{name = Name, balance = Balance,  pin = _Pin}}) ->
   rec2json:msg2json(["Name", "Balance"], [Name, Balance]);
generate_body(Accounts) when is_list(Accounts) ->
   generate_body(hd(Accounts)).

%% -----------------------------------------------------------------------------------------------------------------

search_account_byId(Id, Accounts) ->
   lists:filter(fun(X) -> X#account.id =:= Id end, Accounts).

convert_tuples_to_records(Accounts) ->
   lists:map(fun(X) -> #account{id = element(2,X), details =
      #accountDetails{name=element(2,element(3,X)), balance =element(3,element(3,X)), pin = element(4,element(3,X))}} end, Accounts).

get_balance(Name, Pin, Accounts) ->
   lists:filter(fun(X) -> X#account.details#accountDetails.name =:= Name andalso X#account.details#accountDetails.pin =:= Pin end, Accounts).


parse_body(Data) when is_binary(Data) ->
   Json = binary_to_list(Data),
   Strip_with = "\n\t",
   To_stripp = Json,
   lists:filter( fun(C) -> not lists:member(C, Strip_with) end, To_stripp ).

%%
%% Tests
%%
-ifdef(TESTING).
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
