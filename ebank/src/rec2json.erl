-module(rec2json).
%% -export([to_json/1, from_record/1]).
-compile([debug_info, export_all]).

-define(JSON_WRAPPER(Proplist), {Proplist}).

-record(account_info, {name, balance, pin}).
-record(transaction, {id, date, value}).
-record(account, {id, details=account_info#{}}).

% -spec to_binary(atom() | string() | binary() | integer() | float() | pid() | iolist()) -> binary().
% to_binary(X) when is_float(X) -> to_binary(mochinum:digits(X));
% to_binary(X) when is_integer(X) -> list_to_binary(integer_to_list(X));
% to_binary(X) when is_atom(X) -> list_to_binary(atom_to_list(X));
% to_binary(X) when is_list(X) -> iolist_to_binary(X);
% to_binary(X) when is_pid(X) -> to_binary(pid_to_list(X));
% to_binary(X) when is_binary(X) -> X.

% Account1 = #account{id=1,details=#account_info{name=account1,balance=1234,pin=100}},
% [account,1,#account_info{name = account1,balance = 1234,pin = 100}]


to_json(Account) ->
	AccountList = tuple_to_list(Account),
	erlang:display(hd(AccountList)).


%% @spec from_record(any()) -> string().
% from_record([], _) ->
% 	[];
% from_record(Rec, Acc) when is_tuple(Rec) ->
% 	lists:zip(record_info(fields, account), tl(tuple_to_list(Rec)));

% from_record([_|T]) ->
% 	from_record(T).
%  


to_proplist(Record) ->
	to_proplist(Record, []).

to_proplist(Type = #account{}, []) ->
	lists:zip(record_info(fields, account), to_list(Type));
to_proplist(Type = #account_info{}, []) ->
	lists:zip(record_info(fields, account_info), to_list(Type));


to_proplist(Val, []) ->
	Val;
to_proplist([], Result) ->
	lists:reverse(Result);
to_proplist([H|T], Result) ->
	to_proplist(T, [to_proplist(H,[]) | Result]).

to_list(Type) ->
	[to_proplist(L, []) || L <- tl(tuple_to_list(Type))].

proplist2json(Proplist) ->
	proplist2json(Proplist, "{").

proplist2json([], Acc) ->
	Acc ++ "}";
proplist2json([{K,V}|T], Acc) when is_list(V) ->
	proplist2json(T, Acc ++ "\"" ++ lists:flatten(io_lib:format("~p", [K])) ++ ": [" ++ proplist2json(V, "") ++ "]");
proplist2json([{K,V}|T], Acc) ->
	proplist2json(T, Acc ++ "\"" ++ lists:flatten(io_lib:format("~p", [K])) ++ ":" ++ "\"" ++ lists:flatten(io_lib:format("~p", [V])) ++ "\",").

% %% @spec from_list(json_proplist()) -> object().
% from_list([]) -> true; % new();
% from_list(L) when is_list(L) -> ?JSON_WRAPPER(L).

msg2json(msg, Msg) ->
	"{\n\t\t\"Happy\" : \"" ++ Msg ++ "\"\n}";

msg2json(error, Msg) ->
	"{\n\t\t\"error\" : \"" ++ Msg ++ "\"\n}";

msg2json(Key, Value) when is_list(Key), is_list(Value) ->
	T = lists:zipwith(fun(X, Y) -> "\t\t\"" ++ X ++ " : \"" ++ Y ++ "\",\n" end, Key, Value),
	Last = lists:last(T),
	NewElem = lists:droplast(lists:droplast(Last)),
	"{\n" ++ lists:droplast(T) ++ NewElem ++ "\n}";

msg2json(Key, Value) ->
	"{\n\t\t\"" ++ Key ++ "\" : \"" ++ Value ++ "\"\n}".

json2proplist(JSON) ->
	io:format("~p", JSON).

%  
% %% @spec recursive_from_proplist(any()) -> object().
% recursive_from_proplist([]) -> true; % new();
% recursive_from_proplist(List) when is_list(List) ->
%         case lists:all(fun is_integer/1, List) of
%             'true' -> List;
%             'false' ->
%                 from_list([{to_binary(K) ,recursive_from_proplist(V)}
%                                    || {K,V} <- List
%                                   ])
%         end;
%     recursive_from_proplist(Other) -> Other.
