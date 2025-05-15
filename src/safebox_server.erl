-module(safebox_server).
-export([start/0, accept/1]).

-define(TABLE, safebox_storage).
-define(PORT, 5000).

start() ->
    {ok, ListenSock} = gen_tcp:listen(?PORT, [
        binary, {packet, line}, {active, false}, {reuseaddr, true},
        {ip, {0,0,0,0}}
    ]),
    io:format("SafeBox TCP Server en écoute sur 0.0.0.0:~p~n", [?PORT]),
    init_storage(),
    accept(ListenSock).

accept(ListenSock) ->
    {ok, Socket} = gen_tcp:accept(ListenSock),
    spawn(fun() -> handle_client(Socket) end),
    accept(ListenSock).

handle_client(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Line} ->
            Command = binary_to_list(Line),
            Response = handle_command(string:tokens(string:trim(Command), " ")),
            gen_tcp:send(Socket, list_to_binary(Response ++ "\n")),
            handle_client(Socket); % loop
        {error, closed} ->
            gen_tcp:close(Socket)
    end.

handle_command(["store", Key, EncVal]) ->
    ets:insert(?TABLE, {list_to_binary(Key), list_to_binary(EncVal)}),
    "OK: stored";

handle_command(["get", Key]) ->
    case ets:lookup(?TABLE, list_to_binary(Key)) of
        [{_, Val}] -> "OK: " ++ binary_to_list(Val);
        [] -> "ERR: not_found"
    end;

handle_command(["del", Key]) ->
    ets:delete(?TABLE, list_to_binary(Key)),
    "OK: deleted";

handle_command(_) ->
    "ERR: invalid_command".

init_storage() ->
    case ets:info(?TABLE) of
        undefined -> ets:new(?TABLE, [named_table, public, set]);
        _ -> ok
    end.
