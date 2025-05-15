-module(safebox_server).
-export([start/0, accept/1]).

-record(secret, {key, value}).

-define(TABLE, secret).
-define(PORT, 5000).

start() ->
    setup_mnesia(),
    {ok, ListenSock} = gen_tcp:listen(?PORT, [
        binary, {packet, line}, {active, false}, {reuseaddr, true},
        {ip, {0,0,0,0}}
    ]),
    io:format("SafeBox TCP Server avec Mnesia en écoute sur 0.0.0.0:~p~n", [?PORT]),
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
    Fun = fun() ->
        mnesia:write(#secret{key = list_to_binary(Key), value = list_to_binary(EncVal)})
    end,
    case mnesia:transaction(Fun) of
        {atomic, _} -> "OK: stored";
        {aborted, Reason} -> "ERR: " ++ io_lib:format("~p", [Reason])
    end;

handle_command(["get", Key]) ->
    Fun = fun() ->
        case mnesia:read({?TABLE, list_to_binary(Key)}) of
            [#secret{value = Val}] -> "OK: " ++ binary_to_list(Val);
            [] -> "ERR: not_found"
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Resp} -> Resp;
        _ -> "ERR: mnesia_failure"
    end;

handle_command(["del", Key]) ->
    Fun = fun() ->
        mnesia:delete({?TABLE, list_to_binary(Key)})
    end,
    case mnesia:transaction(Fun) of
        {atomic, _} -> "OK: deleted";
        _ -> "ERR: delete_failed"
    end;

handle_command(_) ->
    "ERR: invalid_command".

setup_mnesia() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    case lists:member(?TABLE, mnesia:system_info(tables)) of
        false ->
            mnesia:create_table(?TABLE, [
                {attributes, record_info(fields, secret)},
                {disc_copies, [node()]},
                {type, set}
            ]);
        true -> ok
    end.