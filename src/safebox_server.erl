-module(safebox_server).
-export([start/0, accept/1]).

-record(user, {login, password}).
-record(secret, {user, key, value}).

-define(PORT, 5000).
-define(USER_TABLE, user).
-define(SECRET_TABLE, secret).

start() ->
    setup_mnesia(),
    {ok, ListenSock} = gen_tcp:listen(?PORT, [
        binary, {packet, line}, {active, false}, {reuseaddr, true},
        {ip, {0,0,0,0}}
    ]),
    io:format("SafeBox TCP Server (multi-utilisateur) sur 0.0.0.0:~p~n", [?PORT]),
    accept(ListenSock).

accept(ListenSock) ->
    {ok, Socket} = gen_tcp:accept(ListenSock),
    spawn(fun() -> handle_client(Socket, undefined) end),
    accept(ListenSock).

handle_client(Socket, Login) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Line} ->
            Command = binary_to_list(Line),
            Tokens = string:tokens(string:trim(Command), " "),
            {NextLogin, Response} = handle_command(Tokens, Login),
            gen_tcp:send(Socket, list_to_binary(Response ++ "\n")),
            handle_client(Socket, NextLogin);
        {error, closed} ->
            gen_tcp:close(Socket)
    end.

handle_command(["register", Login], _) ->
    io:format("Création utilisateur: ~s~n", [Login]),
    io:format("Saisir le mot de passe (non sécurisé dans cette version) :~n"),
    Password = io:get_line(""),
    Fun = fun() ->
        mnesia:write(#user{login = list_to_binary(Login), password = list_to_binary(string:trim(Password))})
    end,
    case mnesia:transaction(Fun) of
        {atomic, _} -> {undefined, "OK: user_created"};
        _ -> {undefined, "ERR: user_creation_failed"}
    end;

handle_command(["login", Login], _) ->
    io:format("Mot de passe : "),
    Password = string:trim(io:get_line("")),
    PasswordBin = list_to_binary(Password),
    Fun = fun() ->
        case mnesia:read({?USER_TABLE, list_to_binary(Login)}) of
            [#user{password = Pw}] ->
                case Pw =:= PasswordBin of
                    true -> true;
                    false -> false
                end;
            _ -> false
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, true} -> {list_to_binary(Login), "OK: login_successful"};
        _ -> {undefined, "ERR: invalid_credentials"}
    end;

handle_command(["store", Key, EncVal], Login) when is_binary(Login) ->
    Fun = fun() ->
        mnesia:write(#secret{user = Login, key = list_to_binary(Key), value = list_to_binary(EncVal)})
    end,
    case mnesia:transaction(Fun) of
        {atomic, _} -> {Login, "OK: stored"};
        _ -> {Login, "ERR: store_failed"}
    end;

handle_command(["get", Key], Login) when is_binary(Login) ->
    Fun = fun() ->
        MatchHead = #secret{user = Login, key = list_to_binary(Key), value = '_'},
        case mnesia:match_object(MatchHead) of
            [#secret{value = Val}] -> "OK: " ++ binary_to_list(Val);
            [] -> "ERR: not_found"
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Resp} -> {Login, Resp};
        _ -> {Login, "ERR: get_failed"}
    end;

handle_command(["del", Key], Login) when is_binary(Login) ->
    Fun = fun() ->
        mnesia:delete_object(#secret{user = Login, key = list_to_binary(Key), value = '_'})
    end,
    case mnesia:transaction(Fun) of
        {atomic, _} -> {Login, "OK: deleted"};
        _ -> {Login, "ERR: delete_failed"}
    end;

handle_command(_, Login) ->
    {Login, "ERR: invalid_command"}.

setup_mnesia() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    ensure_table(?USER_TABLE, record_info(fields, user)),
    ensure_table(?SECRET_TABLE, record_info(fields, secret)).

ensure_table(Name, Fields) ->
    case lists:member(Name, mnesia:system_info(tables)) of
        false ->
            mnesia:create_table(Name, [
                {attributes, Fields},
                {disc_copies, [node()]},
                {type, set}
            ]);
        true -> ok
    end.