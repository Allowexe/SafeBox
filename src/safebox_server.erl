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
    io:format("SafeBox TCP Server (multi-utilisateur, login inclus dans chaque commande) sur 0.0.0.0:~p~n", [?PORT]),
    accept(ListenSock).

accept(ListenSock) ->
    {ok, Socket} = gen_tcp:accept(ListenSock),
    spawn(fun() -> handle_client(Socket) end),
    accept(ListenSock).

handle_client(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Line} ->
            Command = binary_to_list(Line),
            Tokens = string:tokens(string:trim(Command), " "),
            Response = handle_command(Tokens, Socket),
            gen_tcp:send(Socket, list_to_binary(Response ++ "\n")),
            handle_client(Socket);
        {error, closed} ->
            gen_tcp:close(Socket)
    end.

handle_command(["register", Login], Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, PassBin} ->
            Password = string:trim(binary_to_list(PassBin)),
            Fun = fun() ->
                mnesia:write(#user{login = list_to_binary(Login), password = list_to_binary(Password)})
            end,
            case mnesia:transaction(Fun) of
                {atomic, _} -> "OK: user_created";
                _ -> "ERR: user_creation_failed"
            end;
        _ -> "ERR: no_password"
    end;

handle_command(["login", Login], Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, PassBin} ->
            Password = string:trim(binary_to_list(PassBin)),
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
                {atomic, true} -> "OK: login_successful";
                _ -> "ERR: invalid_credentials"
            end;
        _ -> "ERR: no_password"
    end;

handle_command(["store", Login, Key, EncVal], _) ->
    Fun = fun() ->
        mnesia:write(#secret{
            user = list_to_binary(Login),
            key = list_to_binary(Key),
            value = list_to_binary(EncVal)
        })
    end,
    case mnesia:transaction(Fun) of
        {atomic, _} -> "OK: stored";
        _ -> "ERR: store_failed"
    end;

handle_command(["get", Login, Key], _) ->
    Fun = fun() ->
        MatchHead = #secret{
            user = list_to_binary(Login),
            key = list_to_binary(Key),
            value = '_'
        },
        case mnesia:match_object(MatchHead) of
            [#secret{value = Val}] -> "OK: " ++ binary_to_list(Val);
            [] -> "ERR: not_found"
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Resp} -> Resp;
        _ -> "ERR: get_failed"
    end;

handle_command(["del", Login, Key], _) ->
    UserBin = list_to_binary(Login),
    KeyBin = list_to_binary(Key),
    Fun = fun() ->
        Match = #secret{user = UserBin, key = KeyBin, value = '_'},
        case mnesia:match_object(Match) of
            [RealRecord] ->
                mnesia:delete_object(RealRecord),
                ok;
            [] ->
                not_found
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> "OK: deleted";
        {atomic, not_found} -> "ERR: not_found";
        _ -> "ERR: delete_failed"
    end;

handle_command(Cmd, _) ->
    io:format("[DEBUG] invalid_command: ~p~n", [Cmd]),
    "ERR: invalid_command".

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