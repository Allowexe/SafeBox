-module(safebox_cli).
-export([start/1]).

-define(PORT, 5000).

start(IPStr) ->
    io:format("SafeBox TCP Client démarré. Connexion à ~s:~p~n", [IPStr, ?PORT]),
    loop(IPStr, undefined).

loop(IPStr, Login) ->
    io:format("> "),
    case io:get_line("") of
        eof -> ok;
        Line ->
            Input = string:tokens(string:trim(Line), " "),
            {NewLogin, _} = handle_command(Input, IPStr, Login),
            loop(IPStr, NewLogin)
    end.

handle_command(["register", LoginStr], IP, Login) ->
    io:format("Saisir mot de passe : "),
    Password = string:trim(io:get_line("")),
    Response = send_tcp(IP, ["register", LoginStr], Password),
    {Login, Response};

handle_command(["login", LoginStr], IP, _) ->
    io:format("Mot de passe : "),
    Password = string:trim(io:get_line("")),
    Response = send_tcp(IP, ["login", LoginStr], Password),
    case string:prefix(Response, "OK: ") of
        true -> {list_to_binary(LoginStr), Response};
        false -> {undefined, Response}
    end;

handle_command(["add", _KeyStr], _IP, undefined) ->
    io:format("Veuillez d'abord vous connecter avec login <utilisateur>.~n"),
    {undefined, "NOT_LOGGED_IN"};

handle_command(["add", KeyStr], IP, Login) ->
    io:format("Saisir le secret : "),
    SecretInput = string:trim(io:get_line("")),
    Encrypted = safebox_crypto:encrypt(list_to_binary(SecretInput)),
    Response = send_tcp(IP, ["store", KeyStr, binary_to_list(Encrypted)]),
    {Login, Response};

handle_command(["get", _KeyStr], _IP, undefined) ->
    io:format("Veuillez d'abord vous connecter avec login <utilisateur>.~n"),
    {undefined, "NOT_LOGGED_IN"};

handle_command(["get", KeyStr], IP, Login) ->
    Response = send_tcp(IP, ["get", KeyStr]),
    case string:substr(Response, 1, 4) of
        "OK: " ->
            EncBase64 = string:trim(string:substr(Response, 5)),
            Decoded = safebox_crypto:decrypt(list_to_binary(EncBase64)),
            case Decoded of
                {ok, PlainBin} -> io:format("Le secret est : ~s~n", [binary_to_list(PlainBin)]);
                {error, _} -> io:format("Erreur de déchiffrement.~n")
            end;
        _ ->
            io:format("Clé inconnue ou réponse invalide.~n")
    end,
    {Login, Response};

handle_command(["del", _KeyStr], _IP, undefined) ->
    io:format("Veuillez d'abord vous connecter avec login <utilisateur>.~n"),
    {undefined, "NOT_LOGGED_IN"};

handle_command(["del", KeyStr], IP, Login) ->
    Response = send_tcp(IP, ["del", KeyStr]),
    io:format("~s~n", [Response]),
    {Login, Response};

handle_command(["quit"], _, _) ->
    io:format("Fermeture de SafeBox.~n"),
    halt();

handle_command(_, _, Login) ->
    io:format("Commande inconnue.~n"),
    {Login, "ERR"}.

send_tcp(IPStr, Parts) ->
    send_tcp(IPStr, Parts, undefined).

send_tcp(IPStr, Parts, undefined) ->
    {ok, Socket} = gen_tcp:connect(parse_ip(IPStr), ?PORT, [binary, {packet, line}, {active, false}]),
    Command = string:join(Parts, " "),
    gen_tcp:send(Socket, list_to_binary(Command ++ "\n")),
    case gen_tcp:recv(Socket, 0) of
        {ok, Line} -> binary_to_list(Line);
        {error, closed} -> "ERR"
    end;

send_tcp(IPStr, Parts, Password) ->
    {ok, Socket} = gen_tcp:connect(parse_ip(IPStr), ?PORT, [binary, {packet, line}, {active, false}]),
    Command = string:join(Parts, " "),
    gen_tcp:send(Socket, list_to_binary(Command ++ "\n")),
    gen_tcp:send(Socket, list_to_binary(Password ++ "\n")),
    case gen_tcp:recv(Socket, 0) of
        {ok, Line} -> binary_to_list(Line);
        {error, closed} -> "ERR"
    end.

parse_ip(Str) ->
    [A,B,C,D] = [list_to_integer(S) || S <- string:tokens(Str, ".")],
    {A,B,C,D}.