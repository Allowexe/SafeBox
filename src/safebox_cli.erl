-module(safebox_cli).
-export([start/1]).

-define(PORT, 5000).

start(IPStr) ->
    io:format("SafeBox TCP Client démarré. Connexion à ~s:~p~n", [IPStr, ?PORT]),
    loop(IPStr).

loop(IPStr) ->
    io:format("> "),
    case io:get_line("") of
        eof -> ok;
        Line ->
            Input = string:tokens(string:trim(Line), " "),
            handle_command(Input, IPStr),
            loop(IPStr)
    end.

handle_command(["add", KeyStr], IP) ->
    Key = string:trim(KeyStr),
    io:format("Saisir le secret : "),
    SecretInput = string:trim(io:get_line("")),
    Encrypted = safebox_crypto:encrypt(list_to_binary(SecretInput)),
    send_tcp(IP, ["store", Key, binary_to_list(Encrypted)]),
    io:format("Secret chiffré et enregistré.~n");

handle_command(["get", KeyStr], IP) ->
    Key = string:trim(KeyStr),
    Response = send_tcp(IP, ["get", Key]),
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
    end;

handle_command(["del", KeyStr], IP) ->
    Key = string:trim(KeyStr),
    send_tcp(IP, ["del", Key]),
    io:format("Clé supprimée (ou tentative).~n");

handle_command(["quit"], _) ->
    io:format("Fermeture de SafeBox.~n"),
    halt();

handle_command(_, _) ->
    io:format("Commande inconnue.~n").

send_tcp(IPStr, Parts) ->
    {ok, Socket} = gen_tcp:connect(parse_ip(IPStr), ?PORT, [binary, {packet, line}, {active, false}]),
    Command = string:join(Parts, " "),
    gen_tcp:send(Socket, list_to_binary(Command ++ "\n")),
    case gen_tcp:recv(Socket, 0) of
        {ok, Line} ->
            binary_to_list(Line);
        {error, closed} ->
            "ERR"
    end.

parse_ip(Str) ->
    [A,B,C,D] = [list_to_integer(S) || S <- string:tokens(Str, ".")],
    {A,B,C,D}.