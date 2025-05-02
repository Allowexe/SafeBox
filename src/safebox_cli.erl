
-module(safebox_cli).
-export([start/0]).

start() ->
    io:format("Bienvenue dans SafeBox~nCommandes : add <clé>, get <clé>, del <clé>, nodes [liste], quit~n"),
    loop().

loop() ->
    io:format("> "),
    case io:get_line("") of
        eof -> ok;
        Line ->
            Input = string:tokens(string:trim(Line), " "),
            handle_command(Input),
            loop()
    end.

handle_command(["add", KeyStr]) ->
    Key = list_to_binary(KeyStr),
    io:format("Saisir le secret : "),
    SecretInput = string:trim(io:get_line("")),
    Secret = list_to_binary(SecretInput),
    Encrypted = safebox_crypto:encrypt(Secret),
    safebox_net:store_distributed(Key, Encrypted),
    io:format("Secret enregistré.~n");

handle_command(["get", KeyStr]) ->
    Key = list_to_binary(KeyStr),
    case safebox_net:get_distributed(Key) of
        {ok, Encrypted} ->
            case safebox_crypto:decrypt(Encrypted) of
                {ok, Decrypted} ->
                    io:format("Le secret est : ~s~n", [binary_to_list(Decrypted)]);
                {error, invalid_base64} ->
                    io:format("Erreur de déchiffrement.~n")
            end;
        {error, quorum_failed} ->
            io:format("Échec : quorum insuffisant.~n");
        _ ->
            io:format("Clé inconnue.~n")
    end;

handle_command(["del", KeyStr]) ->
    Key = list_to_binary(KeyStr),
    safebox_net:delete_distributed(Key),
    io:format("Secret supprimé (ou tentative).~n");

handle_command(["nodes"]) ->
    Nodes = safebox_net:get_nodes(),
    io:format("Nœuds enregistrés : ~p~n", [Nodes]);

handle_command(["nodes" | Rest]) ->
    NodeList = [list_to_atom(N) || N <- Rest],
    safebox_net:set_nodes(NodeList),
    io:format("Nœuds mis à jour : ~p~n", [NodeList]);

handle_command(["quit"]) ->
    io:format("Fermeture de SafeBox.~n"),
    halt();

handle_command(_) ->
    io:format("Commande inconnue.~n").
