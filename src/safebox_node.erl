-module(safebox_node).
-export([start/0, stop/0, store_secret/2, get_secret/1, delete_secret/1]).

-define(TABLE, safebox_storage).

%% Démarre la table ETS pour le nœud courant
start() ->
    ets:new(?TABLE, [named_table, set, public, {read_concurrency, true}]),
    io:format("~n[~p] SafeBox node démarré.~n", [node()]).

%% Arrête proprement le nœud (supprime la table)
stop() ->
    ets:delete(?TABLE),
    io:format("~n[~p] SafeBox node arrêté.~n", [node()]).

%% Enregistre un secret : store_secret(Key, Value)
store_secret(Key, EncryptedValue) when is_binary(Key), is_binary(EncryptedValue) ->
    ets:insert(?TABLE, {Key, EncryptedValue}),
    {ok, stored}.

%% Récupère un secret chiffré via sa clé
get_secret(Key) when is_binary(Key) ->
    case ets:lookup(?TABLE, Key) of
        [{Key, EncryptedValue}] ->
            {ok, EncryptedValue};
        [] ->
            {error, not_found}
    end.

%% Supprime un secret
delete_secret(Key) when is_binary(Key) ->
    case ets:delete(?TABLE, Key) of
        true -> {ok, deleted};
        _ -> {error, not_found}
    end.
