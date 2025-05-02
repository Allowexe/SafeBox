-module(safebox_net).
-export([
    store_distributed/2,
    get_distributed/1,
    delete_distributed/1,
    set_nodes/1
]).

%% Liste des nœuds connus (modifiable dynamiquement)
-define(NODE_TABLE, safebox_nodes).

%%% ========= GESTION DES NOEUDS ========= %%%

%% Initialise la liste des nœuds à contacter
set_nodes(Nodes) when is_list(Nodes) ->
    case ets:info(?NODE_TABLE) of
        undefined ->
            ets:new(?NODE_TABLE, [named_table, set, public]),
            ets:insert(?NODE_TABLE, {nodes, Nodes});
        _ ->
            ets:insert(?NODE_TABLE, {nodes, Nodes})
    end.

get_nodes() ->
    case ets:lookup(?NODE_TABLE, nodes) of
        [{nodes, Ns}] -> Ns;
        [] -> []
    end.

%%% ========= OPÉRATIONS DISTRIBUÉES ========= %%%

%% store_distributed(Key, EncryptedValue)
store_distributed(Key, EncVal) when is_binary(Key), is_binary(EncVal) ->
    Nodes = get_nodes(),
    Results = [rpc:call(Node, safebox_node, store_secret, [Key, EncVal]) || Node <- Nodes],
    {ok, Results}.

%% get_distributed(Key) : récupère le secret via quorum (2/3)
get_distributed(Key) when is_binary(Key) ->
    Nodes = get_nodes(),
    Results = [rpc:call(Node, safebox_node, get_secret, [Key]) || Node <- Nodes],
    ValidResults = [Val || {ok, Val} <- Results],
    case length(ValidResults) >= 2 of
        true ->
            %% On suppose que les valeurs sont identiques (pas de conflit)
            {ok, hd(ValidResults)};
        false ->
            {error, quorum_failed}
    end.

%% delete_distributed(Key) : supprime le secret partout
delete_distributed(Key) when is_binary(Key) ->
    Nodes = get_nodes(),
    [rpc:call(Node, safebox_node, delete_secret, [Key]) || Node <- Nodes],
    ok.
