-module(start_safebox).
-export([start/0]).

start() ->
    code:add_patha("ebin"),
    application:start(crypto),
    safebox_node:start(),
    safebox_net:set_nodes(['node1@localhost', 'node2@localhost', 'node3@localhost']),
    safebox_cli:start(),
    %% Empêche de revenir à l'interpréteur après le CLI
    timer:sleep(infinity).
