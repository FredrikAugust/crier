%%%-------------------------------------------------------------------
%% @doc Server to handle incoming requests for crier.
%% Creates a process which handles the incoming packets
%% from the user. This module does not handle any of the
%% logic related to the IRC protocol.
%% @end
%%%-------------------------------------------------------------------

-module(crier_server).

-export([start_link/0]).
-export([init/0]).

start_link() ->
    Pid = spawn_link(?MODULE, init, []),
    {ok, Pid}.

init() ->
    {ok, LSocket} = gen_tcp:listen(5000, [{active, false}, {packet, line}]),
    start_server(LSocket).

start_server(LSocket) ->
    case gen_tcp:accept(LSocket) of
        {ok, Socket} ->
            lager:info("New connection: ~p.~n", [Socket]),
            crier_user_store:add_client(Socket),
            start_server(LSocket);
        {error, Reason} ->
            lager:error("Error encountered: ~p.~n", [Reason]),
            start_server(LSocket)
    end.
