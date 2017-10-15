%%%-------------------------------------------------------------------
%% @doc server to handle incoming requests for crier
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
            io:format("New connection: ~p~n", [Socket]),
            crier_user_store:add_client(Socket),
            start_server(LSocket);
        {error, Reason} ->
            io:format("Error encountered: ~p~n", [Reason]),
            start_server(LSocket)
    end.
