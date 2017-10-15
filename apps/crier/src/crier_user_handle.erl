%%%-------------------------------------------------------------------
%% @doc handles incoming messages to the user
%% end
%%%-------------------------------------------------------------------

-module(crier_user_handle).

-export([loop/1]).

loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Packet} ->
            crier_user_store:dispatch_global(Packet, Socket),
            loop(Socket);
        {error, Reason} ->
            io:format("crier_user_handle[~p] shutting down: ~p~n", [Socket, Reason]),
            ok
    end.
