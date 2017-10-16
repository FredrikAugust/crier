%%%-------------------------------------------------------------------
%% @doc handles incoming messages to the user
%% end
%%%-------------------------------------------------------------------

-module(crier_user_handle).

-export([loop/1]).

loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Packet} ->
            lager:info("Packet ~p received from ~p~n", [Packet, Socket]),
            case Packet of
                "NICK " ++ Nick ->
                    crier_user_store:set_nick(Socket, Nick);
                _ ->
                    crier_user_store:dispatch_global(Packet, Socket)
            end,
            loop(Socket);
        {error, Reason} ->
            lager:info("crier_user_handle ~p shutting down: ~p.~n", [Socket, Reason]),
            ok
    end.
