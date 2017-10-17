%%%-------------------------------------------------------------------
%% @doc Handles incoming messages to the user and parses
%% them according to IRC protocol. This module does not
%% handle interaction with the database, just the parsing
%% of the messages.
%% end
%%%-------------------------------------------------------------------

-module(crier_user_handle).

-export([loop/1]).

strip_crlf(String) ->
    lists:subtract(String, "\r\n").

loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Packet} ->
            lager:info("Packet ~p received from ~p~n", [Packet, Socket]),
            case Packet of
                "PING " ++ Host ->
                    crier_user_messages:pong(Socket, strip_crlf(Host));
                "NICK " ++ Nick ->
                    crier_user_store:update_user_data(Socket, nick, strip_crlf(Nick));
                "USER " ++ UserData ->
                    UserDataList = string:split(UserData, " ", all),
                    Username = lists:nth(1, UserDataList),
                    Realname = lists:flatten(lists:join(" ", lists:sublist(UserDataList, 4, 10))) -- ":",
                    crier_user_store:update_user_data(Socket, username, Username),
                    crier_user_store:update_user_data(Socket, realname, strip_crlf(Realname -- ":"));
                _ ->
                    crier_user_store:dispatch_global(Packet, Socket)
            end,
            loop(Socket);
        {error, Reason} ->
            lager:info("crier_user_handle ~p shutting down: ~p.~n", [Socket, Reason]),
            ok
    end.
