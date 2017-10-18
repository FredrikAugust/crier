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
            lager:debug("Packet ~p received from ~p.~n", [Packet, inet:sockname(Socket)]),
            case Packet of
                "PING " ++ Host ->
                    crier_user_messages:pong(Socket, strip_crlf(Host));
                "PRIVMSG " ++ MessageMeta ->
                    MessageMetaList = string:split(MessageMeta, " ", all),
                    case length(MessageMetaList) >= 2 of
                        true ->
                            From = crier_user_store:lookup(Socket),
                            To = lists:nth(1, MessageMetaList),
                            Message = lists:flatten(lists:join(" ", lists:sublist(MessageMetaList, 2, 256))) -- ":",
                            crier_user_messages:privmsg(From, To, Message);
                        false ->
                            crier_user_messages:too_few_params(Socket, "PRIVMSG")
                    end;
                "NICK " ++ Nick ->
                    case crier_checks:unique_nick(Nick) of
                        unique ->
                            crier_user_store:update_user_data(Socket, nick, strip_crlf(Nick));
                        not_unique ->
                            crier_user_messages:nick_taken(Socket, strip_crlf(Nick))
                    end;
                "USER " ++ UserData ->
                    UserDataList = string:split(UserData, " ", all),
                    case length(UserDataList) >= 4 of
                       true ->
                            Username = lists:nth(1, UserDataList),
                            Realname = lists:flatten(lists:join(" ", lists:sublist(UserDataList, 4, 10))) -- ":",
                            crier_user_store:update_user_data(Socket, username, Username),
                            crier_user_store:update_user_data(Socket, realname, strip_crlf(Realname -- ":"));
                        false ->
                            crier_user_messages:too_few_params(Socket, "USER")
                    end;
                "JOIN " ++ Channel ->
                    crier_user_store:join_channel(Socket, strip_crlf(Channel));
                "NAMES " ++ Channel ->
                    crier_user_messages:channel_names_list(Socket, strip_crlf(Channel));
                Command ->
                    crier_user_messages:unknown_command(Socket, strip_crlf(Command))
            end,
            loop(Socket);
        {error, Reason} ->
            lager:debug("crier_user_handle ~p shutting down: ~p.~n", [Socket, Reason]),
            ok
    end.
