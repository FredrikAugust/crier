%%%-------------------------------------------------------------------
%% @doc Responsible for sending messages to the client.
%% Uses the pre-defined IRC codes to ensure compatability
%% with all clients. All functions should return `ok` or
%% raise an error if something unexpected happens.
%% @end
%%%-------------------------------------------------------------------

-module(crier_user_messages).

-export([post_reg/2, pong/2, nick_taken/2, unknown_command/2, too_few_params/2, channel_join/3, privmsg/3, channel_names_list/2]).
-define(HOST, ":localhost ").

-define(CRLF, "\r\n").

%% IRC Codes
-define(PONG, "PONG ").
-define(RPL_WELCOME, "001 ").
-define(RPL_YOURHOST, "002 ").
-define(RPL_CREATED, "003 ").
-define(RPL_MYINFO, "004 ").
-define(RPL_NAMEREPLY, "353 ").
-define(RPL_ENDOFNAMES, "366 ").
-define(ERR_NEEDMOREPARAMS, "461 ").
-define(ERR_NICKNAMEINUSE, "433 ").
-define(ERR_UNKNOWNCOMMAND, "421 ").

-define(IRC_REPLY(Code, Content), ?HOST ++ Code ++ Content ++ ?CRLF).

unknown_command(Socket, Command) ->
    lager:debug("Dispatching unknown command (~p) to ~p~n", [Command, Socket]),
    gen_tcp:send(Socket, ?IRC_REPLY(?ERR_UNKNOWNCOMMAND, Command ++ " :Unknown command " ++ Command)).

post_reg(Socket, Nick) ->
    lager:debug("Dispatching postreg to ~p(~p)~n", [Nick, Socket]),
    gen_tcp:send(Socket, ?IRC_REPLY(?RPL_WELCOME, Nick ++ " :Welcome to crier")),
    gen_tcp:send(Socket, ?IRC_REPLY(?RPL_YOURHOST, Nick ++ " :Your host is " ++ ?HOST)),
    gen_tcp:send(Socket, ?IRC_REPLY(?RPL_CREATED, Nick ++ " :This server was created 2017")),
    gen_tcp:send(Socket, ?IRC_REPLY(?RPL_MYINFO, Nick ++ " localhost")),
    ok.

nick_taken(Socket, TakenNick) ->
    lager:debug("Dispatching nick taken to ~p~n", [Socket]),
    gen_tcp:send(Socket, ?IRC_REPLY(?ERR_NICKNAMEINUSE, TakenNick ++ " :Nick " ++ TakenNick ++ " is already taken")),
    ok.

too_few_params(Socket, Command) ->
    lager:debug("Dispatching too few parameters for command ~p to ~p~n", [Command, Socket]),
    gen_tcp:send(Socket, ?IRC_REPLY(?ERR_NEEDMOREPARAMS, Command ++ " :Too few parameters for command " ++ Command)).

pong(Socket, Host) ->
    lager:debug("Dispatching PONG to ~p~n", [Socket]),
    gen_tcp:send(Socket, ?IRC_REPLY(?PONG, Host ++ " " ++ ?HOST)),
    ok.

channel_join(UserData, Users, Channel) ->
    ChannelMemberSockets = crier_checks:filter_by_channel(Users, Channel),
    lists:foreach(fun(S) ->
                          lager:debug("Notifying ~p that ~p is joining ~p.~n", [S, maps:get(nick, UserData), Channel]),
                          gen_tcp:send(S, ":" ++ maps:get(nick, UserData) ++ "!~" ++ maps:get(username, UserData) ++ "@" ++ (?HOST -- ":") ++ "JOIN " ++ Channel ++ "\r\n")
                  end, ChannelMemberSockets).
     
channel_names_list(Socket, Channel) ->
    ChannelMemberSockets = crier_checks:filter_by_channel(crier_user_store:all(), Channel),
    {Socket, _, _, UserData} = crier_user_store:lookup(Socket),
    lager:debug("Notifying ~p of NAMES on ~p~n", [Socket, Channel]),
    gen_tcp:send(Socket, ?IRC_REPLY(?RPL_NAMEREPLY, maps:get(nick, UserData) ++
                                        " @ " ++ Channel ++ " :" ++
                                        lists:flatten(
                                          lists:join(" ",
                                                     lists:map(fun(S) ->
                                                                       {_, _, _, Data} = crier_user_store:lookup(S),
                                                                       maps:get(nick, Data)
                                                               end, ChannelMemberSockets))))).

privmsg({USocket, _, _, UData}, ToChannel, Message) ->
    case ToChannel of
        "#" ++ _ ->
            lager:debug("Sending message ~p to ~p. Origin: ~p~n", [Message, ToChannel, maps:get(nick, UData)]),
            Recipients = crier_checks:filter_by_channel(crier_user_store:all(), ToChannel) -- [USocket], % no reason to send back to ourselves
            lager:debug("Recipients: ~p~n", [Recipients]),
            lists:foreach(fun(S) ->
                                  lager:debug("PRIVMSG dispatched to ~p in ~p~n", [S, ToChannel]),
                                  gen_tcp:send(S, ":" ++ maps:get(nick, UData) ++ "!~" ++ maps:get(username, UData) ++ "@" ++ (?HOST -- ":") ++ "PRIVMSG " ++ ToChannel ++ " :" ++ Message)
                          end, Recipients);
        _ ->
            lager:warn("PRIVMSG to users is not yet implemented~n"),
            unknown_command(USocket, "PRIVMSG")
    end.
