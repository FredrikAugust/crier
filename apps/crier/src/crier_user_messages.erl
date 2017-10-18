%%%-------------------------------------------------------------------
%% @doc Responsible for sending messages to the client.
%% Uses the pre-defined IRC codes to ensure compatability
%% with all clients. All functions should return `ok` or
%% raise an error if something unexpected happens.
%% @end
%%%-------------------------------------------------------------------

-module(crier_user_messages).

-export([post_reg/2, pong/2, nick_taken/2, unknown_command/2, too_few_params/2, channel_join/3]).
-define(HOST, ":localhost ").

-define(CRLF, "\r\n").

%% IRC Codes
-define(PONG, "PONG ").
-define(RPL_WELCOME, "001 ").
-define(RPL_YOURHOST, "002 ").
-define(RPL_CREATED, "003 ").
-define(RPL_MYINFO, "004 ").
-define(ERR_NEEDMOREPARAMS, "461 ").
-define(ERR_NICKNAMEINUSE, "433 ").
-define(ERR_UNKNOWNCOMMAND, "421 ").

-define(IRC_REPLY(Code, Content), ?HOST ++ Code ++ Content ++ ?CRLF).

unknown_command(Socket, Command) ->
    lager:info("Dispatching unknown command (~p) to ~p~n", [Command, Socket]),
    gen_tcp:send(Socket, ?IRC_REPLY(?ERR_UNKNOWNCOMMAND, Command ++ " :Unknown command " ++ Command)).

post_reg(Socket, Nick) ->
    lager:info("Dispatching postreg to ~p(~p)~n", [Nick, Socket]),
    gen_tcp:send(Socket, ?IRC_REPLY(?RPL_WELCOME, Nick ++ " :Welcome to crier")),
    gen_tcp:send(Socket, ?IRC_REPLY(?RPL_YOURHOST, Nick ++ " :Your host is " ++ ?HOST)),
    gen_tcp:send(Socket, ?IRC_REPLY(?RPL_CREATED, Nick ++ " :This server was created 2017")),
    gen_tcp:send(Socket, ?IRC_REPLY(?RPL_MYINFO, Nick ++ " localhost")),
    ok.

nick_taken(Socket, TakenNick) ->
    lager:info("Dispatching nick taken to ~p~n", [Socket]),
    gen_tcp:send(Socket, ?IRC_REPLY(?ERR_NICKNAMEINUSE, TakenNick ++ " :Nick " ++ TakenNick ++ " is already taken")),
    ok.

too_few_params(Socket, Command) ->
    lager:info("Dispatching too few parameters for command ~p to ~p~n", [Command, Socket]),
    gen_tcp:send(Socket, ?IRC_REPLY(?ERR_NEEDMOREPARAMS, Command ++ " :Too few parameters for command " ++ Command)).

pong(Socket, Host) ->
    lager:info("Dispatching PONG to ~p~n", [Socket]),
    gen_tcp:send(Socket, ?IRC_REPLY(?PONG, Host ++ " " ++ ?HOST)),
    ok.

channel_join(UserData, Users, Channel) ->
    %% Since I couldn't use lists:member in guard clause in ets:fun2ms, I will work around it here
    ChannelMemberSockets = lists:filtermap(fun({USocket, UUserData}) ->
                                                   case lists:member(Channel, maps:get(channels, UUserData)) of
                                                       true ->
                                                           {true, USocket};
                                                       false ->
                                                           false
                                                   end
                                           end, Users),
    lists:foreach(fun(S) ->
                          lager:debug("Notifying ~p that ~p is joining ~p.~n", [S, maps:get(nick, UserData), Channel]),
                          gen_tcp:send(S, ":" ++ maps:get(nick, UserData) ++ "!~" ++ maps:get(username, UserData) ++ "@" ++ (?HOST -- ":") ++ "JOIN " ++ Channel ++ "\r\n")
                  end, ChannelMemberSockets).
                                          
