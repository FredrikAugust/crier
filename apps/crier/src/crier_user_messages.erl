%%%-------------------------------------------------------------------
%% @doc handles sending standardized IRC messages to the
%% client
%% @end
%%%-------------------------------------------------------------------

-module(crier_user_messages).

-export([post_reg/2, pong/2]).
-define(HOST, ":localhost ").

-define(CRLF, "\r\n").

%% IRC Codes
-define(RPL_WELCOME, "001 ").
-define(RPL_YOURHOST, "002 ").
-define(RPL_CREATED, "003 ").
-define(RPL_MYINFO, "004 ").

post_reg(Socket, Nick) ->
    lager:info("Dispatching postreg to ~p(~p)~n", [Nick, Socket]),
    gen_tcp:send(Socket, ?HOST ++ ?RPL_WELCOME ++ Nick ++ " :Welcome to crier." ++ ?CRLF),
    gen_tcp:send(Socket, ?HOST ++ ?RPL_YOURHOST ++ Nick ++ " :Your host is " ++ ?HOST ++ ?CRLF),
    gen_tcp:send(Socket, ?HOST ++ ?RPL_CREATED ++ Nick ++ " :This server was created 2017" ++ ?CRLF),
    gen_tcp:send(Socket, ?HOST ++ ?RPL_MYINFO ++ Nick ++ " localhost" ++ ?CRLF),
    ok.

pong(Socket, Host) ->
    lager:info("Dispatching PONG to ~p~n", [Socket]),
    gen_tcp:send(Socket, ?HOST ++ "PONG " ++ Host ++ " " ++ ?HOST),
    ok.
