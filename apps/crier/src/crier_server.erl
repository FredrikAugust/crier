%%%-------------------------------------------------------------------
%% @doc server to handle incoming requests for crier
%% @end
%%%-------------------------------------------------------------------

-module(crier_server).

-export([init/0]).

-export([loop/1]).

init() ->
    {ok, LSocket} = gen_tcp:listen(5000, [{active, false}, {packet, line}]),
    start_server(LSocket).

start_server(LSocket) ->
    case gen_tcp:accept(LSocket) of
        {ok, Socket} ->
            io:format("~p~n", [Socket]),
            spawn(?MODULE, loop, [Socket]),
            start_server(LSocket);
        {error, Reason} ->
            io:format("Error encountered: ~p~n", [Reason]),
            ok
    end.
    
loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of % 0 here means get all of the data
        {ok, Packet} ->
            io:format("~p~n", [Packet]),
            gen_tcp:send(Socket, "Data Received."),
            loop(Socket);
        {error, Reason} ->
            io:format("Conection closed: ~p~n", [Reason]),
            ok
    end.
