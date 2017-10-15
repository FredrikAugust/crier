%%%-------------------------------------------------------------------
%% @doc this is where we will store association between
%% PID of handler and Socket
%% @end
%%%-------------------------------------------------------------------

-module(crier_user_store).

-behaviour(gen_server).

-export([start_link/0, stop/0, add_client/1, remove_user/1, dispatch_global/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-include_lib("stdlib/include/ms_transform.hrl").

%% API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

add_client(Socket) ->
    io:format("Adding new client to ETS: ~p~n", [Socket]),
    gen_server:cast(?MODULE, {add_client, Socket}).

remove_user(Socket) ->
    gen_server:cast(?MODULE, {remove_user, Socket}).

dispatch_global(Msg, From) ->
    gen_server:cast(?MODULE, {dispatch_global, Msg, From}).

%% CALLBACKS

init([]) ->
    ?MODULE = ets:new(?MODULE, [set, named_table, public]),
    {ok, ?MODULE}.

handle_call(stop, _From, Table) ->
    ets:delete(Table),
    {stop, normal, ok, Table};
handle_call(_Event, _From, Table) ->
    {noreply, Table}.

handle_cast({remove_user, Socket}, Table) ->
    ets:delete(Table, Socket),
    {noreply, Table};
handle_cast({add_client, Socket}, Table) ->
    io:format("New client added: ~p~n", [Socket]),
    Pid = spawn_link(crier_user_handle, loop, [Socket]),
    Ref = erlang:monitor(process, Pid),
    ets:insert(Table, {Socket, Pid, Ref}),
    {noreply, Table};
handle_cast({dispatch_global, Msg, From}, Table) ->
    Users = ets:select(Table, ets:fun2ms(fun({Socket, _Pid, _Ref}) when Socket =/= From -> Socket end)),
    dispatch_messages(Users, Msg),
    {noreply, Table};
handle_cast(_Event, State) ->
    {noreply, State}.

%% Helper function; without this, it will crash if only one user is present
dispatch_messages([], _) ->
    io:format("No other users connected, message will not be sent along.~n"),
    ok;
dispatch_messages(Users, Msg) ->
    io:format("Sending msg: ~p to users.~n", [Msg]),
    lists:foreach(fun(Socket) -> gen_tcp:send(Socket, Msg) end, Users),
    ok.


handle_info({'DOWN', Ref, process, _Pid, Reason}, Table) ->
    io:format("Process shutting down[Ref=~p]: ~p~n", [Ref, Reason]),
    ets:match_delete(Table, {'_', '_', Ref}),
    {noreply, Table};
handle_info(_Event, Table) ->
    {noreply, Table}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
terminate(_Reason, _State) ->
    ok.
