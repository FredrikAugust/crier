%%%-------------------------------------------------------------------
%% @doc this is where we will store association between
%% PID of handler and Socket
%% @end
%%%-------------------------------------------------------------------

-module(crier_user_store).

-behaviour(gen_server).

-export([start_link/0, stop/0, add_client/1, remove_user/1, dispatch_global/2, update_user_data/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-include_lib("stdlib/include/ms_transform.hrl").

%% API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

add_client(Socket) ->
    lager:info("Adding new client to ETS: ~p.~n", [Socket]),
    gen_server:cast(?MODULE, {add_client, Socket}).

remove_user(Socket) ->
    lager:info("Removing user ~p.~n", [Socket]),
    gen_server:cast(?MODULE, {remove_user, Socket}).

dispatch_global(Msg, From) ->
    gen_server:cast(?MODULE, {dispatch_global, Msg, From}).

update_user_data(Socket, Type, Value) ->
    lager:info("Setting ~p's ~p to ~p~n", [Socket, Type, Value]),
    NewData = gen_server:call(?MODULE, {update_user_data, Socket, Type, Value}),
    post_reg_handle(Socket, NewData).

post_reg_handle(Socket, #{nick := Nick, username := Username, post_reg_complete := no}) when is_list(Nick) andalso is_list(Username) ->
    crier_user_messages:post_reg(Socket, Nick),
    update_user_data(Socket, post_reg_complete, yes);
post_reg_handle(_, _) ->
    ok.


%% CALLBACKS

init([]) ->
    ?MODULE = ets:new(?MODULE, [set, named_table, public]),
    {ok, ?MODULE}.

handle_call(stop, _From, Table) ->
    ets:delete(Table),
    {stop, normal, ok, Table};
handle_call({update_user_data, Socket, Type, Value}, _From, Table) ->
    [UserData] = ets:select(Table, ets:fun2ms(fun({TSocket, _, _, TUserData}) when TSocket =:= Socket -> TUserData end)),
    ets:update_element(Table, Socket, {4, UserData#{Type := Value}}),
    lager:info("Set ~p's ~p to ~p~n", [Socket, Type, Value]),
    [NewData] = ets:select(Table, ets:fun2ms(fun({TSocket, _, _, TUserData}) when TSocket =:= Socket -> TUserData end)),
    {reply, NewData, Table};
handle_call(_Event, _From, Table) ->
    {noreply, Table}.

handle_cast({remove_user, Socket}, Table) ->
    ets:delete(Table, Socket),
    {noreply, Table};
handle_cast({add_client, Socket}, Table) ->
    Pid = spawn_link(crier_user_handle, loop, [Socket]),
    Ref = erlang:monitor(process, Pid),
    ets:insert(Table, {Socket, Pid, Ref,
                       #{nick => null, username => null,
                        realname => null, channels => [],
                        post_reg_complete => no}}),
    lager:info("New client added to ETS: ~p.~n", [Socket]),
    {noreply, Table};
handle_cast({dispatch_global, Msg, From}, Table) ->
    Users = ets:select(Table, ets:fun2ms(fun({Socket, _Pid, _Ref, _UserData}) when Socket =/= From -> Socket end)),
    lager:info("Sending msg: ~p to users: ~p.~n", [Msg, Users]),
    lists:foreach(fun(Socket) -> gen_tcp:send(Socket, Msg) end, Users),
    {noreply, Table};
handle_cast(_Event, State) ->
    {noreply, State}.

handle_info({'DOWN', Ref, process, _Pid, Reason}, Table) ->
    lager:info("Process ~p shutting down: ~p.~n", [Ref, Reason]),
    ets:match_delete(Table, {'_', '_', Ref, '_'}),
    {noreply, Table};
handle_info(_Event, Table) ->
    {noreply, Table}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
terminate(_Reason, _State) ->
    ok.
