%%%-------------------------------------------------------------------
%% @doc crier top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(crier_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, {{one_for_one, 5, 60}, [
                                #{id => crier_server,
                                 start => {crier_server, start_link, []}},
                                #{id => crier_user_store,
                                 start => {crier_user_store, start_link, []}}
                               ]}}.

%%====================================================================
%% Internal functions
%%====================================================================
