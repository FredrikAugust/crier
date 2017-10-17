%%%-------------------------------------------------------------------
%% @doc This is a module for validating claims and checking
%% validity of users.
%% @end
%%%-------------------------------------------------------------------

-module(crier_checks).

-export([post_reg_check/2]).

%% @doc Checks if the user is ready to receive the postreg
%% messages. These instanciate the connection between user
%% and client.
post_reg_check(Socket, #{nick := Nick, username := Username, post_reg_complete := no}) when is_list(Nick) andalso is_list(Username) ->
    crier_user_messages:post_reg(Socket, Nick),
    crier_user_store:update_user_data(Socket, post_reg_complete, yes);
post_reg_check(_, _) ->
    ok.
