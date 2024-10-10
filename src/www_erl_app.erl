%%%-------------------------------------------------------------------
%% @doc www_erl public API
%% @end
%%%-------------------------------------------------------------------

-module(www_erl_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  www_erl_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
