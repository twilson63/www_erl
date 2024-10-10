%%%-------------------------------------------------------------------
%% @doc www_erl top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(www_erl_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/", hello_handler, []}
    ]}
  ]),
  %% Define the child specification for the Cowboy listener
  ChildSpecs = [
    {cowboy_clear_listener,
      {cowboy, start_clear, [http_listener,
        [{port, 8080}],
          #{env => #{dispatch => Dispatch}}
        ]},
        permanent,
        infinity,
        worker,
        [cowboy]}
  ],

  %% Supervision strategy
  {ok, {{one_for_one, 1, 60}, ChildSpecs}}.
