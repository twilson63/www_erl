-module(hello_handler).
-export([init/2]).
-behavior(cowboy_handler).

init(Req, State) ->
  {ok, Resp} = cowboy_req:reply(
    200,
    #{<<"content-type">> => <<"text/plain">>},
    <<"Hello, World!">>,
    Req
  ),
  {ok, Resp, State}.


