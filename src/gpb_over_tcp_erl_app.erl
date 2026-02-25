-module(gpb_over_tcp_erl_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    gpb_over_tcp_erl_sup:start_link().

stop(_State) ->
    ok.
