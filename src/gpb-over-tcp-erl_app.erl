%%%-------------------------------------------------------------------
%% @doc gpb-over-tcp-erl public API
%% @end
%%%-------------------------------------------------------------------

-module(gpb-over-tcp-erl_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    gpb-over-tcp-erl_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
