-module(gpb_over_tcp_erl_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy  => one_for_one,
        intensity => 5,
        period    => 10
    },
    Listener = #{
        id       => gpb_tcp_listener,
        start    => {gpb_tcp_listener, start_link, []},
        restart  => permanent,
        shutdown => 5000,
        type     => worker,
        modules  => [gpb_tcp_listener]
    },
    {ok, {SupFlags, [Listener]}}.
