-module(gpb_tcp_listener).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(PORT, 1337).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, Listen} = gen_tcp:listen(?PORT, [binary, {active, false}, {reuseaddr, true}]),
    io:format("[listener] started on port ~p~n", [?PORT]),
    self() ! accept,
    {ok, #{listen => Listen}}.

handle_info(accept, State = #{listen := Listen}) ->
    case gen_tcp:accept(Listen, 1000) of
        {ok, Socket} ->
            {ok, Pid} = gpb_tcp_handler:start(Socket),
            ok = gen_tcp:controlling_process(Socket, Pid),
            gpb_tcp_handler:activate(Pid),
            self() ! accept,
            {noreply, State};
        {error, timeout} ->
            self() ! accept,
            {noreply, State};
        {error, closed} ->
            {stop, normal, State};
        {error, Reason} ->
            io:format("[listener] accept error: ~p~n", [Reason]),
            self() ! accept,
            {noreply, State}
    end;
handle_info(_Msg, State) ->
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, #{listen := Listen}) ->
    gen_tcp:close(Listen),
    ok.
