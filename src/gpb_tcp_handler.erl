-module(gpb_tcp_handler).

-behaviour(gen_server).

-export([start/1, activate/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

start(Socket) ->
    gen_server:start(?MODULE, [Socket], []).

activate(Pid) ->
    gen_server:cast(Pid, activate).

init([Socket]) ->
    {ok, #{socket => Socket}}.

handle_cast(activate, State = #{socket := Socket}) ->
    ok = inet:setopts(Socket, [{active, once}]),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, Socket, Data}, State = #{socket := Socket}) ->
    io:format("[handler ~p] received ~p bytes: ~p~n", [self(), byte_size(Data), Data]),
    ok = inet:setopts(Socket, [{active, once}]),
    {noreply, State};

handle_info({tcp_closed, Socket}, State = #{socket := Socket}) ->
    io:format("[handler ~p] connection closed~n", [self()]),
    {stop, normal, State};

handle_info({tcp_error, Socket, Reason}, State = #{socket := Socket}) ->
    io:format("[handler ~p] socket error: ~p~n", [self(), Reason]),
    {stop, Reason, State};

handle_info(_Msg, State) ->
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

terminate(_Reason, #{socket := Socket}) ->
    gen_tcp:close(Socket),
    ok.
