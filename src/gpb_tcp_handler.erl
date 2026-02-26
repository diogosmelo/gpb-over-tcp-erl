-module(gpb_tcp_handler).

-behaviour(gen_server).

-export([start/1, activate/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

start(Socket) ->
    gen_server:start(?MODULE, [Socket], []).

activate(Pid) ->
    gen_server:cast(Pid, activate).


init([Socket]) ->
    % buffer accumulates bytes across TCP deliveries until we have a complete frame.
    % A single {tcp, Socket, Data} message may contain a partial frame, a full frame,
    % or multiple frames — the buffer absorbs that uncertainty.
    {ok, #{socket => Socket, buffer => <<>>}}.

handle_cast(activate, State = #{socket := Socket}) ->
    ok = inet:setopts(Socket, [{active, once}]),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, Socket, Data}, State = #{socket := Socket, buffer := Buffer}) ->
    NewBuffer = <<Buffer/binary, Data/binary>>,
    NewState = parse_frames(NewBuffer, State),
    ok = inet:setopts(Socket, [{active, once}]),
    {noreply, NewState};

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

% Recursively extract complete frames from the buffer.
%
% A frame is: <<Len:32/big, Payload:Len/binary>>
%
% We pattern-match on the 4-byte length prefix first. If the rest of the buffer
% contains at least Len bytes, we have a complete frame -> extract it, decode it,
% and recurse on whatever is left. If not, we store what we have and wait for
% more data to arrive.
parse_frames(<<Len:32/big, Rest/binary>>, State) when byte_size(Rest) >= Len ->
    <<Payload:Len/binary, Remaining/binary>> = Rest,
    decode_and_log(Payload),
    parse_frames(Remaining, State#{buffer => Remaining});
parse_frames(Incomplete, State) ->
    State#{buffer => Incomplete}.

decode_and_log(Payload) ->
    try kv_pb:decode_msg(Payload, req_envelope) of
        Msg ->
            io:format("[handler ~p] decoded message: ~p~n", [self(), Msg])
    catch
        error:Reason ->
            io:format("[handler ~p] decode error: ~p~n", [self(), Reason])
    end.
