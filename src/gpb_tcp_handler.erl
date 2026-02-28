-module(gpb_tcp_handler).

-behaviour(gen_server).

-include("kv_pb.hrl").

-export([start/1, activate/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(MAX_VIOLATIONS, 3).

start(Socket) ->
    gen_server:start(?MODULE, [Socket], []).

activate(Pid) ->
    gen_server:cast(Pid, activate).

init([Socket]) ->
    % buffer: accumulates bytes across TCP deliveries until we have a complete frame.
    {ok, #{socket => Socket, buffer => <<>>, violations => 0, should_stop => false}}.

handle_cast(activate, State = #{socket := Socket}) ->
    ok = inet:setopts(Socket, [{active, once}]),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, Socket, Data}, State = #{socket := Socket, buffer := Buffer}) ->
    NewBuffer = <<Buffer/binary, Data/binary>>,
    NewState  = parse_frames(NewBuffer, State),
    case should_stop(NewState) of
        true ->
            log_stop_reason(NewState),
            {stop, normal, NewState};
        false ->
            ok = inet:setopts(Socket, [{active, once}]),
            {noreply, NewState}
    end;

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

% Returns true when no more frames should be processed on this connection.
should_stop(#{should_stop := true})                        -> true;
should_stop(#{violations  := V}) when V >= ?MAX_VIOLATIONS -> true;
should_stop(_)                                             -> false.

log_stop_reason(#{violations := V}) when V >= ?MAX_VIOLATIONS ->
    io:format("[handler ~p] too many violations (~p), closing connection~n", [self(), V]);
log_stop_reason(#{should_stop := true}) ->
    % send failure was already logged at the point it occurred
    ok.

% Recursively extract complete frames from the buffer.
%
% A frame is: <<Len:32/big, Payload:Len/binary>>
%
% We pattern-match on the 4-byte length prefix first. If the rest of the buffer
% contains at least Len bytes, we have a complete frame -> extract it, decode it,
% and recurse on whatever is left. If not, we store what we have and wait for
% more data to arrive.
% 
% We stop early if violations hit the limit mid-batch, so bad frames in a single
% TCP delivery do not process frames that follow them.
parse_frames(<<Len:32/big, Rest/binary>>, State) when byte_size(Rest) >= Len ->
    <<Payload:Len/binary, Remaining/binary>> = Rest,
    NewState = dispatch(Payload, State),
    case should_stop(NewState) of
        true  -> NewState#{buffer => Remaining};
        false -> parse_frames(Remaining, NewState#{buffer => Remaining})
    end;
parse_frames(Incomplete, State) ->
    State#{buffer => Incomplete}.

% Decode a GPB frame and dispatch to the appropriate handler.
dispatch(Payload, State = #{socket := Socket}) ->
    try kv_pb:decode_msg(Payload, req_envelope) of
        #req_envelope{type = 'SET_REQ', set_req = #set_request{payload = #data{key = Key, value = Value}}} ->
            case handle_set(Key, Value, Socket) of
                ok              -> State;
                {error, Reason} ->
                    io:format("[handler ~p] send error after SET: ~p~n", [self(), Reason]),
                    State#{should_stop => true}
            end;
        #req_envelope{type = 'GET_REQ', get_req = #get_request{key = Key}} ->
            case handle_get(Key, Socket) of
                ok              -> State;
                {error, Reason} ->
                    io:format("[handler ~p] send error after GET: ~p~n", [self(), Reason]),
                    State#{should_stop => true}
            end;
        #req_envelope{type = Type} ->
            io:format("[handler ~p] unexpected message type: ~p~n", [self(), Type]),
            bump_violations(State)
    catch
        error:Reason ->
            io:format("[handler ~p] decode error: ~p~n", [self(), Reason]),
            bump_violations(State)
    end.

bump_violations(State = #{violations := V}) ->
    State#{violations => V + 1}.

handle_set(Key, Value, Socket) ->
    SetResp = case gpb_kv:set(Key, Value) of
        ok         -> #set_response{result = ok};
        {error, _} -> #set_response{result = internal}
    end,
    send_response(#req_envelope{type = 'SET_RESP', set_resp = SetResp}, Socket).

handle_get(Key, Socket) ->
    GetResp = case gpb_kv:get(Key) of
        {ok, Value}        -> #get_response{result = ok,        payload = #data{key = Key, value = Value}};
        {error, not_found} -> #get_response{result = not_found};
        {error, _}         -> #get_response{result = internal}
    end,
    send_response(#req_envelope{type = 'GET_RESP', get_resp = GetResp}, Socket).

send_response(Envelope, Socket) ->
    Payload = kv_pb:encode_msg(Envelope),
    Frame   = <<(byte_size(Payload)):32/big, Payload/binary>>,
    gen_tcp:send(Socket, Frame).
