-module(gpb_env).

-export([read_env/1, parse_endpoint/1]).

% Read a list of OS environment variable names.
% Returns {ok, Map} where Map is #{VarName => Value} if all are present.
% Returns {error, {missing_env, VarName}} on the first missing variable.
read_env(Vars) ->
    read_env(Vars, #{}).

read_env([], Acc) ->
    {ok, Acc};
read_env([Var | Rest], Acc) ->
    case os:getenv(Var) of
        false -> {error, {missing_env, Var}};
        Value -> read_env(Rest, Acc#{Var => Value})
    end.

% Parse an endpoint URL into the {Scheme, Host, Port} tuple that aws_config expects.
% The scheme is returned with the "://" suffix included, as erlcloud requires.
parse_endpoint(Url) ->
    Map    = uri_string:parse(Url),
    Scheme = maps:get(scheme, Map),
    Host   = maps:get(host, Map),
    Port   = maps:get(port, Map, default_port(Scheme)),
    {Scheme ++ "://", Host, Port}.

default_port("https") -> 443;
default_port("http")  -> 80.
