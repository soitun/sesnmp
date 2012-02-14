-module(sesnmp_app).

-behaviour(application).

-define(APP, sesnmp).

-export([start/0, stop/0]).

%%%-----------------------------------------------------------------
%%% application callbacks
%%%-----------------------------------------------------------------
-export([start/2, stop/1]).

start() ->
    application:start(?APP).

stop() ->
    application:stop(?APP).

start(normal, []) ->
    Env = application:get_all_env(?APP),
    sesnmp_sup:start_link(Env).

stop(_) ->
    ok.

