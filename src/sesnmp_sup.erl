-module(sesnmp_sup).

-behaviour(supervisor).

-export([start_link/1, init/1]).

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------
start_link(Opts) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Opts]).


init([Opts]) ->
    Server = {sesnmp_server, {sesnmp_server, start_link, []},
	      permanent, 10, worker, [sesnmp_server]},
    ClientSup = {sesnmp_client_sup,{sesnmp_client_sup, start_link, [Opts]},
	      permanent, 10, supervisor, [sesnmp_client_sup]},
    Sups = [Server, ClientSup],
    Sups1 = case proplists:get_value(trapd, Opts) of
    undefined -> 
        Sups;
    _ -> 
        Trapd = {sesnmp_trapd, {sesnmp_trapd, start_link, [Opts]},
            permanent, 10, worker, [sesnmp_trapd]},
        Sups ++ [Trapd]
    end,
	{ok, {{one_for_all, 1, 10}, Sups1}}.
