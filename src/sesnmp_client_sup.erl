-module(sesnmp_client_sup).

-behaviour(supervisor).

-export([start_link/1, init/1]).  

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------
start_link(Opts) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Opts]).

%%--------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}   
%%--------------------------------------------------------------------
init([Opts]) ->
    PoolSize = get_opt(Opts, pool_size, 4),
    Sups = [begin
                Id = list_to_atom("sesnmp_client_" ++ integer_to_list(I)), 
                {Id, {sesnmp_client, start_link, [Opts]}, permanent, 10, worker, [sesnmp_server]}
            end || I <- lists:seq(1,PoolSize)],
    {ok, {{one_for_one, 5, 500}, Sups}}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
get_opt(Opts, Key, Def) ->
    sesnmp_misc:get_option(Key, Opts, Def).

