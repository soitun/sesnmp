%%----------------------------------------------------------------------
%%This module implements a simple SNMP manager for Erlang.
%%----------------------------------------------------------------------
-module(sesnmp_server).

%% User interface
-export([start_link/0,
		next_req_id/0,
		stop/0,
		insert/1,
		lookup/1,
		delete/1,
		down/1]).

%% Internal exports
-export([init/1,
		handle_call/3,
		handle_cast/2,
		handle_info/2,
		code_change/3,
		terminate/2]).

-include("sesnmp_internal.hrl").

-record(state, {req_id}).

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

next_req_id() ->
    gen_server:call(?MODULE, next_req_id).

insert(Req) ->
    gen_server:call(?MODULE, {insert, Req}).

lookup(ReqId) ->
    ets:lookup(sesnmp_request_table, ReqId).

delete(ReqId) ->
    gen_server:cast(?MODULE, {delete, ReqId}).

down(MonRef) ->
    gen_server:cast(?MODULE, {down, MonRef}).

init([]) ->
    case (catch do_init()) of
	{ok, State} ->
	    {ok, State};
	{error, Reason} ->
	    {stop, Reason}
    end.

do_init() ->
    process_flag(trap_exit, true),
    {A,B,C} = erlang:now(),
    random:seed(A,B,C),
    ReqId = random:uniform(2147483647),
    ets:new(sesnmp_request_table, [set, named_table, protected, {keypos, #request.id}]),
    {ok, #state{req_id = ReqId}}.

handle_call(next_req_id, _From, #state{req_id = Id} = State) ->
    Id2 = Id + 1,
    {reply, Id2, State#state{req_id = Id2}};

handle_call({insert, Req}, _From, State) ->
    ets:insert(sesnmp_request_table, Req),
    {reply, ok, State};
    
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(Req, _From, State) ->
    {stop, {badreq, Req}, State}.

handle_cast({delete, ReqId}, State) ->
    ets:delete(sesnmp_request_table, ReqId),
    {noreply, State};

handle_cast({down, MonRef}, State) ->
    Pat = #request{id = '$1', ref = '$2', mon = MonRef, _ = '_'},
    Match = ets:match(sesnmp_request_table, Pat),
    lists:foreach(fun([Id, Ref]) -> 
		  ets:delete(sesnmp_request_table, Id),
		  cancel_timer(Ref)
	end, Match),
    {noreply, State};

handle_cast(Msg, State) ->
    {stop, {badcast, Msg}, State}.

handle_info(Info, State) ->
    {stop, {badinfo, Info}, State}.

code_change(_Vsn, State, _Extra) ->
    {ok, State}.
 
terminate(_Reason, _State) ->
    ok.

%%%-----------------------------------------------------------------
%%% Internal Functions
%%%-----------------------------------------------------------------
cancel_timer(undefined) ->
    ok;
cancel_timer(Ref) ->
    (catch erlang:cancel_timer(Ref)).
