%%----------------------------------------------------------------------
%%This module implements a simple SNMP manager for Erlang.
%%----------------------------------------------------------------------
-module(sesnmp_client).

%% User interface
-export([start_link/1, 
        stop/1, 
        get/4, get/5, 
        get_next/4, get_next/5, 
        get_bulk/6, get_bulk/7, 
        set/4, set/5]).

%% Internal exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-include("elog.hrl").

-include_lib("snmp/include/snmp_types.hrl").

-record(state,
	{net_if,
	 net_if_ref,
     net_if_opts
	}).

-record(request, 
	{id, 
	 addr, 
	 port, 
	 type, 
	 data, 
	 ref, 
	 mon, 
	 from
	}). 

-define(PORT, 161).

-define(VERSION, v2c).

-define(COMMUNITY, "public").

-define(WRITE_COMMUNITY, "private").

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------
start_link(Opts) ->
	gen_server:start_link(?MODULE, [Opts], []).

stop(Pid) ->
    call(Pid, stop).

get(Addr0, Oids, AgentData, Timeout) ->
    get(Addr0, ?PORT, Oids, AgentData, Timeout).

get(Addr0, Port, Oids, AgentData, Timeout) ->
    case inet:getaddr(Addr0, inet) of
    {ok, Addr} -> 
        call({sync_get, self(), Addr, Port, Oids, AgentData, Timeout});
    {error, Error} ->
        {error, Error}
    end.

get_next(Addr0, Oids, AgentData, Timeout) ->
    get_next(Addr0, ?PORT, Oids, AgentData, Timeout). 

get_next(Addr0, Port, Oids, AgentData, Timeout) ->
    case inet:getaddr(Addr0, inet) of
    {ok, Addr} ->
        call({sync_get_next, self(), Addr, Port, Oids, AgentData, Timeout});
    {error, Error} ->
        {error,Error}
    end.

get_bulk(Addr0, NonRep, MaxRep, Oids, AgentData, Timeout) ->
    get_bulk(Addr0, ?PORT, NonRep, MaxRep, Oids, AgentData, Timeout).

get_bulk(Addr0, Port, NonRep, MaxRep, Oids, AgentData, Timeout) ->
    case inet:getaddr(Addr0, inet) of
    {ok, Addr} -> 
        call({sync_get_bulk, self(), Addr, Port, NonRep, MaxRep, Oids, AgentData, Timeout});
    {error, Error} ->
        {error, Error}
    end.

set(Addr0, VarsAndVals, AgentData, Timeout) ->
    set(Addr0, ?PORT, VarsAndVals, AgentData, Timeout).

set(Addr0, Port, VarsAndVals, AgentData, Timeout) ->
    case inet:getaddr(Addr0, inet) of
    {ok, Addr} -> 
        call({sync_set, self(), Addr, Port, VarsAndVals, AgentData, Timeout});
    {error, Error} ->
        {error,Error}
    end.

init([Opts]) ->
    case (catch do_init(Opts)) of
	{ok, State} ->
	    {ok, State};
	{error, Reason} ->
	    {stop, Reason}
    end.

do_init(Opts) ->
    process_flag(trap_exit, true),
    pg2:create(sesnmp_client),
    pg2:join(sesnmp_client, self()),
    NetIfOpts = get_opt(net_if, Opts),
    {NetIf, NetIfRef} = do_init_net_if(NetIfOpts),
    {ok, #state{net_if = NetIf, net_if_ref = NetIfRef, net_if_opts = NetIfOpts}}.
    
do_init_net_if(NetIfOpts) ->
    {ok, NetIf} = sesnmp_udp:start_link(self(), NetIfOpts),
    NetIfRef = erlang:monitor(process, NetIf),
    {NetIf, NetIfRef}.

handle_call({sync_get, Pid, Addr, Port, Oids, AgentData, Timeout}, From, State) ->
    case (catch handle_sync_get(Pid, Addr, Port, Oids, AgentData, Timeout, From, State)) of
	ok ->
	    {noreply, State};
	Error ->
	    {reply, Error, State}
    end;

handle_call({sync_get_next, Pid, Addr, Port, Oids, AgentData, Timeout}, From, State) ->
    case (catch handle_sync_get_next(Pid, Addr, Port, Oids, AgentData, Timeout, From, State)) of
	ok ->
	    {noreply, State};
	Error ->
	    {reply, Error, State}
    end;

handle_call({sync_get_bulk, Pid, Addr, Port, NonRep, MaxRep, Oids, AgentData, Timeout}, 
	    From, State) ->
    case (catch handle_sync_get_bulk(Pid, Addr, Port, NonRep, MaxRep, Oids, AgentData, Timeout, From, State)) of
	ok ->
	    {noreply, State};
	Error ->
	    {reply, Error, State}
    end;

handle_call({sync_set, Pid, Addr, Port, VarsAndVals, AgentData, Timeout}, From, State) ->
    case (catch handle_sync_set(Pid, Addr, Port, VarsAndVals, AgentData, Timeout, From, State)) of
	ok ->
	    {noreply, State};
	Error ->
	    {reply, Error, State}
    end;

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(Req, _From, State) ->
    ?WARNING("unexpected request: ~p", [Req]),
    {reply, {error, unexpected_request}, State}.

handle_cast(Msg, State) ->
    ?WARNING("unexpected message: ~p", [Msg]),
    {noreply, State}.
    
handle_info({sync_timeout, ReqId, From}, State) ->
    %?WARNING("received sync_timeout [~w] message", [ReqId]),
    case lookup_req(ReqId) of
	[#request{addr = Addr, mon = MonRef, from = From} = _Req0] ->
	    gen_server:reply(From, {error, {timeout, {ReqId, Addr}}}),
	    maybe_demonitor(MonRef),
	    delete_req(ReqId);
	_ ->
        ?WARNING("cannot lookup request: ~p", [ReqId])
    end,
    {noreply, State};

handle_info({snmp_pdu, Pdu, Addr, Port}, State) ->
    handle_snmp_pdu(Pdu, Addr, Port, State),
    {noreply, State};

handle_info({snmp_trap, Trap, Addr, Port}, State) ->
    ?WARNING("unexpected snmp_trap message", [Addr, Port, Trap]),
    {noreply, State};

handle_info({snmp_error, Pdu, Reason}, State) ->
    handle_snmp_error(Pdu, Reason, State),
    {noreply, State};

handle_info({snmp_error, Reason, Addr, Port}, State) ->
    handle_snmp_error(Addr, Port, -1, Reason, State),
    {noreply, State};

handle_info({snmp_error, ReqId, Reason, Addr, Port}, State) ->
    handle_snmp_error(Addr, Port, ReqId, Reason, State),
    {noreply, State};

handle_info({'DOWN', _MonRef, process, Pid, _Reason}, 
	    #state{net_if_opts = NetIfOpts, net_if = Pid} = State) ->
    {NetIf, NetIfRef} = do_init_net_if(NetIfOpts),
    {noreply, State#state{net_if = NetIf, net_if_ref = NetIfRef}};

handle_info({'DOWN', MonRef, process, _Pid, _Reason}, State) ->
    sesnmp_server:down(MonRef),
    {noreply, State};

handle_info(Info, State) ->
    ?WARNING("received unknown info: ~p", [Info]),
    {noreply, State}.

code_change(_Vsn, State, _Extra) ->
    {ok, State}.
 
%%----------------------------------------------------------
%% Terminate
%%----------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

handle_sync_get(Pid, Addr, Port, Oids, AgentData, Timeout, From, State) ->    
    MsgData = msg_data(AgentData),
    ReqId = send_get_request(Addr, Port, Oids, MsgData, State),
    Msg    = {sync_timeout, ReqId, From},
    Ref    = erlang:send_after(Timeout, self(), Msg),
    MonRef = erlang:monitor(process, Pid),
    Req    = #request{id = ReqId,
              addr    = Addr,
              port    = Port,
              type    = get, 
              data    = MsgData, 
              ref     = Ref, 
              mon     = MonRef, 
              from    = From},
    insert_req(Req),
    ok.

handle_sync_get_next(Pid, Addr, Port, Oids, AgentData, Timeout, From, State) ->
    MsgData = msg_data(AgentData),
    ReqId  = send_get_next_request(Addr, Port, Oids, MsgData, State),
    Msg    = {sync_timeout, ReqId, From},
    Ref    = erlang:send_after(Timeout, self(), Msg),
    MonRef = erlang:monitor(process, Pid),
    Req    = #request{id = ReqId,
              addr    = Addr,
              port    = Port,
              type    = get_next, 
              data    = AgentData,
              ref     = Ref, 
              mon     = MonRef, 
              from    = From},
    insert_req(Req),
    ok.

handle_sync_get_bulk(Pid, Addr, Port, NonRep, MaxRep, Oids, AgentData, Timeout, From, State) ->
    MsgData = msg_data(AgentData),
    ReqId  = send_get_bulk_request(Oids, Addr, Port, NonRep, MaxRep, MsgData, State),
    Msg    = {sync_timeout, ReqId, From},
    Ref    = erlang:send_after(Timeout, self(), Msg),
    MonRef = erlang:monitor(process, Pid),
    Req    = #request{id = ReqId,
              addr    = Addr,
              port    = Port,
              type    = get_bulk, 
              data    = MsgData, 
              ref     = Ref, 
              mon     = MonRef, 
              from    = From},
    insert_req(Req),
    ok.

handle_sync_set(Pid, Addr, Port, VarsAndVals, AgentData, Timeout, From, State) ->
    MsgData = msg_data(write, AgentData),
    ReqId  = send_set_request(Addr, Port, VarsAndVals, MsgData, State),
    Msg    = {sync_timeout, ReqId, From},
    Ref    = erlang:send_after(Timeout, self(), Msg),
    MonRef = erlang:monitor(process, Pid),
    Req    = #request{id = ReqId,
              addr    = Addr,
              port    = Port,
              type    = set, 
              data    = MsgData, 
              ref     = Ref, 
              mon     = MonRef, 
              from    = From},
    insert_req(Req),
    ok.
 
handle_snmp_error(#pdu{request_id = ReqId} = _Pdu, Reason, _State) ->
    case lookup_req(ReqId) of
	[#request{ref = _Ref, mon = MonRef, from = From}] -> 
	    maybe_demonitor(MonRef),
	    Reply = {error, {send_failed, ReqId, Reason}},
	    gen_server:reply(From, Reply),
	    delete_req(ReqId),
	    ok;
	_ ->
		?ERROR("unexpected snmp error: ~p, ~p",[ReqId, Reason])
    end;

handle_snmp_error(CrapError, Reason, _State) ->
    ?ERROR("received crap (snmp) error => ~n~p~n~p", [CrapError, Reason]),
    ok.

handle_snmp_error(Addr, Port, ReqId, Reason, _State) ->
    ?WARNING("snmp error: ~p ~p ~p ~p", [Addr, Port, ReqId, Reason]).

handle_snmp_pdu(#pdu{type = 'get-response', request_id = ReqId} = Pdu, 
    _Addr, _Port, _State) ->
    case lookup_req(ReqId) of
	[#request{ref = Ref, mon = MonRef, from = From}] -> 
	    Remaining = 
		case (catch cancel_timer(Ref)) of
		    Rem when is_integer(Rem) ->
			Rem;
		    _ ->
			0
		end,
	    ?DEBUG("handle_snmp_pdu(get-response) -> Remaining: ~p", [Remaining]),
	    maybe_demonitor(MonRef),
	    #pdu{error_status = EStatus, 
		 error_index  = EIndex, 
		 varbinds     = Varbinds} = Pdu,
	    SnmpReply = {EStatus, EIndex, Varbinds},
	    Reply = {ok, SnmpReply, Remaining},
	    gen_server:reply(From, Reply),
	    delete_req(ReqId),
	    ok;
	_ ->
        ?ERROR("delay get-response: ~p", [ReqId]),
        ok
	end;

handle_snmp_pdu(CrapPdu, Addr, Port, _State) ->
    ?ERROR("received crap (snmp) Pdu from ~w:~w =>"
	      "~p", [Addr, Port, CrapPdu]),
    ok.

%%----------------------------------------------------------------------
%% 
%%----------------------------------------------------------------------
send_get_request(Addr, Port, Oids, MsgData, #state{net_if = NetIf}) ->
    Pdu = make_pdu(get, Oids),
    (catch sesnmp_udp:send_pdu(NetIf, Addr, Port, Pdu, MsgData)),
    Pdu#pdu.request_id.

send_get_next_request(Addr, Port, Oids, MsgData,  
		      #state{net_if = NetIf}) ->
    Pdu = make_pdu(get_next, Oids),
    sesnmp_udp:send_pdu(NetIf, Addr, Port, Pdu, MsgData),
    Pdu#pdu.request_id.

send_get_bulk_request(Addr, Port, Oids, NonRep, MaxRep, MsgData,  
		      #state{net_if = NetIf}) ->
    Pdu = make_pdu(bulk, {NonRep, MaxRep, Oids}),
    sesnmp_udp:send_pdu(NetIf, Addr, Port, Pdu, MsgData),
    Pdu#pdu.request_id.

send_set_request(Addr, Port, VarsAndVals, MsgData,  
		 #state{net_if = NetIf}) ->
    Pdu = make_pdu(set, VarsAndVals),
    sesnmp_udp:send_pdu(NetIf, Addr, Port, Pdu, MsgData),
    Pdu#pdu.request_id.

%%----------------------------------------------------------------------
%% 
%%----------------------------------------------------------------------
make_pdu(set, VarsAndVals) ->
    VBs = [var_and_value_to_varbind(VAV) || VAV <- VarsAndVals],
    make_pdu_impl(set, VBs);

make_pdu(bulk, {NonRepeaters, MaxRepetitions, Oids}) ->
    check_is_pure_oid(lists:flatten(Oids)),
    #pdu{type         = 'get-bulk-request', 
	 request_id   = request_id(),
	 error_status = NonRepeaters, 
	 error_index  = MaxRepetitions,
	 varbinds     = [make_vb(Oid) || Oid <- Oids]};

make_pdu(Op, Oids) ->
    make_pdu_impl(Op, Oids).

make_pdu_impl(get, Oids) ->
    check_is_pure_oid(lists:flatten(Oids)),
    #pdu{type         = 'get-request',
	 request_id   = request_id(),
	 error_status = noError, 
	 error_index  = 0,
	 varbinds     = [make_vb(Oid) || Oid <- Oids]};

make_pdu_impl(get_next, Oids) ->
    check_is_pure_oid(lists:flatten(Oids)),
    #pdu{type         = 'get-next-request', 
	 request_id   = request_id(), 
	 error_status = noError, 
	 error_index  = 0,
	 varbinds     = [make_vb(Oid) || Oid <- Oids]};

make_pdu_impl(set, Varbinds) ->
    #pdu{type         = 'set-request', 
	 request_id   = request_id(),
	 error_status = noError, 
	 error_index  = 0, 
	 varbinds     = Varbinds}.
	       
check_is_pure_oid([]) -> [];
check_is_pure_oid([X | T]) when is_integer(X), X >=0 ->
    [X | check_is_pure_oid(T)];
check_is_pure_oid([X | _T]) ->
    throw({error, {invalid_oid, X}}).

var_and_value_to_varbind({Oid, Type, Value}) ->
    #varbind{oid = Oid, 
	     variabletype = char_to_type(Type), 
	     value = Value}.

char_to_type(i) ->
    'INTEGER';
char_to_type(u) ->
    'Unsigned32';
char_to_type(g) -> 
    'Unsigned32';
char_to_type(b) -> 
    'BITS';
char_to_type(ia) -> 
    'IpAddress';
char_to_type(op) -> 
    'Opaque';
char_to_type(c32) -> 
    'Counter32';
char_to_type(c64) -> 
    'Counter64';
char_to_type(tt) -> 
    'TimeTicks';
char_to_type(o) ->
    'OBJECT IDENTIFIER';
char_to_type(s) ->
    'OCTET STRING';
char_to_type(C) ->
    throw({error, {invalid_value_type, C}}).

make_vb(Oid) ->
    #varbind{oid = Oid, variabletype = 'NULL', value = 'NULL'}.

msg_data(AgentData) ->
    msg_data(read, AgentData).

msg_data(read, AgentData) ->
    Vsn = get_opt(vsn, AgentData, ?VERSION),
    Comm = get_opt(community, AgentData, ?COMMUNITY),
    {version(Vsn), Comm};

msg_data(write, AgentData) ->
    Vsn = get_opt(vsn, AgentData, ?VERSION),
    Comm = get_opt(writeCommunity, AgentData, ?WRITE_COMMUNITY),
    {version(Vsn), Comm}.

version(v1) ->
    'version-1';

version(v2c) ->
    'version-2';

version(v3) ->
    'version-3'.

%%----------------------------------------------------------------------
maybe_demonitor(undefined) ->
    ok;

maybe_demonitor(MonRef) ->
    erlang:demonitor(MonRef).

%%----------------------------------------------------------------------
call(Req) ->
    gen_server:call(pg2:get_closest_pid(sesnmp_client), Req, 40000).

call(Req, Timeout) ->
    gen_server:call(pg2:get_closest_pid(sesnmp_client), Req, Timeout).

%%----------------------------------------------------------------------
request_id() ->
    sesnmp_server:next_req_id().

insert_req(Req) ->
    sesnmp_server:insert(Req).

lookup_req(ReqId) ->
    sesnmp_server:lookup(ReqId).

delete_req(ReqId) ->
    sesnmp_server:delete(ReqId).

cancel_timer(undefined) ->
    ok;
cancel_timer(Ref) ->
    (catch erlang:cancel_timer(Ref)).
%%----------------------------------------------------------------------
get_opt(Key, Opts) ->
    sesnmp_misc:get_option(Key, Opts).

get_opt(Key, Opts, Def) ->
    sesnmp_misc:get_option(Key, Opts, Def).

