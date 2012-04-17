-module(sesnmp_udp).

-include_lib("elog/include/elog.hrl").

-behaviour(gen_server).

%% Network Interface callback functions
-export([start_link/2, stop/1, send_pdu/5]).

%% gen_server callbacks
-export([init/1,
		handle_call/3,
		handle_cast/2,
		handle_info/2,
		code_change/3,
		terminate/2]).

-include_lib("snmp/include/snmp_types.hrl").

-record(state, {server, sock, mpd_state}).

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------
start_link(Server, NetIfOpts) ->
	gen_server:start_link(?MODULE, [Server, NetIfOpts], []).

stop(Pid) ->
    call(Pid, stop).

send_pdu(Pid, Addr, Port, Pdu, MsgData) when is_record(Pdu, pdu) ->
    cast(Pid, {send_pdu, Addr, Port, Pdu, MsgData}).

%%%-------------------------------------------------------------------
%%% Callback functions from gen_server
%%%-------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init([Server, NetIfOpts]) -> 
    case (catch do_init(Server, NetIfOpts)) of
	{error, Reason} ->
	    {stop, Reason};
	{ok, State} ->
	    {ok, State}
    end.
	    
do_init(Server, NetIfOpts) ->
    process_flag(trap_exit, true),
    %% -- MPD --
    MpdState   = sesnmp_mpd:init([v1, v2c]),
    %% -- Socket --
    Port = get_opt(port, NetIfOpts, 0),
    BindTo  = get_opt(bind_to,  NetIfOpts, any),
    NoReuse = get_opt(no_reuse, NetIfOpts, false),
    SndBuf  = get_opt(sndbuf, NetIfOpts,   default),
    RecBuf  = get_opt(recbuf, NetIfOpts,   default),
    {ok, Sock} = do_open_port(Port, SndBuf, RecBuf, BindTo, NoReuse),
    %%-- We are done ---
    {ok, #state{server = Server, mpd_state  = MpdState, sock = Sock}}.

%% Open port 
do_open_port(Port, SendSz, RecvSz, BindTo, NoReuse) ->
    IpOpts1 = bind_to(BindTo),
    IpOpts2 = no_reuse(NoReuse),
    IpOpts3 = recbuf(RecvSz),
    IpOpts4 = sndbuf(SendSz),
    IpOpts  = [binary | IpOpts1 ++ IpOpts2 ++ IpOpts3 ++ IpOpts4],
    gen_udp:open(Port, IpOpts).

bind_to(any) ->
    [];

bind_to(Addr) when is_list(Addr) ->
    [{ip, list_to_tuple(Addr)}];

bind_to(_) ->
	[].

no_reuse(false) ->
    [{reuseaddr, true}];
no_reuse(_) ->
    [].

recbuf(default) ->
    [];
recbuf(Sz) ->
    [{recbuf, Sz}].

sndbuf(default) ->
    [];
sndbuf(Sz) ->
    [{sndbuf, Sz}].

%%--------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(Req, _From, State) ->
    {stop, {badcall, Req}, State}.

%%--------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast({send_pdu, Addr, Port, Pdu, MsgData}, State) ->
    ?DEBUG("received send_pdu message with"
	  "~n   Pdu:     ~p"
	  "~n   MsgData: ~p"
	  "~n   Addr:    ~p"
	  "~n   Port:    ~p", [Pdu, MsgData, Addr, Port]),
    handle_send_pdu(Addr, Port, Pdu, MsgData, State), 
    {noreply, State};

handle_cast(Msg, State) ->
    {stop, {badcast, Msg}, State}.

%%--------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info({udp, Sock, Ip, Port, Bytes}, #state{sock = Sock} = State) ->
    ?DEBUG("received ~w bytes from ~p:~p", [size(Bytes), Ip, Port]),
    handle_recv_msg(Ip, Port, Bytes, State),
    {noreply, State};

handle_info(Info, State) ->
    {stop, {badinfo, Info}, State}.

%%--------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------
code_change(_Vsn, State, _Extra) ->
    {ok, State}.
 
%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
handle_recv_msg(Addr, Port, Bytes, #state{server = Pid}) 
  when is_binary(Bytes) and (size(Bytes) == 0) ->
    Pid ! {snmp_error, {empty_message, Addr, Port}, Addr, Port},
    ok;

handle_recv_msg(Addr, Port, Bytes, 
   #state{server = Pid, 
        mpd_state  = MpdState, 
	    sock       = _Sock}) ->
    case (catch sesnmp_mpd:process_msg(Bytes, Addr, Port, MpdState)) of
	%% BMK BMK BMK
	%% Do we really need message size here??
	{ok, _Vsn, #pdu{type = 'snmpv2-trap'} = Pdu, _MS, _ACM} ->
	    Pid ! {snmp_trap, Pdu, Addr, Port};

	{ok, _Vsn, Trap, _MS, _ACM} when is_record(Trap, trappdu) ->
	    Pid ! {snmp_trap, Trap, Addr, Port};

	{ok, _Vsn, Pdu, _MS, _ACM} when is_record(Pdu, pdu) ->
	    Pid ! {snmp_pdu, Pdu, Addr, Port};

	{discarded, Reason} ->
	    ?DEBUG("discarded: ~p", [Reason]),
	    ErrorInfo = {failed_processing_message, Reason},
	    Pid ! {snmp_error, ErrorInfo, Addr, Port},
	    ok;
	Error ->
	    ?ERROR("processing of received message failed: ~n ~p", [Error]),
	    ok
    end.

handle_send_pdu(Addr, Port, Pdu, MsgData,  
		#state{server = Pid, sock = Sock}) ->
    case (catch sesnmp_mpd:generate_msg(Pdu, MsgData)) of
	{ok, Msg} ->
	    ?DEBUG_MSG("handle_send_pdu -> message generated"),
	    udp_send(Sock, Addr, Port, Msg);	    
	{discarded, Reason} ->
	    ?ERROR("PDU not sent: "
		  "~n   PDU:    ~p"
		  "~n   Reason: ~p", [Pdu, Reason]),
	    Pid ! {snmp_error, Pdu, Reason},
	    ok
    end.

udp_send(Sock, Addr, Port, Msg) ->
    case (catch gen_udp:send(Sock, Addr, Port, Msg)) of
	ok ->
	    ?DEBUG("sent ~w bytes to ~w:~w [~w]", [sz(Msg), Addr, Port, Sock]),
	    ok;
	{error, Reason} ->
	    ?ERROR("failed sending message to ~p:~p: "
		      "~n   ~p",[Addr, Port, Reason]);
	Error ->
	    ?ERROR("failed sending message to ~p:~p: "
		      "~n   ~p",[Addr, Port, Error])
    end.

sz(B) when is_binary(B) ->
    size(B);
sz(L) when is_list(L) ->
    length(L);
sz(_) ->
    undefined.

%%%-------------------------------------------------------------------
get_opt(Key, Opts, Def) ->
    sesnmp_misc:get_option(Key, Opts, Def).

call(Pid, Req) ->
    call(Pid, Req, infinity).

call(Pid, Req, Timeout) ->
    gen_server:call(Pid, Req, Timeout).

cast(Pid, Msg) ->
    gen_server:cast(Pid, Msg).

