-module(sesnmp_mpd).

-export([init/1,
		process_msg/4,
		generate_msg/2]).

-include_lib("elog/include/elog.hrl").

-include_lib("snmp/include/snmp_types.hrl").

-define(empty_msg_size, 24).

-record(state, {v1 = true, v2c = true, v3 = false}).
					
%%%-----------------------------------------------------------------
%%% This module implemets the Message Processing and Dispatch part of
%%% the multi-lingual SNMP agent.
%%%
%%% The MPD is responsible for:
%%%   *) call the security module (auth/priv).
%%%   *) decoding the message into a PDU.
%%%   *) decide a suitable Access Control Model, and provide it with
%%%      the data it needs.
%%%   *) maintaining SNMP counters.
%%%
%%% In order to take care of the different versions of counters, it
%%% implements and maintains the union of all SNMP counters (i.e. from
%%% rfc1213 and from rfc1907).  It is up to the administrator of the
%%% agent to load the correct MIB.  Note that this module implements
%%% the counters only, it does not provide instrumentation functions
%%% for the counters.
%%%
%%% With the terms defined in rfc2271, this module implememts part
%%% of the Dispatcher and the Message Processing functionality.
%%%-----------------------------------------------------------------
init(Vsns) ->
    State = init_versions(Vsns, #state{}),
    State.

%%-----------------------------------------------------------------
%% Func: process_msg(Packet, TDomain, TAddress, State) ->
%%       {ok, SnmpVsn, Pdu, PduMS, ACMData} | {discarded, Reason}
%% Types: Packet = binary()
%%        TDomain = snmpUDPDomain | atom()
%%        TAddress = {Ip, Udp}
%%        State = #state
%% Purpose: This is the main Message Dispatching function. (see
%%          section 4.2.1 in rfc2272)
%%-----------------------------------------------------------------
process_msg(Msg, Addr, Port, State) ->

    case (catch sesnmp_pdus:dec_message_only(binary_to_list(Msg))) of
	%% Version 1
	#message{version = 'version-1', community = Community, data = Data} 
	  when State#state.v1 =:= true ->
	    HS = ?empty_msg_size + length(Community),
	    process_v1_v2c_msg('version-1', Msg, Addr, Port, Community, Data, HS);

	%% Version 2
	#message{version = 'version-2', community = Community, data = Data}
	  when State#state.v2c =:= true ->
	    HS = ?empty_msg_size + length(Community),
	    process_v1_v2c_msg('version-2', Msg, Addr, Port, Community, Data, HS);

	%% Crap
	{'EXIT', {bad_version, Vsn}} ->
	    ?INFO("exit: bad version: ~p",[Vsn]),
	    {discarded, snmpInBadVersions};

	%% More crap
	{'EXIT', Reason} ->
	    ?INFO("exit: ~p",[Reason]),
	    {discarded, Reason};

	%% Really crap
	Crap ->
	    ?INFO("unknown message: ~n ~p",[Crap]),
	    {discarded, snmpInBadVersions}
    end.


%%-----------------------------------------------------------------
%% Handles a Community based message (v1 or v2c).
%%-----------------------------------------------------------------
process_v1_v2c_msg(Vsn, _Msg, Addr, Port, Community, Data, HS) ->
    ?DEBUG("process_v1_v2c_msg -> entry with"
	    "~n   Vsn:       ~p"
	    "~n   Addr:      ~p"
	    "~n   Port:      ~p"
	    "~n   Community: ~p"
	    "~n   HS:        ~p", [Vsn, Addr, Port, Community, HS]),
    
    Max      = get_max_message_size(),
    AgentMax = Max,
    PduMS    = pdu_ms(Max, AgentMax, HS),

    ?DEBUG("process_v1_v2c_msg -> PduMS: ~p", [PduMS]),
    
    case (catch sesnmp_pdus:dec_pdu(Data)) of
	Pdu when is_record(Pdu, pdu) ->
	    ?DEBUG_MSG("process_v1_v2c_msg -> was a pdu"),
	    MsgData = {Community, sec_model(Vsn)},
	    {ok, Vsn, Pdu, PduMS, MsgData};

	Trap when is_record(Trap, trappdu) ->
	    ?DEBUG_MSG("process_v1_v2c_msg -> was a trap"),
	    MsgData = {Community, sec_model(Vsn)},
	    {ok, Vsn, Trap, PduMS, MsgData};

	{'EXIT', Reason} ->
	    %?WARNING("process_v1_v2c_msg -> failed decoding PDU: "
		%  "~n   Reason: ~p", [Reason]),
	    {discarded, Reason}
    end.

pdu_ms(MgrMMS, AgentMMS, HS) when AgentMMS < MgrMMS ->
    AgentMMS - HS;
pdu_ms(MgrMMS, _AgentMMS, HS) ->
    MgrMMS - HS.

sec_model('version-1') -> ?SEC_V1;
sec_model('version-2') -> ?SEC_V2C;
sec_model('version-3') -> ?SEC_USM.

%%-----------------------------------------------------------------
%% Generate a message
%%-----------------------------------------------------------------
generate_msg(Pdu, {Vsn, Community}) ->
    generate_v1_v2c_msg(Vsn, Pdu, Community).

generate_v1_v2c_msg(Vsn, Pdu, Community) ->
    case (catch sesnmp_pdus:enc_pdu(Pdu)) of
	{'EXIT', Reason} ->
	    %?ERROR("failed encoding pdu: (pdu: ~w, community: ~w): ~n~w", [Pdu, Community, Reason]),
	    {discarded, Reason};
	PduBytes ->
	    MMS  = get_max_message_size(),
	    Message = #message{version = Vsn, 
			       community = Community, 
			       data    = PduBytes},
	    case generate_v1_v2c_outgoing_msg(Message) of
		{error, Reason} ->
		    {discarded, Reason};
		{ok, Packet} when size(Packet) =< MMS ->
            {ok, Packet};
		{ok, Packet} ->
            ?ERROR("packet is toobig: ~p, ~p, nms : ~p", [Packet, size(Packet), MMS]),
		    {discarded, too_big}
	    end
    end.

generate_v1_v2c_outgoing_msg(Message) ->
    case (catch sesnmp_pdus:enc_message_only(Message)) of
	{'EXIT', Reason} ->
	    {error, Reason};
	Bin when is_binary(Bin) ->
	    {ok, Bin};
	Packet when is_list(Packet) ->
	    case (catch list_to_binary(Packet)) of
		Bin when is_binary(Bin) ->
		    {ok, Bin};
		{'EXIT', Reason} ->
		    {error, Reason}
	    end
    end.

get_max_message_size() ->
    %%Notice: 
    20480.

%%-----------------------------------------------------------------
%% Version(s) functions
%%-----------------------------------------------------------------
init_versions([], S) ->
    S;
init_versions([v1|Vsns], S) ->
    init_versions(Vsns, S#state{v1 = true});
init_versions([v2c|Vsns], S) ->
    init_versions(Vsns, S#state{v2c = true});
init_versions([v3|Vsns], S) ->
    init_versions(Vsns, S#state{v3 = true}).

