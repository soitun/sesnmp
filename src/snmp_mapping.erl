%%%----------------------------------------------------------------------
%%% File    : snmp_mapping.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : Mapping snmp varbinds to erlang lists.
%%% Created : 3 Mar 2008
%%% License : http://www.opengoss.com/license
%%%
%%% Copyright (C) 2007-2008, www.opengoss.com 
%%%----------------------------------------------------------------------
-module(snmp_mapping).

-author('ery.lee@gmail.com').

-include_lib("snmp/include/snmp_types.hrl").

-export([discovery/2, 
        discovery/3,
        get_group/2, 
        get_group/3, 
        get_table/2, 
        get_table/3, 
        get_table/4,
        get_entry/3,
        get_entry/4,
        set_group/3
    ]).

discovery(Addr, Scalars) ->
    get_group(Addr, Scalars).

discovery(Addr, Scalars, AgentData) ->
    get_group(Addr, Scalars, AgentData).

get_group(Addr, Scalars) ->
    sesnmp:get_group(Addr, Scalars).
    
get_group(Addr, Scalars, AgentData) ->
    sesnmp:get_group(Addr, Scalars, AgentData).

get_table(Addr, Columns) ->
    sesnmp:get_table(Addr, Columns).

get_table(Addr, Columns, AgentData) ->
    sesnmp:get_table(Addr, Columns, AgentData).

get_table(Addr, Columns, AgentData, TIMEOUT) ->
    sesnmp:get_table(Addr, Columns, AgentData, TIMEOUT).

get_entry(Addr, Columns, Indices) ->
    sesnmp:get_entry(Addr, Columns, Indices).

get_entry(Addr, Columns, Indices, AgentData) ->
    sesnmp:get_entry(Addr, Columns, Indices, AgentData).

set_group(Addr, Scalars, AgentData) ->
    sesnmp:set(Addr, Scalars, AgentData).
