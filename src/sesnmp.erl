-module(sesnmp).

-export([get_group/2, get_group/3, get_group/4, get_group/5,
         get_table/2, get_table/3, get_table/4,
         get_entry/3, get_entry/4, get_entry/5,
         set/3, set/4
     ]).

-include_lib("snmp/include/snmp_types.hrl").

-define(PORT, 161).

-define(RETRIES, 2).

-define(TIMEOUT, 20000).

-define(MAX_LIMIT, 9999).

%%Scalars = [{Name, Oid}]
get_group(Addr, Scalars) ->
    get_group(Addr, Scalars, []).

get_group(Addr, Port, Scalars) when is_integer(Port) and is_list(Scalars) ->
    get_group(Addr, Port, Scalars, []);

get_group(Addr, Scalars, AgentData) when is_list(Scalars) and is_list(AgentData) ->
    get_group(Addr, ?PORT, Scalars, AgentData).

get_group(Addr, Port, Scalars, AgentData) ->
    get_group(Addr, Port, Scalars, AgentData, ?TIMEOUT).

get_group(Addr, Port, Scalars, AgentData, Timeout) ->
	{Names, Oids} = split_vars(Scalars),
	case retry(fun() -> sesnmp_client:get(Addr, Port, Oids, AgentData, Timeout) end, ?RETRIES) of
	{ok, {noError, _, Varbinds}, _} -> 
		{ok, merge_vars(Names, Varbinds)};
    {ok, Error, _} ->
        {error, Error};
	{error, Error} -> 
		{error, Error}
	end.

%%VarVals = [{name, oid, type, val}]
set(Addr, VarVals, AgentData) ->
    set(Addr, ?PORT, VarVals, AgentData).

set(Addr, Port, VarVals, AgentData) ->
    {Names, VarsAndVals} = split_var_vals(VarVals),
    case retry(fun() -> sesnmp_client:set(Addr, Port, VarsAndVals, AgentData, ?TIMEOUT) end, ?RETRIES) of
    {ok, {noError, _, Varbinds}, _} ->
		{ok, merge_vars(Names, Varbinds)}; %TODO
    {ok, Error, _} ->
        {error, Error};
	{error, Error} ->
		{error, Error}
	end.
    
get_table(Addr, Columns) ->
    get_table(Addr, Columns, []).

get_table(Addr, Columns, AgentData) ->
    get_table(Addr, ?PORT, Columns, AgentData, ?TIMEOUT).

get_table(Addr, Port, Columns, AgentData)
    when is_integer(Port) and is_list(Columns) ->
    get_table(Addr, Port, Columns, AgentData, ?TIMEOUT);

get_table(Addr, Columns, AgentData, Timeout) 
    when is_list(Columns) and is_integer(Timeout) ->
    get_table(Addr, ?PORT, Columns, AgentData, Timeout).

get_table(Addr, Port, Columns, AgentData, TIMEOUT) ->
	[{_, Col1Oid} | _] = Columns,
	case get_table(Addr, Port, Col1Oid, Columns, AgentData, TIMEOUT, []) of
	{ok, Rows} ->
		{ok, lists:reverse(Rows)};
	{error, Error} ->
		{error, Error}
	end.

get_table(Addr, Port, Col1Oid, Columns, AgentData, TIMEOUT, Acc) ->
    {value, Limit} = dataset:get_value(limit, AgentData, ?MAX_LIMIT),
    case length(Acc) > Limit of
    true ->
        {ok, Acc};
    false ->
        {Names, Oids} = split_vars(Columns),
        case retry(fun() -> sesnmp_client:get_next(Addr, Port, Oids, AgentData, TIMEOUT) end, ?RETRIES) of
        {ok, {noError, _, Varbinds}, _} -> 
            #varbind{oid=Oid} = lists:nth(1, Varbinds),
            case start_with_oid(Col1Oid, Oid) of
            true ->
                NewOids = lists:map(fun(Varbind) -> Varbind#varbind.oid end, Varbinds),
                NewColumns = to_name_oid_map(Names, NewOids),
				Row = [{'$tableIndex', Oid -- Col1Oid} | merge_vars(Names, Varbinds)],
				Row1 = 
				case proplists:get_value(timestamp, AgentData) of
				true -> [{'$timestamp', extbif:timestamp()} | Row];
				undefined -> Row
				end,
                get_table(Addr, Port, Col1Oid, NewColumns, AgentData, TIMEOUT, [Row1 | Acc]);
            false ->
                {ok, Acc}
            end;
        {ok, Error, _} ->
            {error, Error};
        {error, Error} -> 
            {error, Error}
        end
    end.

get_entry(Addr, Columns, Indices) ->
    get_entry(Addr, Columns, Indices, []). 

get_entry(Addr, Columns, Indices, AgentData) ->
    get_entry(Addr, ?PORT, Columns, Indices, AgentData).

get_entry(Addr, Port, Columns, Indices, AgentData) ->
	{Names, Oids} = split_vars(Columns),
	Oids1 = [lists:append(Oid, Indices) || Oid <- Oids],
    case retry(fun() -> sesnmp_client:get(Addr, Port, Oids1, AgentData, ?TIMEOUT) end, ?RETRIES) of
	{ok, {noError, _, Varbinds}, _} -> 
		{ok, merge_vars(Names, Varbinds)};
    {ok, Error, _} ->
        {error, Error};
	{error, Error} -> 
		{error, Error}
	end.

%%-------------------------------------------------------
%% Retry
%%-------------------------------------------------------
retry(Fun, 0) ->
    Fun();

retry(Fun, R) when R < 0 ->
    Fun();

retry(Fun, R) ->
    case Fun() of
    {error, {timeout, _}} -> 
        retry(Fun, R - 1);
    {error, timeout} -> 
        retry(Fun, R - 1);
    Result -> 
        Result
    end.

%%Internale functions.
split_vars(Vars) ->
	split_vars(Vars, [], []).

split_vars([{Name, Oid}|T], Names, Oids) ->
	split_vars(T, [Name|Names], [Oid|Oids]);
split_vars([], Names, Oids) ->
	{lists:reverse(Names), lists:reverse(Oids)}.

split_var_vals(VarList) ->
    split_var_vals(VarList, [], []).

split_var_vals([{Name, Oid, Type, Val}|T], Names, VarsAndVals) ->
    split_var_vals(T, [Name|Names], [{Oid, Type, Val}|VarsAndVals]);
split_var_vals([], Names, VarsAndVals) ->
    {lists:reverse(Names), lists:reverse(VarsAndVals)}.
	
merge_vars(Names, Varbinds) ->
	merge_vars(Names, Varbinds, []).

merge_vars([Name|T1], [Varbind|T2], Acc) ->
	merge_vars(T1, T2, [{Name, Varbind#varbind.value}|Acc]);

merge_vars([], [], Acc) ->
	lists:reverse(Acc).

to_name_oid_map(Names, Oids) ->
	to_name_oid_map(Names, Oids, []).

to_name_oid_map([Name|T1], [Oid|T2], Acc) ->
	to_name_oid_map(T1, T2, [{Name, Oid} | Acc]);

to_name_oid_map([], [], Acc) ->
	lists:reverse(Acc).

%%Oid2 is the oid with index.
start_with_oid(Oid1, Oid2) ->
	if 
	length(Oid2) > length(Oid1) -> 
		{Oid3, _} = lists:split(length(Oid1), Oid2),
		Oid1 == Oid3;
	true ->
		false
	end.

