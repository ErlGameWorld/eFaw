-module(fwWTP).

-behavior(gen_srv).

-include("eFaw.hrl").

-export([
	start_link/3
]).

%% worker back
-export([
	work/1
]).

-export([
	init/1
	, handleAfter/2
	, handleCall/3
	, handleCast/2
	, handleInfo/2
	, terminate/2
	, code_change/3
]).

-define(SERVER, ?MODULE).
-record(state, {wParam}).

%% ********************************************  API *******************************************************************
start_link(FName, WorkerName, IsTmp) ->
	gen_srv:start_link({local, WorkerName}, ?MODULE, [FName, IsTmp], []).

%% ********************************************  callback **************************************************************
init([FName, IsTmp]) ->
	erlang:process_flag(trap_exit, true),
	case is_boolean(IsTmp) of
		true ->
			{ok, #state{wParam = fwUtil:initWParam(FName, IsTmp)}, {doAfter, 0}};
		_ ->
			{ok, #state{wParam = fwUtil:initWParam(FName, IsTmp)}}
	end.

handleAfter(0, #state{wParam = WParam} = State) ->
	NewState = fwUtil:tryWorkLoop(WParam, State),
	{noreply, NewState}.

handleCall({mDoWork, Work}, #state{wParam = #wParam{fName = FName, mod = Mod, isTmp = IsTmp}} = _State, FROM) ->
	try Mod:work(Work) of
		Ret ->
			gen_srv:reply(FROM, Ret)
	catch
		C:R:S ->
			gen_srv:reply(FROM, {work_error, {C, R, S}}),
			?FwErr("woker do task error ~p ~p ~p ~p ~p ~n", [FName, Mod, IsTmp, self(), {C, R, S}])
	end,
	kpS;
handleCall(_Msg, _State, _FROM) ->
	?FwErr("~p call receive unexpect msg ~p ~n ", [?MODULE, _Msg]),
	{reply, ok}.

%% 默认匹配
handleCast({mDoWork, Work}, #state{wParam = #wParam{fName = FName, mod = Mod, isTmp = IsTmp}} = _State) ->
	try Mod:work(Work)
	catch
		C:R:S ->
			?FwErr("woker do task error ~p ~p ~p ~p ~p ~n", [FName, Mod, IsTmp, self(), {C, R, S}])
	end,
	kpS;
%% 默认匹配
handleCast(_Msg, _State) ->
	?FwErr("~p cast receive unexpect msg ~p ~n ", [?MODULE, _Msg]),
	kpS.

handleInfo(mTryWork, #state{wParam = WParam} = _State) ->
	fwUtil:tryWorkLoop(WParam),
	kpS;
handleInfo(_Msg, _State) ->
	?FwErr("~p info receive unexpect msg ~p ~n ", [?MODULE, _Msg]),
	kpS.

terminate(_Reason, State) ->
	flush_msg(State),
	ok.

%% 严格的处理所有收到的消息
flush_msg(State) ->
	receive
		{'$gen_call', {From, _}, Msg} ->
			handleCall(Msg, State, From),
			flush_msg(State);
		{'$gen_cast', Msg} ->
			handleCast(Msg, State),
			flush_msg(State);
		Msg ->
			handleInfo(Msg, State),
			flush_msg(State)
	after 0 ->
		ok
	end.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

work(task1) ->
	task1;
work(task2) ->
	task2;
work(_Task) ->
	timer:sleep(1),
	% io:format("work out ~p ~p ~n",[self(), _Task]),
	_Task.