-module(eFaw).

-include("eFaw.hrl").

-export([
	start/0                 %% start app
	, stop/0                %% stop app
]).

%% worker model
-export([
	openW/2                 %% Open a factory
	, closeW/1              %% close a factory
	, doWork/2              %% Insert async task
	, doWork/3              %% Insert async task
	, doSyncWork/2          %% Insert sync task And wait for the result to receive
	, doSyncWork/3          %% Insert sync task And wait for the result to receive
	, doSyncWork/4          %% Insert sync task And wait for the result to receive
]).

%% factory model
-export([
	openF/2                 %% Open a factory
	, closeF/1              %% close a factory
	, sizeF/1               %% size a factory
	, hireW/3               %% hire worker
	, inWork/2              %% Insert async task
	, inWorks/2             %% Insert async tasks
	, inSyncWork/2          %% Insert sync task And wait for the result to receive
	, inSyncWork/3          %% Insert sync task And wait for the result to receive
]).

start() ->
	application:ensure_all_started(eFaw).

stop() ->
	application:stop(eFaw).

openW(WName, Kvs) ->
	CfgKvs = fwUtil:initCfg(Kvs),
	fwKvsToBeam:load(WName, CfgKvs),
	WFCnt = WName:getV(?wFCnt),
	?IIF(WFCnt > 0, ok, throw({error, <<"workrt cnt need > 0">>})),
	FChildSpec = #{
		id => WName,
		start => {fwWSup, start_link, [WName, WName:getV(?wMod)]},
		restart => permanent,
		shutdown => infinity,
		type => supervisor,
		modules => [fwWSup]
	},
	case supervisor:start_child(eFaw_sup, FChildSpec) of
		{ok, _Pid} = Ret ->
			NameList = [{Idx, workerName(Idx)} || Idx <- lists:seq(1, WFCnt)],
			[supervisor:start_child(WName, [WorkName, worker]) || {_Idx, WorkName} <- NameList],
			fwKvsToBeam:load(WName, [CfgKvs | NameList]),
			Ret;
		ErrRet ->
			?FwErr("open factory error ~p~n", [ErrRet]),
			ErrRet
	end.

workerName(Idx) ->
	binary_to_atom(<<"$fawWork_", (integer_to_binary(Idx))/binary>>).

closeW(WName) ->
	supervisor:terminate_child(eFaw_sup, WName),
	supervisor:delete_child(eFaw_sup, WName).

doWork(WName, Work) ->
	doWork(WName, Work, bind).
-spec doWork(WName :: atom(), Work :: term(), Strategy :: rand | bind) -> overflow | true.
doWork(WName, Work, Strategy) ->
	FTMax = WName:getV(?fTMax),
	WFCnt = WName:getV(?wFCnt),
	Idx = ?IIF(Strategy == rand, rand:uniform(WFCnt), erlang:phash2(self(), WFCnt) + 1),
	WorkerName = WName:getV(Idx),
	case FTMax of
		infinity ->
			gen_srv:cast(WorkerName, {mDoWork, Work}),
			ok;
		_ ->
			{_, MsgLen} = process_info(whereis(WorkerName), message_queue_len),
			case MsgLen >= FTMax of
				true ->
					%% The factory is overloaded
					overflow;
				_ ->
					gen_srv:cast(WorkerName, {mDoWork, Work}),
					ok
			end
	end.

doSyncWork(WName, Work) ->
	doSyncWork(WName, Work, bind, ?WAIT_TIME).
doSyncWork(WName, Work, Strategy) ->
	doSyncWork(WName, Work, Strategy, ?WAIT_TIME).
-spec doSyncWork(WName :: atom(), Work :: term(), Strategy :: rand | bind, Timeout :: integer() | infinity) -> overflow | term().
doSyncWork(WName, Work, Strategy, Timeout) ->
	FTMax = WName:getV(?fTMax),
	WFCnt = WName:getV(?wFCnt),
	Idx = ?IIF(Strategy == rand, rand:uniform(WFCnt), erlang:phash2(self(), WFCnt) + 1),
	WorkerName = WName:getV(Idx),
	case FTMax of
		infinity ->
			gen_srv:call(WorkerName, {mDoWork, Work}, Timeout);
		_ ->
			{_, MsgLen} = process_info(whereis(WorkerName), message_queue_len),
			case MsgLen >= FTMax of
				true ->
					%% The factory is overloaded
					overflow;
				_ ->
					gen_srv:call(WorkerName, {mDoWork, Work}, Timeout)
			end
	end.

openF(FName, Kvs) ->
	fwKvsToBeam:load(FName, fwUtil:initCfg(Kvs)),
	case erlang:whereis(fwFMgr) of
		undefined ->
			FMgrSpec = #{
				id => fwFMgr,
				start => {fwFMgr, start_link, []},
				restart => permanent,
				shutdown => 3000,
				type => worker,
				modules => [fwFMgr]
			},
			case supervisor:start_child(eFaw_sup, FMgrSpec) of
				{ok, _FMgrPid} ->
					ok;
				FMgrErrRet ->
					?FwErr("open factory mgr error ~p~n", [FMgrErrRet]),
					FMgrErrRet
			end;
		_ ->
			ignore
	end,
	
	FChildSpec = #{
		id => FName,
		start => {fwWSup, start_link, [FName, FName:getV(?wMod)]},
		restart => permanent,
		shutdown => infinity,
		type => supervisor,
		modules => [fwWSup]
	},
	case supervisor:start_child(eFaw_sup, FChildSpec) of
		{ok, _Pid} = Ret ->
			fwFMgr:newQueue(FName),
			hireW(FName:getV(?wFCnt), FName, false),
			Ret;
		ErrRet ->
			?FwErr("open factory error ~p~n", [ErrRet]),
			ErrRet
	end.

hireW(WorkerNum, FName, IsTmp) when is_integer(WorkerNum), WorkerNum > 0 ->
	case supervisor:start_child(FName, [IsTmp]) of
		{ok, _Pid} ->
			ignore;
		ErrRet ->
			?FwErr("hire worker error ~p~n", [ErrRet])
	end,
	hireW(WorkerNum - 1, FName, IsTmp);
hireW(_WorkerNum, _FName, _IsTmp) ->
	ok.

closeF(FName) ->
	supervisor:terminate_child(eFaw_sup, FName),
	supervisor:delete_child(eFaw_sup, FName),
	fwFMgr:delQueue(FName).

sizeF(FName) ->
	QRef = persistent_term:get(FName),
	eLfq:size(QRef).

-spec inWork(FName :: atom(), Work :: term()) -> overflow | true | false.
inWork(FName, Work) ->
	QRef = persistent_term:get(FName),
	FTaskLen = eLfq:size(QRef),
	FTMax = FName:getV(?fTMax),
	FTLfl = FName:getV(?fTLfl),
	WFCnt = FName:getV(?wFCnt),
	
	if
		FTaskLen > FTMax ->
			%% The factory is overloaded
			overflow;
		FTaskLen == FTLfl ->
			%% See factory if need to hire hourly worker
			case eLfq:in(QRef, Work) of
				true ->
					fwFMgr:chAddW(FName),
					ok;
				_ ->
					false
			end;
		FTaskLen < WFCnt ->
			%% See if need to wake up idle workers
			case eLfq:in(QRef, Work) of
				true ->
					fwFMgr:chAwkW(FName),
					true;
				_ ->
					false
			end;
		true ->
			eLfq:in(QRef, Work)
	end.

-spec inWorks(FName :: atom(), Works :: [term(), ...]) -> overflow | true | false.
inWorks(FName, Works) ->
	QRef = persistent_term:get(FName),
	FTaskLen = eLfq:size(QRef),
	FTMax = FName:getV(?fTMax),
	FTLfl = FName:getV(?fTLfl),
	WFCnt = FName:getV(?wFCnt),
	if
		FTaskLen > FTMax ->
			%% The factory is overloaded.
			overflow;
		FTaskLen == FTLfl ->
			%% See factory if need to hire hourly worker
			case eLfq:ins(QRef, Works) of
				true ->
					fwFMgr:chAddW(FName),
					true;
				_ ->
					false
			end;
		FTaskLen < WFCnt ->
			%% See if need to wake up idle workers
			case eLfq:ins(QRef, Works) of
				true ->
					fwFMgr:chAwkW(FName);
				_ ->
					false
			end;
		true ->
			eLfq:ins(QRef, Works)
	end.


inSyncWork(FName, Work) ->
	inSyncWork(FName, Work, ?WAIT_TIME).
-spec inSyncWork(FName :: atom(), Work :: term(), Timeout :: pos_integer() | infinity) -> overflow | true | false.
inSyncWork(FName, Work, Timeout) ->
	QRef = persistent_term:get(FName),
	FTaskLen = eLfq:size(QRef),
	FTMax = FName:getV(?fTMax),
	FTLfl = FName:getV(?fTLfl),
	WFCnt = FName:getV(?wFCnt),
	RetTag = erlang:make_ref(),
	if
		FTaskLen > FTMax ->
			%% The factory is overloaded.
			overflow;
		FTaskLen == FTLfl ->
			%% See factory if need to hire hourly worker
			case eLfq:in(QRef, {'$SyncWork', RetTag, Work}) of
				true ->
					fwFMgr:chAddW(FName),
					receive
						{RetTag, Ret} ->
							Ret
					after Timeout ->
						timeout
					end;
				_ ->
					false
			end;
		FTaskLen < WFCnt ->
			%% See if need to wake up idle workers
			case eLfq:in(QRef, {'$SyncWork', RetTag, Work}) of
				true ->
					fwFMgr:chAwkW(FName),
					receive
						{RetTag, Ret} ->
							Ret
					after Timeout ->
						timeout
					end;
				_ ->
					false
			end;
		true ->
			case eLfq:in(QRef, {'$SyncWork', RetTag, Work}) of
				true ->
					receive
						{RetTag, Ret} ->
							Ret
					after Timeout ->
						timeout
					end;
				_ ->
					false
			end
	end.


