-module(eFaw).

-include("eFaw.hrl").

-export([
   start/0                 %% start app
   , stop/0                %% stop app
   , openF/2               %% Open a factory
   , closeF/1              %% close a factory
   , hireW/3               %% hire worker
   , inWork/2              %% Insert async task
   , inWorks/2             %% Insert async tasks
   , syncWork/4            %% Insert sync task And wait for the result to receive
]).

start() ->
   application:ensure_all_started(eFaw).

stop() ->
   application:stop(eFaw).

openF(FName, Kvs) ->
   fwKvsToBeam:load(FName, fwUtil:initCfg(Kvs)),
   FChildSpec = #{
      id => FName,
      start => {fwWSup, start_link, [FName, FName:getV(?wMod)]},
      restart => permanent,
      shutdown => 300000,
      type => supervisor,
      modules => [fwWSup]
   },
   case supervisor:start_child(eFaw_sup, FChildSpec) of
      {ok, _Pid} = Ret ->
         io:format("IMY************** ~p~n", [Ret]),
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

-spec inWork(FName :: atom(), Work :: term()) -> true | false.
inWork(FName, Work) ->
   % fwQueue:in(FName, Work).
   FTaskLen = fwQueue:size(FName),
   FTMax = FName:getV(?fTMax),
   FTLfl = FName:getV(?fTLfl),
   WFCnt = FName:getV(?wFCnt),
   if
      FTaskLen > FTMax ->
         %% The factory is overloaded
         false;
      FTaskLen == FTLfl ->
         %% See factory if need to hire hourly worker
         %io:format("IMY*****************try addddddddd~n"),
         fwQueue:in(FName, Work),
         fwFMgr:chAddW(FName),
         true;
      FTaskLen < WFCnt ->
         %% See if need to wake up idle workers
         fwQueue:in(FName, Work),
         fwFMgr:chAwkW(FName),
         true;
      true ->
         fwQueue:in(FName, Work)
   end.

-spec inWorks(FName :: atom(), Works :: [term(), ...]) -> true | false.
inWorks(FName, Works) ->
   FTaskLen = fwQueue:size(FName),
   FTMax = FName:getV(?fTMax),
   FTLfl = FName:getV(?fTLfl),
   WFCnt = FName:getV(?wFCnt),
   if
      FTaskLen > FTMax ->
         %% The factory is overloaded.
         false;
      FTaskLen == FTLfl ->
         %% See factory if need to hire hourly worker
         fwQueue:ins(FName, Works),
         fwFMgr:chAddW(FName),
         true;
      FTaskLen < WFCnt ->
         %% See if need to wake up idle workers
         fwQueue:ins(FName, Works),
         fwFMgr:chAwkW(FName),
         true;
      true ->
         fwQueue:ins(FName, Works)
   end.

-spec syncWork(FName :: atom(), RetTag :: atom(), Timeout :: pos_integer() | infinity, Work :: term()) -> true | false.
syncWork(FName, RetTag, Timeout, Work) ->
   FTaskLen = fwQueue:size(FName),
   FTMax = FName:getV(?fTMax),
   FTLfl = FName:getV(?fTLfl),
   WFCnt = FName:getV(?wFCnt),
   if
      FTaskLen > FTMax ->
         %% The factory is overloaded.
         false;
      FTaskLen == FTLfl ->
         %% See factory if need to hire hourly worker
         fwQueue:in(FName, Work),
         fwFMgr:chAddW(FName),
         receive
            {RetTag, Ret} ->
               Ret
         after Timeout ->
            timeout
         end;
      FTaskLen < WFCnt ->
         %% See if need to wake up idle workers
         fwQueue:in(FName, Work),
         fwFMgr:chAwkW(FName),
         receive
            {RetTag, Ret} ->
               Ret
         after Timeout ->
            timeout
         end;
      true ->
         fwQueue:in(FName, Work),
         receive
            {RetTag, Ret} ->
               Ret
         after Timeout ->
            timeout
         end
   end.


