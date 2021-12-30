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

initCfg(Kvs) ->
   [
      begin
         case lists:keyfind(Key, 1, Kvs) of
            false ->
               {Key, DefV};
            Tuple ->
               Tuple
         end
      end
      || {Key, DefV} <- ?FawDefV
   ].


openF(FName, Kvs) ->
   FChildSpec = #{
      id => FName,
      start => {fwWSup, start_link, [FName]},
      restart => permanent,
      shutdown => 300000,
      type => supervisor,
      modules => [fwWSup]
   },
   Ret = supervisor:start_child(eFaw_sup, FChildSpec),
   fwKvsToBeam:load(FName, initCfg(Kvs)),
   hireW(FName:getV(?wFCnt), FName, false),
   Ret.

hireW(WorkerNum, FName, IsTmp) when is_integer(WorkerNum), WorkerNum > 0 ->
   supervisor:start_child(FName, [IsTmp]),
   hireW(WorkerNum - 1, FName, IsTmp);
hireW(_WorkerNum, _FName, _IsTmp) ->
   ok.

closeF(FName) ->
   supervisor:terminate_child(eFaw_sup, FName).

-spec inWork(FName :: atom(), Work :: term()) -> true | false.
inWork(FName, Work) ->
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
         gen_srv:send(fwFMgr, mChAddW),
         fwQueue:in(FName, Work);
      FTaskLen < WFCnt ->
         %% See if need to wake up idle workers
         gen_srv:send(fwFMgr, mChAwkW),
         fwQueue:in(FName, Work);
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
         gen_srv:send(fwFMgr, mChAddW),
         fwQueue:ins(FName, Works);
      FTaskLen < WFCnt ->
         %% See if need to wake up idle workers
         gen_srv:send(fwFMgr, mChAwkW),
         fwQueue:ins(FName, Works);
      true ->
         fwQueue:ins(FName, Works)
   end.

-spec syncWork(FName :: atom(), Work :: term()) -> true | false.
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
         gen_srv:send(fwFMgr, mChAddW),
         fwQueue:in(FName, Work),
         receive
            {RetTag, Ret} ->
               Ret
            after Timeout ->
               timeout
         end;
      FTaskLen < WFCnt ->
         %% See if need to wake up idle workers
         gen_srv:send(fwFMgr, mChAwkW),
         fwQueue:in(FName, Work),
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


