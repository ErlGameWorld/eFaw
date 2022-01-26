-module(eFaw).

-include("eFaw.hrl").

-export([
   start/0                 %% start app
   , stop/0                %% stop app
   , openF/2               %% Open a factory
   , closeF/1              %% close a factory
   , sizeF/1               %% size a factory
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

-spec inWork(FName :: atom(), Work :: term()) -> true | false.
inWork(FName, Work) ->
   QRef = persistent_term:get(FName),
   FTaskLen = eLfq:size(QRef),
   FTMax = FName:getV(?fTMax),
   FTLfl = FName:getV(?fTLfl),
   WFCnt = FName:getV(?wFCnt),

   if
      FTaskLen > FTMax ->
         %% The factory is overloaded
         false;
      FTaskLen == FTLfl ->
         %% See factory if need to hire hourly worker
         case eLfq:in(QRef, Work) of
            true ->
               fwFMgr:chAddW(FName),
               true;
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

-spec inWorks(FName :: atom(), Works :: [term(), ...]) -> true | false.
inWorks(FName, Works) ->
   QRef = persistent_term:get(FName),
   FTaskLen = eLfq:size(QRef),
   FTMax = FName:getV(?fTMax),
   FTLfl = FName:getV(?fTLfl),
   WFCnt = FName:getV(?wFCnt),
   if
      FTaskLen > FTMax ->
         %% The factory is overloaded.
         false;
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

-spec syncWork(FName :: atom(), RetTag :: atom(), Timeout :: pos_integer() | infinity, Work :: term()) -> true | false.
syncWork(FName, RetTag, Timeout, Work) ->
   QRef = persistent_term:get(FName),
   FTaskLen = eLfq:size(QRef),
   FTMax = FName:getV(?fTMax),
   FTLfl = FName:getV(?fTLfl),
   WFCnt = FName:getV(?wFCnt),
   if
      FTaskLen > FTMax ->
         %% The factory is overloaded.
         false;
      FTaskLen == FTLfl ->
         %% See factory if need to hire hourly worker
         case eLfq:in(QRef, Work) of
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
         case eLfq:in(QRef, Work) of
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
         case eLfq:in(QRef, Work) of
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


