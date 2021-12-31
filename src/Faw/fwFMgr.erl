-module(fwFMgr).

-behavior(gen_srv).

-include("eFaw.hrl").

-export([
   start_link/0
   , newQueue/1
   , delQueue/1
   , chAddW/1
   , chAwkW/1
   , tickCheck/0
   , wSleep/2
   , tWOver/2
]).

-export([
   init/1
   , handleCall/3
   , handleCast/2
   , handleInfo/2
   , terminate/2
   , code_change/3
]).

-define(SERVER, ?MODULE).
-define(TickCheck, 10000).             %% 10秒检车一次各个工厂的状态

newQueue(FName) ->
   gen_srv:call(fwFMgr, {mNewQueue, FName}).

delQueue(FName) ->
   gen_srv:call(fwFMgr, {mDelQueue, FName}).

chAddW(FName) ->
   gen_srv:send(fwFMgr, {mChAddW, FName}).

chAwkW(FName) ->
   gen_srv:send(fwFMgr, {mChAwkW, FName}).

tickCheck() ->
   gen_srv:send(fwFMgr, mTickCheck).

wSleep(FName, Pid) ->
   gen_srv:send(fwFMgr, {mWSleep, FName, Pid}).

tWOver(FName, Pid) ->
   gen_srv:send(fwFMgr, {mTWOver, FName, Pid}).


%% ********************************************  API *******************************************************************
start_link() ->
   gen_srv:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ********************************************  callback **************************************************************
init(_Args) ->
   erlang:send_after(?TickCheck, self(), mTickCheck),
   {ok, #{}}.

handleCall({mNewQueue, FName}, _State, _FROM) ->
   Ret = fwQueue:new(FName),
   {reply, Ret};
handleCall({mDelQueue, FName}, _State, _FROM) ->
   Ret = fwQueue:del(FName),
   {reply, Ret};
handleCall(_Msg, _State, _FROM) ->
   ?FwErr("~p call receive unexpect msg ~p ~n ", [?MODULE, _Msg]),
   {reply, ok}.

%% 默认匹配
handleCast(_Msg, _State) ->
   ?FwErr("~p cast receive unexpect msg ~p ~n ", [?MODULE, _Msg]),
   kpS.

handleInfo({mChAddW, FName}, _State) ->
   WTCnt = FName:getV(?wTCnt),
   case WTCnt > 0 of
      true ->
         WFCnt = FName:getV(?wFCnt),
         Counts = supervisor:count_children(FName),
         {_, WorkerCnt} = lists:keyfind(workers, 1, Counts),
         AddCnt = WTCnt + WFCnt - WorkerCnt,
         case AddCnt > 0 of
            true ->
               %io:format("IMY*****************addddddddd ~p~n", [AddCnt]),
               eFaw:hireW(AddCnt, FName, true);
            _ ->
               ignore
         end;
      _ ->
         ignore
   end,
   kpS;
handleInfo({mChAwkW, FName}, State) ->
   case State of
      #{FName := PidList} ->
         case PidList of
            [OnePid | LeftList] ->
               case erlang:is_process_alive(OnePid) of
                  true ->
                     NewState = State#{FName := LeftList},
                     gen_srv:send(OnePid, mTryWork),
                     {noreply, NewState};
                  _ ->
                     NewState = State#{FName := LeftList},
                     handleInfo({mChAwkW, FName}, NewState)
               end;
            _ ->
               {noreply, State}
         end
   end;
handleInfo(mTickCheck, State) ->
   NewState = tickCheck(State),
   erlang:send_after(?TickCheck, self(), mTickCheck),
   {noreply, NewState};
handleInfo({mWSleep, FName, Pid}, State) ->
   NewState =
      case State of
         #{FName := OldList} ->
            case lists:member(Pid, OldList) of
               true ->
                  State;
               _ ->
                  State#{FName := [Pid | OldList]}
            end;
         _ ->
            State#{FName => [Pid]}
      end,
   {noreply, NewState};
handleInfo({mTWOver, FName, Pid}, State) ->
   supervisor:terminate_child(FName, Pid),
   {noreply, State};
handleInfo(_Msg, _State) ->
   ?FwErr("~p info receive unexpect msg ~p ~n ", [?MODULE, _Msg]),
   kpS.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

tickCheck(State) ->
   tickCheck(maps:iterator(State), State).

tickCheck(Iterator, State) ->
   case maps:next(Iterator) of
      {FName, IdleList, NextIter} ->
         TaskLen = fwQueue:size(FName),
         WFCnt = FName:getV(?wFCnt),
         IdleCnt = length(IdleList),

         TemState =
            if
               IdleCnt > 0 andalso TaskLen >= WFCnt ->
                  [gen_srv:send(OnePid, mTryWork) || OnePid <- IdleList],
                  State#{FName := []};
               TaskLen > 0 andalso WFCnt == IdleCnt ->
                  [gen_srv:send(OnePid, mTryWork) || OnePid <- IdleList],
                  State#{FName := []};
               true ->
                  State
            end,
         tickCheck(NextIter, TemState);
      none ->
         State
   end.

