-module(fwFMgr).

-behavior(gen_srv).

-include("eFaw.hrl").

-export([
   start_link/0
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

%% ********************************************  API *******************************************************************
start_link() ->
   ut_gen_srv:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ********************************************  callback **************************************************************
init(_Args) ->
   {ok, #{}}.

handleCall(_Msg, _State, _FROM) ->
   ?ERR("~p call receive unexpect msg ~p ~n ", [?MODULE, _Msg]),
   {reply, ok}.

%% 默认匹配
handleCast(_Msg, _State) ->
   ?ERR("~p cast receive unexpect msg ~p ~n ", [?MODULE, _Msg]),
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
               hireW(AddCnt, FName, true);
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
               NewState = State#{FName := LeftList},
               gen_srv:send(OnePid, tryWork),
               {noreply, NewState};
            _ ->
               kpS
         end
   end;
handleInfo({mWSleep, FName, Pid}, State) ->
   NewState =
      case State of
         #{FName := OldList} ->
            State#{FName := [Pid | OldList]};
         _ ->
            State#{FName => [Pid]}
      end,
   {noreply, NewState};
handleInfo(_Msg, _State) ->
   ?ERR("~p info receive unexpect msg ~p ~n ", [?MODULE, _Msg]),
   kpS.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.
