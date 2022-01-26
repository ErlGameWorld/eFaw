-module(fwWTP).

-behavior(gen_srv).

-include("eFaw.hrl").

-export([
   start_link/2
]).

%% worker back
-export([
   idle/1
   , work/2
   , close/1
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
start_link(FName, IsTmp) ->
   gen_srv:start_link(?MODULE, [FName, IsTmp], []).

%% ********************************************  callback **************************************************************
init([FName, IsTmp]) ->
   erlang:process_flag(trap_exit, true),
   {ok, #state{wParam = fwUtil:initWParam(FName, IsTmp)}, {doAfter, 0}}.

handleAfter(0, #state{wParam = WParam} = State) ->
   %fwUtil:tryWorkOnce(WParam, State);
   NewState = fwUtil:tryWorkLoop(WParam, State),
   {noreply, NewState}.

handleCall(_Msg, _State, _FROM) ->
   ?FwErr("~p call receive unexpect msg ~p ~n ", [?MODULE, _Msg]),
   {reply, ok}.

%% 默认匹配
handleCast(_Msg, _State) ->
   ?FwErr("~p cast receive unexpect msg ~p ~n ", [?MODULE, _Msg]),
   kpS.

handleInfo(mTryWork, #state{wParam = WParam} = State) ->
   %fwUtil:tryWorkOnce(WParam, State);
   NewState = fwUtil:tryWorkLoop(WParam, State),
   {noreply, NewState};
handleInfo(_Msg, _State) ->
   ?FwErr("~p info receive unexpect msg ~p ~n ", [?MODULE, _Msg]),
   kpS.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

work(task1, State) ->
   State;
work(task2, State) ->
   State;
work(_Task, State) ->
   timer:sleep(1),
   % io:format("work out ~p ~p ~n",[self(), _Task]),
   State.

idle(State) ->
   State.

close(State) ->
   State.