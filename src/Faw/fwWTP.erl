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
   , handleCall/3
   , handleCast/2
   , handleInfo/2
   , terminate/2
   , code_change/3
]).

-define(SERVER, ?MODULE).
-record(state, {fName :: ets:tid(), isTmp = false :: boolean()}).

%% ********************************************  API *******************************************************************
start_link(FName, IsTmp) ->
   gen_srv:start_link({local, ?SERVER}, ?MODULE, [FName, IsTmp], []).

%% ********************************************  callback **************************************************************
init([FName, IsTmp]) ->
   erlang:process_flag(trap_exit, true),
   {ok, #state{fName = FName, isTmp = IsTmp}}.

handleCall(_Msg, _State, _FROM) ->
   ?ERR("~p call receive unexpect msg ~p ~n ", [?MODULE, _Msg]),
   {reply, ok}.

%% 默认匹配
handleCast(_Msg, _State) ->
   ?ERR("~p cast receive unexpect msg ~p ~n ", [?MODULE, _Msg]),
   kpS.

handleInfo(tryWork, _State) ->
   %fwUtil:tryWorkLoop(xxxxxxxx);
   NewState = fwUtil:tryWorkOnce(),
   {noreply, NewState};
handleInfo(_Msg, _State) ->
   ?ERR("~p info receive unexpect msg ~p ~n ", [?MODULE, _Msg]),
   kpS.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

work(task1, State) ->
   State;
work(task1, State) ->
   State;
work(_Task, State) ->
   State.

idle(State) ->
   State.

close(State) ->
   State.