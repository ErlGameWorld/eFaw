-module(fwUtil).

-include("eFaw.hrl").

-export([
   initCfg/1
   , initWParam/2
   , tryWorkOnce/2
   , tryWorkLoop/2
]).

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

initWParam(FName, IsTmp) ->
   #wParam{fName = FName, fNameTid = persistent_term:get(FName), mod = FName:getV(?wMod), isTmp = IsTmp}.

tryWorkOnce(#wParam{fName = FName, fNameTid = FNameTid, mod = Mod, isTmp = IsTmp}, State) ->
   Task = eLfq:tryOut(FNameTid),
   case Task of
      lfq_empty ->
         case IsTmp of
            false ->
               fwFMgr:wSleep(FName, self()),
               case erlang:function_exported(Mod, idle, 1) of
                  true ->
                     Mod:idle(State);
                  _ ->
                     State
               end;
            _ ->
               fwFMgr:tWOver(FName, self()),
               case erlang:function_exported(Mod, close, 1) of
                  true ->
                     Mod:close(State);
                  _ ->
                     State
               end
         end;
      _ ->
         try Mod:work(Task, State) of
            NewState ->
               NewState
         catch
            C:R:S ->
               ?FwErr("woker do task error ~p ~p ~p ~p ~p ~n", [FName, Mod, IsTmp, self(), {C, R, S}]),
               State
         end
   end.

tryWorkLoop(#wParam{fName = FName, fNameTid = FNameTid, mod = Mod, isTmp = IsTmp} = WParam, State) ->
   Task = eLfq:tryOut(FNameTid),
   case Task of
      lfq_empty ->
         case IsTmp of
            false ->
               fwFMgr:wSleep(FName, self()),
               case erlang:function_exported(Mod, idle, 1) of
                  true ->
                     Mod:idle(State);
                  _ ->
                     State
               end;
            _ ->
               fwFMgr:tWOver(FName, self()),
               case erlang:function_exported(Mod, close, 1) of
                  true ->
                     Mod:close(State);
                  _ ->
                     State
               end
         end;
      _ ->
         NewState =
            try Mod:work(Task, State) of
               TemState ->
                  TemState
            catch
               C:R:S ->
                  ?FwErr("woker do task error ~p ~p ~p ~p ~p ~n", [FName, Mod, IsTmp, self(), {C, R, S}]),
                  State
            end,
         tryWorkLoop(WParam, NewState)
   end.