-module(fwUtil).

-include("eFaw.hrl").

-export([
   initCfg/1
   , initWParam/2
   , tryWorkLoop/1
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
   #wParam{fName = FName, fNameTid = ?IIF(is_boolean(IsTmp), persistent_term:get(FName), undefined), mod = FName:getV(?wMod), isTmp = IsTmp}.

tryWorkLoop(#wParam{fName = FName, fNameTid = FNameTid, mod = Mod, isTmp = IsTmp} = WParam) ->
   Task = eLfq:tryOut(FNameTid),
   case Task of
      lfq_empty ->
         case IsTmp of
            false ->
               fwFMgr:wSleep(FName, self());
            _ ->
               fwFMgr:tWOver(FName, self())
         end;
      _ ->
         try Mod:work(Task)
         catch
            C:R:S ->
               ?FwErr("woker do task error ~p ~p ~p ~p ~p ~n", [FName, Mod, IsTmp, self(), {C, R, S}])
         end,
         tryWorkLoop(WParam)
   end.