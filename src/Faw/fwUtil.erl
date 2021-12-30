-module(fwUtil).

-export([
   tryWorkOnce/4
   , tryWorkLoop/4
]).

tryWorkOnce(FName, Mod, WStrategy, State) ->
   case WStrategy of
      fifo ->
         Task = fwQueue:outF(FName);
      _ ->
         Task = fwQueue:outL(FName)
   end,
   case Task of
      empty ->
         Mod:idle(State);
      _ ->
         try Mod:work(Task, State) of
            NewState ->
               NewState
         catch
            _C:_R:_S -> State
         end
   end.

tryWorkLoop(FName, Mod, WStrategy, State) ->
   case WStrategy of
      fifo ->
         Task = fwQueue:outF(FName);
      _ ->
         Task = fwQueue:outL(FName)
   end,
   case Task of
      empty ->
         Mod:idle(State);
      _ ->
         NewState =
            try Mod:work(Task, State) of
               TemState ->
                  TemState
            catch
               _C:_R:_S -> State
            end,
         tryWorkLoop(FName, Mod, WStrategy, NewState)
   end.