-define(wMod, wMod).          %% worker Mod
-define(wFCnt, wFCnt).        %% worker fixed count
-define(wTCnt, wTCnt).        %% worker temp count
-define(fTLfl, fTLfl).        %% Factory task load line When the factory load exceeds this value, temp workers can be hired
-define(fTMax, fTMax).        %% Maximum plant load Beyond this value, the factory will no longer accept tasks

%% 同步任务的等待时间
-define(WAIT_TIME, 5000).

-type fawOtp() :: {?wMod, atom()} | {?wFCnt, pos_integer()} | {?wTCnt, pos_integer()} | {?fTLfl, pos_integer() | infinity} | {?fTMax, pos_integer() | infinity}.

-define(FawDefV, [
   {?wMod, fwWTP}
   , {?wFCnt, 30}
   , {?wTCnt, 20}
   , {?fTLfl, 10000}
   , {?fTMax, infinity}
]).

-type fawOtps() :: [fawOtp(), ...].

-record(wParam, {fName :: atom(), fNameTid :: ets:tid(), mod :: atom(), isTmp = false :: boolean()}).

-define(IIF(Cond, Ret1, Ret2), (case Cond of true -> Ret1; _ -> Ret2 end)).

-define(FwErr(Format, Args), error_logger:error_msg(Format, Args)).

