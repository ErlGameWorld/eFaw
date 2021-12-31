-define(wMod, wMod).          %% worker Mod
-define(wFCnt, wFCnt).        %% worker fixed count
-define(wTCnt, wTCnt).        %% worker temp count
-define(fTpm, fTpm).          %% Factory task processing mode fifo lifo
-define(fTLfl, fTLfl).        %% Factory task load line When the factory load exceeds this value, temp workers can be hired
-define(fTMax, fTMax).        %% Maximum plant load Beyond this value, the factory will no longer accept tasks

-type fawOtp() :: {?wMod, atom()} |{?wFCnt, pos_integer()} |{?wTCnt, pos_integer()} |{?fTpm, fifo | lifo} |{?fTLfl, pos_integer() | infinity} | {?fTMax, pos_integer() | infinity}.

-define(FawDefV, [
   {?wMod, fwWTP}
   , {?wFCnt, 30}
   , {?wTCnt, 20}
   , {?fTpm, fifo}
   , {?fTLfl, 10000}
   , {?fTMax, infinity}
]).

-type fawOtps() :: [fawOtp(), ...].

-record(wParam, {fName :: atom(), fNameTid :: ets:tid(), mod :: atom(), fTpm = fifo :: fifo | lifo, isTmp = false :: boolean()}).

-define(FwErr(Format, Args), error_logger:error_msg(Format, Args)).

