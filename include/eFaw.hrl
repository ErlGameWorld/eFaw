-define(wMod, wMod).          %% 工人任务处理模块
-define(wFCnt, wFCnt).        %% 固定工人人数
-define(wTCnt, wTCnt).        %% 可以聘请的零时工数量
-define(wTLive, wTLive).      %% 零时工最大空闲时间单位(s) 空闲超时之后就解雇
-define(fTpm, fTpm).          %% 工厂任务处理模式 fifo lifo
-define(fTLfl, fTLfl).        %% 工厂任务承载线 超过该值就可以聘请零时工了 0 固定工人数 工厂负载超过该值时增加零时工
-define(fTMax, fTMax).        %% 工厂最大负载量 超过该值时 工厂就不再接受任务了

-type fawOtp() :: {?wMod, atom()} |{?wFCnt, pos_integer()} |{?wTCnt, pos_integer()} |{?wTLive, pos_integer()} |{?fTpm, fifo | lifo} |{?fTLfl, pos_integer()} | {?fTMax, pos_integer()}.

-define(FawDefV, [
   , {?wMod, fwWTP}
   , {?wFCnt, 30}
   , {?wTCnt, 20}
   , {?wTLive, 300}
   , {?fTpm, fifo}
   , {?fTLfl, 10000}
   , {?fTMax, 10000}
   ]).

-type fawOtps() :: [fawOtp(), ...].

-define(ERR(Format, Args), error_logger:error_msg(Format, Args)).

