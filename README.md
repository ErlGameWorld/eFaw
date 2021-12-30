eFaw
=====

Eralng's Factories and workers.

Build
-----

    $ rebar3 compile

Useage
------
    1 write you worker mod like: fwWtp.erl 
    2 open your factory        : eFaw:openF(myFactory, [{wMod, fwWtp}, ...]), more option see eFaw.hrl
    3 send your async task to your factory: eFaw:inWork(myFactory, [{report_log, xxxx}, {write_log, yyyyyy}]).
    4 apply your sync task to your factory: eFaw:syncWork(myFactory, retTag, 5000, {report_log, xxxx}).
    5 then worker auto do the task

    all API:
        start/0               %% start app
        stop/0                %% stop app
        openF/2               %% Open a factory
        closeF/1              %% close a factory
        hireW/3               %% hire worker
        inWork/2              %% Insert async task
        inWorks/2             %% Insert async tasks
        syncWork/4            %% Insert sync task And wait for the result to receive
