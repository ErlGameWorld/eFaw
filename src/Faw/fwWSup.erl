-module(fwWSup).

-behavior(supervisor).

-export([start_link/2]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(FName, Mod) ->
   supervisor:start_link({local, FName}, ?MODULE, [FName, Mod]).

init([FName, Mod]) ->
   SupFlags = #{strategy => simple_one_for_one, intensity => 100, period => 3600},
   ChildSpecs = [
      #{
         id => Mod,
         start => {Mod, start_link, [FName]},
         restart => permanent,
         shutdown => 30000,
         type => worker,
         modules => [Mod]
      }
   ],
   {ok, {SupFlags, ChildSpecs}}.