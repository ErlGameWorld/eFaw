-module(fwQueue).

-export([
   new/1
   , del/1
   , in/2
   , ins/2
   , outF/1
   , outL/1
   , clear/1
   , size/1
]).

-spec new(Name :: atom()) -> ok | name_used.
new(Name) ->
   case ets:info(Name, id) of
      undefined ->
         ets:new(Name, [ordered_set, public, named_table, {write_concurrency, true}]);
      _ ->
         name_used
   end.

-spec del(Name :: atom()) -> ok.
del(Name) ->
   ets:delete(Name).

-spec in(Name :: atom(), Value :: term()) -> true.
in(Name, Value) ->
   ets:insert(Name, {erlang:unique_integer(), Value}).

-spec ins(Name :: atom(), Values :: [term()]) -> true.
ins(Name, Values) ->
   Tasks = [{erlang:unique_integer(), Value} || Value <- Values],
   ets:insert(Name, Tasks),
   true.

-spec out(Name :: atom()) -> empty | Value :: term().
outF(Name) ->
   case ets:first_take(Name) of
      [] ->
         empty;
      [{_, Value}] ->
         Value
   end.

-spec out(Name :: atom()) -> empty | Value :: term().
outL(Name) ->
   case ets:last_take(Name) of
      [] ->
         empty;
      [{_, Value}] ->
         Value
   end.

-spec clear(Name :: atom()) -> ok.
clear(Name) ->
   ets:delete_all_objects(Name).

-spec size(Name :: atom()) -> Size :: integer() | undefined.
size(Name) ->
   ets:info(Name, size).
