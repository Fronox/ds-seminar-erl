%%%-------------------------------------------------------------------
%%% @author fronox
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Apr 2020 16:33
%%%-------------------------------------------------------------------
-module(sorts).
-author("fronox").

%% API
-export([ms_test/2, compare/2]).

recv_res() ->
  receive
    {res, Res} -> Res
  end.

par_merge_sort([El], _) -> [El];

par_merge_sort(List, {ps, PsCount, PsLim}) when PsCount < PsLim - 1->
  % io:format("Let's go parallel! (~w, ~w, ~w)~n", [PsCount, erlang:system_info(process_count), PsLim]),
  {L1, L2} = lists:split(length(List) div 2, List),
  Self = self(),
  PsInfo = {ps, erlang:system_info(process_count), PsLim},
  spawn_link(fun() -> Self ! {res, par_merge_sort(L1, PsInfo)} end),
  PsInfo2 = {ps, erlang:system_info(process_count), PsLim},
  spawn_link(fun() -> Self ! {res, par_merge_sort(L2, PsInfo2)} end),
  SL1 = recv_res(),
  SL2 = recv_res(),
  lists:merge(SL1, SL2);

par_merge_sort(List, {ps, _, PsLim}) ->
  {L1, L2} = lists:split(length(List) div 2, List),
  PsInfo = {ps, erlang:system_info(process_count), PsLim}, 
  SL1 = par_merge_sort(L1, PsInfo),
  PsInfo2 = {ps, erlang:system_info(process_count), PsLim},
  SL2 = par_merge_sort(L2, PsInfo2),
  lists:merge(SL1, SL2).

seq_merge_sort([El]) -> [El];

seq_merge_sort(List) ->
  {L1, L2} = lists:split(length(List) div 2, List),
  SL1 = seq_merge_sort(L1),
  SL2 = seq_merge_sort(L2),
  lists:merge(SL1, SL2).

ms_test(par, Len, PsCap) ->
  List = [rand:uniform(100) || _ <- lists:seq(1, Len)],
  PsInfo = {ps, erlang:system_info(process_count), erlang:system_info(process_count) + PsCap},
  {Time, _} = timer:tc(fun() -> par_merge_sort(List, PsInfo) end),
  Time / 1000.

ms_test(seq, Len) ->
  List = [rand:uniform(100) || _ <- lists:seq(1, Len)],
  {Time, _} = timer:tc(fun() -> seq_merge_sort(List) end),
  Time / 1000.

compare({arr_len, Len}, {ps_nr, PsCap}) ->
  ParTime = ms_test(par, Len, PsCap),
  SeqTime = ms_test(seq, Len),
  {{seq, SeqTime}, {par, ParTime}, ms}.