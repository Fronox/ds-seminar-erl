%%%-------------------------------------------------------------------
%%% @author fronox
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Apr 2020 14:04
%%%-------------------------------------------------------------------
-module(matx).
-author("fronox").

%% API
-export([compare/1]).

% Seq implementation
mat_mul_iter(M1Row, M2Col) ->
  lists:sum([X * Y || {X, Y} <- lists:zip(M1Row, M2Col)]).

seq_mat_mul(M1, M2) when length(hd(M1)) == length(M2) ->
  seq_mat_mul(M1, M2, [], 1, []).

seq_mat_mul([], _, _, _, AccGlob) ->
  AccGlob;

seq_mat_mul(M1, M2, Acc, Idx, AccGlob) when Idx =< length(hd(M2)) -> % For each column in M2
  M1Row = hd(M1),
  M2Col = lists:map(fun(M2Row) -> lists:nth(Idx, M2Row) end, M2),
  IterRes = mat_mul_iter(M1Row, M2Col),
  seq_mat_mul(M1, M2, Acc ++ [IterRes], Idx + 1, AccGlob);

seq_mat_mul([_|T1], M2, Acc, _, AccGlob) ->
  seq_mat_mul(T1, M2, [], 1, AccGlob ++ [Acc]).


% Par implementation
par_mat_mul(M1, M2, PsLim) ->
    Indices = lists:seq(1, length(M1)),
    Self = self(),
    Tags = lists:map(fun(Idx) -> 
        Tag = make_ref(),
        MsgInfo = {msg, Self, Tag},
        PsInfo = {ps, erlang:system_info(process_count), PsLim},
        par_row_mat_mul(M1, M2, Idx, MsgInfo, PsInfo),
        Tag
    end, Indices),
    [receive 
        {Tag, Res} -> Res 
    end || Tag <- Tags].

% Row-mat operations
row_mat_mul(M1, M2, Idx, PsLim) ->
    M1Row = lists:nth(Idx, M1),
    Self = self(),
    M2Indices = lists:seq(1, length(hd(M2))),
    Tags = lists:map(fun(Idx2) -> 
        Tag = make_ref(),
        MsgInfo = {msg, Self, Tag},
        PsInfo = {ps, erlang:system_info(process_count), PsLim},
        par_row_col_mul(M1Row, M2, Idx2, MsgInfo, PsInfo),
        Tag
    end, M2Indices),
    [receive 
        {Tag, Res} -> Res 
    end || Tag <- Tags].

par_row_mat_mul(M1, M2, Idx, {msg, Parent, Tag}, {ps, PsCount, PsLim}) when PsCount < PsLim ->
    spawn_link(fun() ->
        Res = row_mat_mul(M1, M2, Idx, PsLim),
        Parent ! {Tag, Res}
    end);

par_row_mat_mul(M1, M2, Idx, {msg, Parent, Tag}, {ps, _, PsLim}) ->
    Res = row_mat_mul(M1, M2, Idx, PsLim),
    Parent ! {Tag, Res}.

% Row-col opeartions
row_col_mul(M1Row, M2, Idx2) ->
    M2Col = lists:map(fun(M2Row) -> lists:nth(Idx2, M2Row) end, M2),
    mat_mul_iter(M1Row, M2Col).

par_row_col_mul(M1Row, M2, Idx2, {msg, Parent, Tag}, {ps, PsCount, PsLim}) when PsCount < PsLim ->
    spawn_link(fun() -> 
        Res = row_col_mul(M1Row, M2, Idx2),
        Parent ! {Tag, Res}
    end);

par_row_col_mul(M1Row, M2, Idx2, {msg, Parent, Tag}, _) ->
    Res = row_col_mul(M1Row, M2, Idx2),
    Parent ! {Tag, Res}.

compare(PsCap) ->
    M1 = [
        [1,2,3,4],
        [5,6,7,8],
        [1,1,1,1]
    ],
    M2 = [
        [1, 1],
        [2, 3],
        [3, 4],
        [4, 1]
    ],
    {SeqTime, Res} = timer:tc(fun() -> seq_mat_mul(M1, M2) end),
    {ParTime, Res2} = timer:tc(fun() -> par_mat_mul(M1, M2, erlang:system_info(process_count) + PsCap) end),
    io:format("~w~n", [Res]),
    io:format("~w~n", [Res2]),
    {{seq, SeqTime}, {par, ParTime}, ms}.
