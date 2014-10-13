%%% @author Alex Gerdes <alex@quviq.com>
%%% @copyright (C) 2014, QuviQ AB
%%% @doc
%%%
%%% @end
%%% Created : 30 Jul 2014 by Alex Gerdes <alex@quviq.com>

-module(couch_eqc).


-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-compile(export_all).

-define(TESTFILE, "couch_eqc_test_file").

-record(state,  {file_desc, contents}).
-record(entry,  {index, type, data}).
-record(header, {data}).

initial_state() ->
  #state{file_desc = undefined, contents = []}.

%% -- Open
open_args(_S) ->
  [].

open() ->
  couch_file:open(?TESTFILE, [create, overwrite]).

open_pre(S, _Args) ->
  S#state.file_desc == undefined.

open_next(S, Fd, _Args) ->
  S#state{file_desc = Fd, contents = []}.

open_post(_S, _Args, {ok, Res}) ->
  is_pid(Res) andalso eq(couch_file:bytes(Res), {ok, 0}).


%% -- Append 
append_args(S) ->
  [S#state.file_desc, frequency([ {10, {bin,      eqc_gen:largebinary()}}
                                , {10, {term,     term()}}
                                , {5,  {list_bin, non_empty(list(binary()))}}
                                ])].

append({ok, Fd}, {Type, Data}) ->
  case Type of
    term -> couch_file:append_term(Fd, Data);
    _    -> couch_file:append_binary(Fd, Data)
  end.

append_pre(S) ->
  S#state.file_desc /= undefined.

append_next(S, Index, [_Fd, {Type, Data}]) ->
  S#state{contents = [ #entry{type = Type, index = Index, data = Data} 
                     | S#state.contents ]}.

append_post(_S, _, {ok, _Index, _Size}) ->
  true. 

%% -- Read term --
pread_args(S) ->
  [S#state.file_desc, safe_oneof(entries(S))].

pread({ok, Fd}, #entry{type = Type, index = {ok, Index, _Size}}) ->
  case Type of
    bin      -> couch_file:pread_binary(Fd, Index);
    list_bin -> couch_file:pread_iolist(Fd, Index);
    _        -> couch_file:pread_term(Fd, Index)
  end.

pread_pre(S) ->
  S#state.file_desc /= undefined andalso entries(S) /= [].

pread_post(S, [_, #entry{type = Type, index = Index}], {ok, Term}) ->
  case Type of
    list_bin -> eq(iolist_to_binary(Term), concat_bin(get_term(Index, S)));
    _        -> eq(Term, get_term(Index, S))
  end.

%% -- Truncate file --
truncate_args(S) ->
  [S#state.file_desc, safe_oneof([ {ok, 0, 0} 
                                 | [I || #entry{index = I} <- entries(S)] ])].

truncate({ok, Fd}, {ok, Index, _Size}) ->
  couch_file:truncate(Fd, Index).

truncate_pre(S) ->
  S#state.file_desc /= undefined andalso entries(S) /= [].

truncate_next(S, _Value, [_, Index]) ->
  F = fun(#entry{index = I}) -> I /= Index;
         (_)                 -> true
      end,
  Cts = case lists:dropwhile(F, S#state.contents) of
          [] -> [];
          Xs -> tl(Xs)
        end,
  S#state{contents = Cts}.

%% -- Number of bytes --
bytes_args(S) ->
  [S#state.file_desc].

bytes({ok, Fd}) ->
  couch_file:bytes(Fd).

bytes_pre(S) -> 
  S#state.file_desc /= undefined.

bytes_post(S, _, {ok, Size}) ->
  case entries(S) of
    [] -> Size == 0 orelse headers(S) /= [];
    [#entry{index = {ok, Index, TermSize}} | _] -> 
      Size >= Index + TermSize
  end.

%% -- Write header
write_header_args(S) ->
  [S#state.file_desc, list(char())].

write_header({ok, Fd}, Txt) ->
  couch_file:write_header(Fd, Txt).

write_header_pre(S) ->
  S#state.file_desc /= undefined.

write_header_next(S, _, [_, Txt]) ->
  S#state{contents = [#header{data = Txt} | S#state.contents]}.

write_header_post(_S, _, Res) ->
  eq(Res, ok).

%% -- Read header
read_header_args(S) ->
  [S#state.file_desc].

read_header({ok, Fd}) ->
  couch_file:read_header(Fd).

read_header_pre(S) ->
  S#state.file_desc /= undefined.

read_header_post(S, _, Res) ->
  case headers(S) of
    []                       -> eq(Res, no_valid_header);
    [#header{data = Data}|_] -> eq(Res, {ok, Data})
  end.

%% -- Sync
sync_args(S) ->
  [S#state.file_desc].

sync({ok, Fd}) ->
  couch_file:sync(Fd).

sync_pre(S) ->
  S#state.file_desc /= undefined.

sync_post(_S, _, Res) ->
  eq(Res, ok).

%% -- Close --
close_args(S) ->
  [S#state.file_desc].

close({ok, Fd}) ->
  couch_file:close(Fd).

close_pre(S, _Args) ->
  S#state.file_desc /= undefined.

close_next(S, _Value, _Fd) ->
  S#state{file_desc = undefined, contents = []}.

close_post(_S, _Args, Res) ->
  eq(Res, ok).


%% @doc weight/2 - Distribution of calls
weight(_S, open)         -> 1;
weight(_S, append)       -> 20;
weight(_S, pread)        -> 25;
weight(_S, truncate)     -> 9;
weight(_S, sync)         -> 3;
weight(_S, bytes)        -> 5;
weight(_S, write_header) -> 8;
weight(_S, read_header)  -> 8;
weight(_S, close)        -> 1;
weight(_S, _Cmd)         -> 1.

%% @doc Default generated property
prop_file_basics() ->
  ?SETUP(fun() ->
           os:cmd("rm -f " ++ ?TESTFILE),
           fun() -> ok end
         end,
         ?FORALL(Cmds, commands(?MODULE),
                 aggregate(command_names(Cmds),
                   begin
                     {H, S, Res} = run_commands(?MODULE,Cmds),
                     couch_close(S) ,
                     pretty_commands(?MODULE, Cmds, {H, S, Res},
                                     Res == ok)
                   end))).

couch_close(S) ->
  case S#state.file_desc of
    undefined -> true;
    {ok, Fd}  -> couch_file:close(Fd), true
  end.

test() ->
  quickcheck(prop_file_basics()).
test(N) ->
  quickcheck(numtests(N, prop_file_basics())).

%% ------ Generators ----------
safe_oneof([]) -> undef;
safe_oneof(Xs) -> oneof(Xs).

term() ->
  ?SIZED(Size, term(Size)).

term(0)    -> literal();
term(Size) -> N = Size div 3, oneof([literal(), list(term(N)), tuple(term(N))]).

tuple(Gen) ->
  ?LET(Xs, list(Gen), list_to_tuple(Xs)).

literal() ->
  oneof([atom(), bool(), char(), int()]).

atom() ->
  ?LET(S, list(char()), list_to_atom(S)).

%% -- Help functions
entries(S) ->
  [C || C = #entry{} <- S#state.contents].

headers(S) ->
  [H || H = #header{} <- S#state.contents].

get_term(Index, S) ->
  #entry{data = Data} = lists:keyfind(Index, #entry.index, entries(S)), Data.

concat_bin(Bs) ->
  lists:foldr(fun(X, Y) -> <<X/binary, Y/binary>> end, <<>>, Bs).
