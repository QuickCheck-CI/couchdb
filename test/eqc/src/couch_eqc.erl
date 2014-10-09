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

-record(state, {file_desc, contents, header}).
-record(header, {index, content, size}).

initial_state() ->
  #state{file_desc = undefined, contents = [], header = not_set}.

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
  [S#state.file_desc, frequency([{10, {bin,      eqc_gen:largebinary()}},
                                 {10, {term,     term()}},
                                 {5,  {list_bin, non_empty(list(binary()))}}])].

append({ok, Fd}, {Type, Data}) ->
  case Type of
    term -> couch_file:append_term(Fd, Data);
    _    -> couch_file:append_binary(Fd, Data)
  end.

append_pre(S) ->
  S#state.file_desc /= undefined.

append_next(S, Index, [_Fd, {Type, Data}]) ->
  S#state{contents = S#state.contents ++ [{Index, Type, Data}]}.

append_post(_S, _, {ok, _Index, _Size}) ->
  true. 

%% -- Read term --
pread_args(S) ->
  [S#state.file_desc, safe_oneof(S#state.contents)].

pread({ok, Fd}, {{ok, Index, _Size}, Type, _Term}) ->
  case Type of
    bin      -> couch_file:pread_binary(Fd, Index);
    list_bin -> couch_file:pread_iolist(Fd, Index);
    _        -> couch_file:pread_term(Fd, Index)
  end.

pread_pre(S) ->
  S#state.file_desc /= undefined andalso S#state.contents /= [].

pread_next(S, _, _) -> 
  S.

pread_post(S, [_, {Index, Type,  _}], {ok, Term}) ->
  case Type of
    list_bin -> eq(iolist_to_binary(Term), concat_bin(get_term(Index, S)));
    _        -> eq(Term, get_term(Index, S))
  end.

concat_bin(Bs) ->
  lists:foldr(fun(X, Y) -> <<X/binary, Y/binary>> end, <<>>, Bs).

get_term(Index, S) ->
  {Index, _Type, Data} = lists:keyfind(Index, 1, S#state.contents), Data.

%% -- Truncate file --
truncate_args(S) ->
  [S#state.file_desc, safe_oneof([I || {I, _, _} <- S#state.contents])].

truncate({ok, Fd}, {ok, Index, _Size}) ->
  couch_file:truncate(Fd, Index).

truncate_pre(S) ->
  S#state.file_desc /= undefined andalso S#state.contents /= [].

truncate_next(S, _Value, [_, Index]) ->
  Cts = lists:takewhile(fun({I, _, _}) -> I /= Index end, S#state.contents),
  Hd  = if (S#state.header)#header.index >= Index -> not_set;
           true                                   -> S#state.header
        end,
  S#state{ contents = Cts, header = Hd }.

truncate_post(_S, _Args, _Res) ->
  true.

%% -- Number of bytes --
bytes_args(S) ->
  [S#state.file_desc].

bytes({ok, Fd}) ->
  couch_file:bytes(Fd).

bytes_pre(S) -> 
  S#state.file_desc /= undefined.

bytes_next(S, _, _) -> 
  S.

bytes_post(S, _, {ok, Size}) ->
  {ok, Index, TermSize} = last_index(S),
  Size >= Index + TermSize.

last_index(S) ->
  case S#state.contents of
    [] -> {ok, 0, 0};
    Xs -> element(1,  lists:last(Xs))
  end.

%% -- Write header
write_header_args(S) ->
  [S#state.file_desc, list(char())].

write_header({ok, Fd}, Txt) ->
  couch_file:write_header(Fd, Txt).

write_header_pre(S) ->
  S#state.file_desc /= undefined.

write_header_next(S, _, [_, Txt]) ->
  Size = {call, ?MODULE, mybytes, [S#state.file_desc]},
  S#state{header = #header{ index = last_index(S)
                          , size  = Size
                          , content = Txt }}.

mybytes({ok, Fd}) -> {ok, Size} = couch_file:bytes(Fd), Size.

write_header_post(_S, _, Res) ->
  eq(Res, ok).

%% -- Read header
read_header_args(S) ->
  [S#state.file_desc].

read_header({ok, Fd}) ->
  couch_file:read_header(Fd).

read_header_pre(S) ->
  S#state.file_desc /= undefined.

read_header_next(S, _, _) ->
  S.

read_header_post(S, _, Res) ->
  case S#state.header of
    not_set -> eq(Res, no_valid_header);
    _       -> eq(Res, {ok, (S#state.header)#header.content})
  end.

%% -- Sync
sync_args(S) ->
  [S#state.file_desc].

sync({ok, Fd}) ->
  couch_file:sync(Fd).

sync_pre(S) ->
  S#state.file_desc /= undefined.

sync_next(S, _, _) ->
  S.

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
  S#state{file_desc = undefined, contents = [], header = not_set}.

close_post(_S, _Args, Res) ->
  eq(Res, ok).


%% @doc weight/2 - Distribution of calls
-spec weight(S :: eqc_statem:symbolic_state(), Command :: atom()) -> integer().
weight(_S, open)         -> 3;
weight(_S, append)       -> 20;
weight(_S, pread)        -> 25;
weight(_S, truncate)     -> 3;
weight(_S, sync)         -> 3;
weight(_S, bytes)        -> 5;
weight(_S, write_header) -> 3;
weight(_S, read_header)  -> 5;
weight(_S, close)        -> 1;
weight(_S, _Cmd)         -> 1.

%% @doc Default generated property
-spec prop_file_basics() -> eqc:property().
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
