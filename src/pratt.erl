%%% A short Pratt parser implementation in Erlang.

-module(pratt).
-export([main/1, expr/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% -----------------------------------------------------------------------------
%% Entrypoint
%% -----------------------------------------------------------------------------

%% Parse each line in stdin.
main(_Args) ->
  case io:get_line("") of
    eof ->
      erlang:halt(0);
    Line ->
      {_, Output} = expr(Line),
      io:format("~s~n", [Output])
  end.

%% Parse an input string to an S expression string.
-spec expr(Input :: string()) -> {ok, string()} | {error, string()}.
expr(Input) ->
  case expr_bp(tokenize(Input), 0) of 
    {ok, Expr, [eof]} -> {ok, display(Expr)};
    {error, _} = Error -> Error
  end.

%% -----------------------------------------------------------------------------
%% Tokenizing and Displaying
%% -----------------------------------------------------------------------------

%% Simple token conversion: alphanumeric chars are atoms, whitespace is
%% ignored, and everything else is an operator.
tokenize(Input) -> tokenize(Input, []).

tokenize([Char | Tail], Tokens) when Char >= $0, Char =< $9; Char >= $a, Char =< $z;
                                     Char >= $A, Char =< $Z ->
  tokenize(Tail, [{atom, [Char]}] ++ Tokens);
tokenize([Char | Tail], Tokens) when Char =:= $\s; Char =:= $\t; Char =:= $\n; Char =:= $\f;
                                     Char =:= $\r ->
  tokenize(Tail, Tokens);
tokenize([Char | Tail], Tokens) ->
  tokenize(Tail, [{op, list_to_atom([Char])}] ++ Tokens);
tokenize([], Tokens) ->
  lists:reverse([eof] ++ Tokens).

%% Print out S expressions.
display({atom, Atom}) ->
  Atom;
display({cons, Head, Tail}) ->
  TailString = lists:flatten([" " ++ display(T) || T <- Tail]),
  "(" ++ atom_to_list(Head) ++ TailString ++ ")".

%% -----------------------------------------------------------------------------
%% Parsing
%% -----------------------------------------------------------------------------

%% Parse expressions with a minimum binding power.
%%
%% A prefix expression is parsed first, followed by any number of postfix and
%% infix expressions.
expr_bp(Tokens, MinBP) ->
  case expr_prefix(Tokens) of
    {ok, Expr, Tokens2} -> expr_postfix_infix(Tokens2, Expr, MinBP);
    {error, _} = Error -> Error
  end.

%% Parse expressions that don't require the previous.
expr_prefix([{atom, Atom} | Tail]) ->
  {ok, {atom, Atom}, Tail};
expr_prefix([{op, '('} | Tail]) ->
  % Ensure a closing paren after expression.
  case expr_bp(Tail, 0) of
    {ok, Expr, [{op, ')'} | Tail2]} ->
      {ok, Expr, Tail2};
    {ok, _, [Next | _]} ->
      build_error("unexpected token '~s', expected ')'", [Next]);
    {error, _} = Error ->
      Error
  end;
expr_prefix([{op, Op} | Tail]) ->
  RightBP = prefix_binding_power(Op),
  case expr_bp(Tail, RightBP) of
    {ok, Expr, Tail2} ->
      {ok, {cons, Op, [Expr]}, Tail2};
    {error, _} = Error ->
      Error
  end;
expr_prefix([Head | _]) ->
  build_error("unexpected token '~s'", [Head]).

%% Parse postfix and infix expressions as long as there remain tokens to parse
%% and the operators meet the required binding power.
expr_postfix_infix([{op, Op} | Tail] = Tokens, LeftExpr, MinBP) ->
  {Action, Result} =
    case {postfix_binding_power(Op), infix_binding_power(Op)} of
      {LeftBP, _} when LeftBP =/= nil, LeftBP >= MinBP ->
        {continue, expr_postfix(Tail, Op, LeftExpr)};
      {_, {LeftBP, RightBP}} when LeftBP =/= nil, LeftBP >= MinBP ->
        RightResult = expr_bp(Tail, RightBP),
        {continue, expr_infix_wrapped(Op, LeftExpr, RightResult, RightBP)};
      _ ->
        % No operators with high enough binding power.
        {break, {ok, LeftExpr, Tokens}}
    end,
  case {Action, Result} of
    {continue, {ok, Expr, Tokens2}} -> expr_postfix_infix(Tokens2, Expr, MinBP);
    {break, {ok, _, _}} -> Result;
    {_, {error, _} = Error2} -> Error2
  end;
expr_postfix_infix([eof], LeftExpr, _MinBP) ->
  % End of parsing. Bubble up the EOF so it can stop a caller's recursion.
  {ok, LeftExpr, [eof]};
expr_postfix_infix([Head | _Tail], _LeftExpr, _MinBP) ->
  build_error("unexpected token '~s', expected operator", [Head]).

%% Call expr_infix when RightExpr parsing succeeds.
expr_infix_wrapped(Op, LeftExpr, {ok, RightExpr, Tokens}, RightBP) ->
  expr_infix(Tokens, Op, LeftExpr, RightExpr, RightBP);
expr_infix_wrapped({error, _} = Error, _Op, _LeftExpr, _RightBP) ->
  Error.

%% Parse expressions that need the previous expression.
expr_postfix(Tokens, '[', LeftExpr) ->
  % Ensure a closing bracket after expression.
  case expr_bp(Tokens, 0) of
    {ok, Expr, [{op, ']'} | Tail]} ->
      {ok, {cons, '[', [LeftExpr, Expr]}, Tail};
    {ok, _, [Next | _]} ->
      build_error("unexpected token '~s', expected ']'", [Next]);
    {error, _} = Error ->
      Error
  end;
expr_postfix(Tokens, Op, LeftExpr) ->
  {ok, {cons, Op, [LeftExpr]}, Tokens}.

%% Parse expressions that need both the previous and next expressions.
expr_infix([{op, ':'} | Tail], '?', LeftExpr, MiddleExpr, RightBP) ->
  case expr_bp(Tail, RightBP) of
    {ok, RightExpr, Tokens} ->
      {ok, {cons, '?', [LeftExpr, MiddleExpr, RightExpr]}, Tokens};
    {error, _} = Error ->
      Error
  end;
expr_infix([Head | _], '?', _LeftExpr, _MiddleExpr, _RightBP) ->
  build_error("unexpected token '~s', expected ':'", [Head]);
expr_infix(Tokens, Op, LeftExpr, RightExpr, _RightBP) ->
  {ok, {cons, Op, [LeftExpr, RightExpr]}, Tokens}.

%% Binding power of prefix operators (applies to the right).
prefix_binding_power('+') -> 9;
prefix_binding_power('-') -> 9.

%% Binding power of postfix operators (applies to the left).
postfix_binding_power('!') -> 11;
postfix_binding_power('[') -> 11;
postfix_binding_power(_) -> nil.

%% Binding power of infix operators (left and right). Higher binding power on
%% the left makes an operator right-associative, and higher binding power on
%% makes an operator left-associative.
infix_binding_power('=') -> {2, 1};
infix_binding_power('?') -> {4, 3};
infix_binding_power('+') -> {5, 6};
infix_binding_power('-') -> {5, 6};
infix_binding_power('*') -> {7, 8};
infix_binding_power('/') -> {7, 8};
infix_binding_power('.') -> {14, 13};
infix_binding_power(_) -> nil.

%% -----------------------------------------------------------------------------
%% Helpers
%% -----------------------------------------------------------------------------

build_error(Format, Terms) ->
  TermStrings = [io_lib:write(T) || T <- Terms],
  Message = lists:flatten(io_lib:fwrite(Format, TermStrings)),
  {error, "error: " ++ Message}.

%% -----------------------------------------------------------------------------
%% Tests
%% -----------------------------------------------------------------------------

-ifdef(TEST).
expr_test_() ->
  [?_assert(expr("1") =:=
              {ok, "1"}),
   ?_assert(expr("1 + 2 * 3") =:=
              {ok, "(+ 1 (* 2 3))"}),
   ?_assert(expr("a + b * c * d + e") =:=
              {ok, "(+ (+ a (* (* b c) d)) e)"}),
   ?_assert(expr("f . g . h") =:=
              {ok, "(. f (. g h))"}),
   ?_assert(expr(" 1 + 2 + f . g . h * 3 * 4") =:=
              {ok, "(+ (+ 1 2) (* (* (. f (. g h)) 3) 4))"}),
   ?_assert(expr("--1 * 2") =:=
              {ok, "(* (- (- 1)) 2)"}),
   ?_assert(expr("--f . g") =:=
              {ok, "(- (- (. f g)))"}),
   ?_assert(expr("-9!") =:=
              {ok, "(- (! 9))"}),
   ?_assert(expr("f . g !") =:=
              {ok, "(! (. f g))"}),
   ?_assert(expr("(((0)))") =:=
              {ok, "0"}),
   ?_assert(expr("x[0][1]") =:=
              {ok, "([ ([ x 0) 1)"}),
   ?_assert(expr("a ? b : 
                  c ? d
                  : e") =:=
              {ok, "(? a b (? c d e))"}),
   ?_assert(expr("a = 0 ? b : c = d") =:=
              {ok, "(= a (= (? 0 b c) d))"})
  ].
-endif.
