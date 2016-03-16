% ---------------------------------------------------------
% assertparser - A parser for the intermediate asserts representation
% ---------------------------------------------------------
% Copyright 2016 Santiago Saavedra
%
% This code is distributed under the terms of Apache License
% See LICENSE for more information
% ---------------------------------------------------------


-module(assertparser).

-author("Santiago Saavedra").
-include("irparser.hrl").

-export([parse_functions/1]).


% Regular expression definitions --------------------------

% A type declaration has the form <Var> :: <Type>
-define(PAT_TYPE_VAR, "([A-Za-z0-9_]+)\\s*::\\s*([A-Za-z0-9_]+)").

% An assertion has the form -assert("<Assertion>")
-define(PAT_ASSERT, "\\-assert\\(\"(.*)\"\\)").

parse_assert(Str, Vars) ->
    %% This function must parse expressions such as:
    %% (given vars inc. [{H, heap}, {V, loc (array int)}, {J, int}, {I, int}]
    %% forall E: array int. deref(H,V) = E -> sorted_sub E 0 (J + 1) and sorted_sub E (J + 1) (I + 1)

    %% or maybe
    %% forall E: array int. deref(H,V) = E ->
    %%   forall J1: int. J1 = J + 1 ->
    %%   forall I1: int. I1 = I + 1 -> sorted_sub E 0 J1 and sorted_sub E J1 I1

    %% WIP
    Str.

parse_asserts_for_function({Name, Arity}, Args) ->
    fun (#{line := AssertLine, assertType := AssertType, rawExpr := Raw}) ->
	    Params = irparser:params_to_json(Args),
	    Expr = parse_assert(Raw, Params),

	    #{line => AssertLine,
	      assertType => AssertType,
	      rawExpr => Raw,
	      params => Params,
	      expr => Expr
	     }
    end.

parse_function (K, F) ->
    {LineNo, Args, Asserts, Body} = F,
    Asserts2 = lists:map(parse_asserts_for_function (K, Args), Asserts),
    {LineNo, Args, Asserts2, Body}.
    

parse_functions(FileInfo) ->
    maps:map(fun parse_function/2, FileInfo#finf.functions).
