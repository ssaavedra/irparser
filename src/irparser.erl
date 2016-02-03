% ---------------------------------------------------------
% irparser - A parser for the intermediate representation
% ---------------------------------------------------------

-module(irparser).

-author("Manuel Montenegro").

-export([main/1]).



% A type declaration has the form <Var> :: <Type>
-define(PAT_TYPE_VAR, "([A-Za-z0-9_]+)\\s*::\\s*([A-Za-z0-9_]+)").

% An assertion has the form -assert("<Assertion>")
-define(PAT_ASSERT, "\\-assert\\(\"(.*)\"\\)").

-define(CHK(Exp, ErrorId), try Exp catch error:{badmatch,X} -> throw({ErrorId, X})).

-import(erl_syntax, [type/1, concrete/1, attribute_name/1, attribute_arguments/1,
                    function_name/1, function_arity/1, function_clauses/1, get_pos/1,
                    clause_patterns/1, variable_literal/1, clause_body/1,
                    application_operator/1, application_arguments/1, get_attrs/1,
                    get_pos/1, map_expr_fields/1, map_field_assoc_name/1,
                    map_field_assoc_value/1, variable_name/1, tuple_elements/1,
                    match_expr_pattern/1, match_expr_body/1, list_elements/1,
                    module_qualifier_argument/1, module_qualifier_body/1,
                    arity_qualifier_body/1, arity_qualifier_argument/1,
                    list_head/1, list_tail/1, case_expr_argument/1, case_expr_clauses/1,
                    map_field_exact_name/1, map_field_exact_value/1,
                    variable/1, abstract/1, application/2]).

% ------------------------------------------------------------------
% Metadata attached to the source file in the form of comments.
% It contains:
%    - Type annotations  (<Var> :: <Type>)
%    - Assertions  (-assert("<assertion>"))
% ------------------------------------------------------------------

-type lineno() :: integer().
-type varname() :: string().
-type vartype() :: string().
-type expression() :: term().
-type assertion() :: string().

                   
-record(finf, {
                    typeenv = #{} :: #{{lineno(), varname()} => vartype()},
                    asserts = [] :: [{lineno(), assertion()}],
                    module = no_module :: atom(),
                    units = [] :: [{atom(), integer()}],
                    builtins = [] :: [{atom(), integer()}],
                    namespace = #{} :: #{{atom(), integer()} => atom()},
                    functions = #{} :: 
                            #{{atom(), integer()} 
                                    => {lineno(), [{varname(), vartype()}], expression()}}
               }).                   

-type comment_info() :: {integer(), integer(), integer(), [string()]}.

% It returns the metadata of the source code by exploring the comments.

-spec get_metadata([comment_info()]) -> #finf{}.
get_metadata(Comments) ->
    {ok, PatTypeVar} = re:compile(?PAT_TYPE_VAR),
    {ok, PatAssert} = re:compile(?PAT_ASSERT),
    TypeDecls = [{{Line, VarName}, Type} || 
                                {Line,_,_,CommsPerLine} <- Comments,
                                Comm <- CommsPerLine,
                                {match, Captures} <- [re:run(Comm, PatTypeVar, 
                                                        [{capture,all,list}, global])],
                                [_, VarName, Type] <- Captures ],
                                
    Asserts = [{Line, Assertion} ||                       
                                {Line,_,_,CommsPerLine} <- Comments,
                                Comm <- CommsPerLine,
                                {match, Captures} <- [re:run(Comm, PatAssert, 
                                                        [{capture,all,list}, global])],
                                [_, Assertion] <- Captures ],
    #finf{typeenv = maps:from_list(TypeDecls), 
              asserts = Asserts}.



to_bin(X) when is_atom(X) -> X;
to_bin(X) when is_list(X) -> list_to_binary(X).


process_form(Form, FileInfo) ->
    case type(Form) of
        attribute -> 
            case concrete(attribute_name(Form)) of
                module ->
                    [ModName] = attribute_arguments(Form),
                    FileInfo#finf{module = concrete(ModName)};
                import ->
                    [ModName, ImportsList] = attribute_arguments(Form),
                    Imports = list_elements(ImportsList),
                    #finf{namespace = NSInit} = FileInfo,
                    NSNew = lists:foldl(
                             fun(I, NS) -> 
                                FunName = arity_qualifier_body(I),
                                Arity = arity_qualifier_argument(I),
                                NS#{{concrete(FunName), concrete(Arity)} => concrete(ModName)}
                             end, NSInit, Imports),
                    FileInfo#finf{namespace = NSNew};
                units -> 
                    #finf{units = Us} = FileInfo,
                    [Funs] = attribute_arguments(Form),
                    FileInfo#finf{units = concrete(Funs) ++ Us};
                builtins ->
                    #finf{builtins = Bs} = FileInfo,
                    [Funs] = attribute_arguments(Form),
                    FileInfo#finf{builtins = concrete(Funs) ++ Bs};
                _ -> FileInfo
            end;
        function -> 
            #finf{functions = Fs, typeenv = TypeEnv} = FileInfo,
            LineNo = get_pos(get_attrs(Form)),
            Name = concrete(function_name(Form)),
            Arity = function_arity(Form),
            [Clause] = function_clauses(Form),
            Patterns = clause_patterns(Clause),
            Vars = [ {variable_literal(Var), 
                        maps:get({LineNo, variable_literal(Var)}, TypeEnv, none)}
                 || Var <- Patterns, type(Var) == variable ],
            Body = clause_body(Clause),
            get_calls(Body),
            FileInfo#finf{functions = Fs#{{Name,Arity} => {LineNo, Vars, Body}}};
        _ -> FileInfo
    end.

parse_file(FileName, Metadata) ->
    {parser, {ok, Forms}} = {parser, epp:parse_file(FileName, [])},
    lists:foldr(fun (Form, FileInfo) -> process_form(Form, FileInfo) end,
                 Metadata, Forms).


acc_calls(Node, Calls) ->
    case type(Node) of
        application ->
            case type(application_operator(Node)) of
                atom -> 
                    Name = concrete(application_operator(Node)),
                    Arity = length(application_arguments(Node)),
                    [{Name,Arity} | Calls];
                _ -> Calls
            end;
        _ -> Calls
    end.
    
get_calls(Node) when is_list(Node) ->
    F = fun(N, Ac) -> erl_syntax_lib:fold(fun acc_calls/2, Ac, N) end,
    lists:foldl(F, [], Node);
get_calls(Node) ->
    get_calls([Node]).
    
    
get_dependency_graph(#finf{functions = Fs, builtins = Bs}) ->
    DepGraph = digraph:new([private]),
    FsList = maps:to_list(Fs),
    [ digraph:add_vertex(DepGraph, FunArity) || {FunArity, _} <- FsList],
    [ digraph:add_edge(DepGraph, FA1, FA2) || 
        {FA1, {_, _, Body}} <- FsList,
        FA2 <- get_calls(Body),
        not lists:member(FA2, Bs)],
    DepGraph.
    

build_letfun([], JMainExp) -> JMainExp;
build_letfun(JDefs, JMainExp) -> 
    #{ expType => letfun,
       defs => JDefs,
       main => JMainExp }.

lit_value_to_JSON(Lit) ->
    case type(Lit) of
        string -> to_bin(concrete(Lit));
        _ -> concrete(Lit)
    end.


variable_to_vartype(ExpVar, TypeEnv) ->
    LineNo = get_pos(get_attrs(ExpVar)),
    Type = maps:get({LineNo, variable_literal(ExpVar)}, TypeEnv, unknown),
    #{name => variable_name(ExpVar), type => to_bin(Type)}.

lhs_to_json(Exp, #finf{typeenv = TypeEnv}) ->
    case type(Exp) of
        variable -> variable_to_vartype(Exp, TypeEnv);
        tuple -> [ variable_to_vartype(SubExp, TypeEnv) 
            || SubExp <- tuple_elements(Exp), type(SubExp) =:= variable 
        ];
        _ -> unknown
    end.



binding_to_json(Binding, FileInfo) ->
    case type(Binding) of
        match_expr -> 
            #{lhs => lhs_to_json(match_expr_pattern(Binding), FileInfo),
              rhs => exp_to_json(match_expr_body(Binding), FileInfo)};
        _ -> unknown
    end.


pattern_to_json(map_expr, Exp, _FileInfo) ->
    [Field] = map_expr_fields(Exp),
    Type = map_field_exact_name(Field),
    Value = map_field_exact_value(Field),
    #{
        patType => literal,
        type => concrete(Type),
        value => lit_value_to_JSON(Value)
    };

pattern_to_json(atom, Exp, _FileInfo) ->
    #{
        patType => constructor,
        name => concrete(Exp),
        args => []
    };

pattern_to_json(list, Exp, FileInfo) ->
    ConsName = concrete(list_head(Exp)),
    Arguments = 
        begin 
            Tail = list_tail(Exp),
            case type(Tail) of
                tuple -> tuple_elements(Tail);
                _ -> [Tail]
            end
        end,
    #{
        patType => constructor,
        name => ConsName,
        args => [pattern_to_json(Arg, FileInfo) || Arg <- Arguments]        
    };    


pattern_to_json(variable, Exp, #finf{typeenv = TypeEnv}) ->
    JVar = variable_to_vartype(Exp, TypeEnv),
    JVar#{ patType => variable };    
    
    
pattern_to_json(tuple, Exp, FileInfo) ->
    #{
        patType => tuple,
        components => [pattern_to_json(P, FileInfo) || P <- tuple_elements(Exp)]
    };

pattern_to_json(_, Exp, _) -> 
    io:format("Unknown pattern: ~s~n", [erl_prettypr:format(Exp)]),
    #{patType => unknown}.

pattern_to_json(Exp, FileInfo) -> pattern_to_json(type(Exp), Exp, FileInfo).


branch_to_json(Branch, FileInfo) -> 
    [Pat] = clause_patterns(Branch),
    Body = clause_body(Branch),
    #{
        pattern => pattern_to_json(Pat, FileInfo),
        exp => exp_to_json(Body, FileInfo)
    }.
    

exp_to_json(sequence, [Exp], FileInfo) -> exp_to_json(Exp, FileInfo);
exp_to_json(sequence, Exps, FileInfo) -> 
    Len = length(Exps),
    {Bindings, MainExp} = lists:split(Len - 1, Exps),
    JBindings = [ JBinding || Binding <- Bindings,
                              JBinding <- [binding_to_json(Binding, FileInfo)],
                              JBinding =/= unknown ],
    JMainExp = exp_to_json(MainExp, FileInfo),
    case JBindings of
        [] -> JMainExp;
        _ -> #{
                expType => 'let',
                bindings => JBindings,
                main => JMainExp
             }
    end;

exp_to_json(map_expr, Exp, _FileInfo) ->
    {singleton_maps, [Field]} = {singleton_maps, map_expr_fields(Exp)},
    LitType = map_field_assoc_name(Field),
    LitValue = map_field_assoc_value(Field),
    #{
        expType => literal, 
        type => concrete(LitType),
        value => lit_value_to_JSON(LitValue)
    };

exp_to_json(variable, Exp, _FileInfo) ->
    #{
        expType => variable,
        variable => variable_name(Exp)
    };    

exp_to_json(tuple, Exp, FileInfo) ->
    #{
        expType => tuple,
        components => [exp_to_json(E, FileInfo) || E <- tuple_elements(Exp)]
    };
    
exp_to_json(list, Exp, FileInfo) ->
    ConsName = concrete(list_head(Exp)),
    Arguments = 
        begin 
            Tail = list_tail(Exp),
            case type(Tail) of
                tuple -> tuple_elements(Tail);
                _ -> [Tail]
            end
        end,
    #{
        expType => constructor,
        name => ConsName,
        args => [exp_to_json(Arg, FileInfo) || Arg <- Arguments]        
    };    

exp_to_json(atom, Exp, _FileInfo) ->
    ConsName = 
        case concrete(Exp) of
            true -> <<"true">>;
            false -> <<"false">>;
            C -> C
        end,
    #{
        expType => constructor,
        name => ConsName,
        args => []
    }; 

exp_to_json(case_expr, Exp, FileInfo) ->
    Discriminant = case_expr_argument(Exp),
    {Default, Nondefault} = list_span(
        fun(Branch) ->
            Pats = clause_patterns(Branch),
            length(Pats) =:= 1 andalso type(hd(Pats)) =:= underscore
        end, case_expr_clauses(Exp)
    ),
    Map = #{
        expType => 'case',
        discriminant => exp_to_json(Discriminant, FileInfo),
        branches => [branch_to_json(Br, FileInfo) || Br <- Nondefault]
    },
    case Default of
        [] -> Map;
        [X|_] -> Map#{defaultBranch => exp_to_json(clause_body(X), FileInfo)}
    end;
        


exp_to_json(application, Exp, #finf{namespace = NS} = FileInfo) ->
    Operator = application_operator(Exp),
    Args = application_arguments(Exp),
    JArgs = [ exp_to_json(Arg, FileInfo) || Arg <- Args ],
    case type(Operator) of
        module_qualifier ->
            ModName = module_qualifier_argument(Operator),
            FunName = module_qualifier_body(Operator),
            #{
                expType => application,
                external => true,
                name => #{ moduleName => concrete(ModName), funName => concrete(FunName),
                           arity => length(Args)},
                arguments => JArgs                
            };
        atom ->
            FunName = concrete(Operator),
            Arity = length(Args),
            case maps:get({FunName, Arity}, NS, false) of
                false -> #{
                            expType => application,
                            external => false,
                            name => to_bin(stringifyFA({FunName, Arity})),
                            arguments => JArgs
                         };
                ModName -> #{
                              expType => application,
                              external => true,
                              name => #{ moduleName => ModName, funName => FunName, arity => Arity },
                              arguments => JArgs
                           }
            end
    end;

exp_to_json(_, Exp, _) -> 
    io:format("Unknown expression: ~s~n", [erl_prettypr:format(Exp)]),
    #{expType => unknown}.

exp_to_json(Exp, FileInfo) when is_list(Exp) -> exp_to_json(sequence, Exp, FileInfo);
exp_to_json(Exp, FileInfo) -> exp_to_json(type(Exp), Exp, FileInfo).
    

        

def_to_json(FunArity, {_, Params, Exp}, FileInfo) ->
    #{
      name => to_bin(stringifyFA(FunArity)),
      params => [ 
        case Type of
            none -> #{name => to_bin(Var)};
            _    -> #{name => to_bin(Var), type => to_bin(Type)}
        end || {Var,Type} <- Params
      ],
      body => exp_to_json(Exp, FileInfo)
      }.

stringifyFA({Fun,Arity}) -> atom_to_list(Fun) ++ "_" ++ integer_to_list(Arity).


build_json_for(#finf{module = M, functions = Fs} = FileInfo, DepGraph, {Fun,Arity} = FunArity) ->
    {_, VarTypes, Body} = maps:get(FunArity, Fs),
    Deps = digraph_utils:reachable_neighbours([FunArity], DepGraph),
    MainRecursive = lists:member(FunArity, Deps),
    JDepFuns = [ def_to_json(FA, maps:get(FA, Fs), FileInfo) || FA <- Deps ],
    MainExp = case MainRecursive of
                 false -> exp_to_json(Body, FileInfo);
                 true -> exp_to_json(
                            application(abstract(Fun), 
                                        [variable(V) || {V,_} <- VarTypes]),
                            FileInfo
                         )
              end,
    #{
        name => #{module => M, fun_name => Fun, arity => Arity},
        params => [ #{name => to_bin(Var), type => to_bin(Type)} || {Var,Type} <- VarTypes],
        body => build_letfun(JDepFuns, MainExp),
        entry_point => to_bin(stringifyFA(FunArity))
    }.


write_to_file(ModName, {F,A}, JSON) ->
    FileName = atom_to_list(ModName) ++ "_" 
                ++ atom_to_list(F) ++ "_" ++ integer_to_list(A) ++ ".json",
    {ok, Handle} = file:open(FileName, [write]),
    io:format(Handle, "~ts~n", [jsx:prettify(jsx:encode(JSON))]),
    io:format("Execution unit ~w/~w written into ~s~n", [F, A, FileName]).

main([FileName]) ->
    Comments = erl_comment_scan:file(FileName),
    FileInfo = get_metadata(Comments),
    FileInfo2 = parse_file(FileName, FileInfo),
    DepGraph = get_dependency_graph(FileInfo2),
    #finf{units = Us, module = ModName} = FileInfo2,
    JSONReps = [ {U, build_json_for(FileInfo2, DepGraph, U)} || U <- Us ],
    [ write_to_file(ModName, U, JSON) || {U, JSON} <- JSONReps ],
    io:format("Done~n");
main(_) -> io:format("Usage: irparser <filename.erl>~n").    
    
    
list_span(_   , Yes, No, []) -> {lists:reverse(Yes), lists:reverse(No)};
list_span(Pred, Yes, No, [X|Xs]) ->
    case Pred(X) of
        true  -> list_span(Pred, [X|Yes], No, Xs);
        false -> list_span(Pred, Yes, [X|No], Xs)
    end.
    
list_span(Pred, Xs) -> list_span(Pred, [], [], Xs).    


