% ---------------------------------------------------------
% irparser - A parser for the intermediate representation
% ---------------------------------------------------------
% Copyright 2016 Manuel Montenegro
%
% This code is distributed under the terms of Apache License
% See LICENSE for more information
% ---------------------------------------------------------


-module(irparser).

-author("Manuel Montenegro").

-export([main/1]).


% Regular expression definitions --------------------------

% A type declaration has the form <Var> :: <Type>
-define(PAT_TYPE_VAR, "([A-Za-z0-9_]+)\\s*::\\s*([A-Za-z0-9_]+)").

% An assertion has the form -assert("<Assertion>")
-define(PAT_ASSERT, "\\-assert\\(\"(.*)\"\\)").



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
% Global information gathered from the source code:
%
% It contains: 
%    - Type annotations  (<Var> :: <Type>)
%    - Assertions  (-assert("<assertion>"))
%    - Module name
%    - Execution units to be transformed into JSON
%    - Functions imported in the current namespace
%    - Function definitions contained within the file
% ------------------------------------------------------------------

-type lineno() :: integer().
-type varname() :: string().
-type vartype() :: string().
-type expression() :: term().
-type assertion() :: string().

                   
-record(finf, {
                    % A type environment associates the declaration of a variable
                    % in a given line to its type.
                    typeenv = #{} :: #{{lineno(), varname()} => vartype()},
                    
                    % Assertions contained within the program (not used yet)
                    asserts = [] :: [{lineno(), assertion()}],
                    
                    % Name of the module being transformed
                    module = no_module :: atom(),
                    
                    % Execution units to be generated (name/arity)
                    units = [] :: [{atom(), integer()}],
                    
                    % Functions to be excluded from the transformation (name/arity)
                    builtins = [] :: [{atom(), integer()}],
                    
                    % Imported functions: for each name/arity it gives the module
                    % from which it is imported.
                    namespace = #{} :: #{{atom(), integer()} => atom()},
                    
                    % Functions contained within the file. For each function it
                    % stores the line number, the parameters (with its types), and
                    % their body
                    functions = #{} :: 
                            #{{atom(), integer()} 
                                    => {lineno(), [{varname(), vartype()}], expression()}}
               }).                   


% It traverses the comments in order to fill in the 'typeenv' and 'asserts' fields
% of the FileInfo structure.

get_metadata(Comments, FileInfo) ->
    {ok, PatTypeVar} = re:compile(?PAT_TYPE_VAR),
    {ok, PatAssert} = re:compile(?PAT_ASSERT),

    % It matches the PAT_TYPE_VAR regular expression against each comment
    TypeDecls = [{{Line, VarName}, Type} || 
                                {Line,_,_,CommsPerLine} <- Comments,
                                Comm <- CommsPerLine,
                                {match, Captures} <- [re:run(Comm, PatTypeVar, 
                                                        [{capture,all,list}, global])],
                                [_, VarName, Type] <- Captures ],
                                
    % The same, but for assertions
    Asserts = [{Line, Assertion} ||                       
                                {Line,_,_,CommsPerLine} <- Comments,
                                Comm <- CommsPerLine,
                                {match, Captures} <- [re:run(Comm, PatAssert, 
                                                        [{capture,all,list}, global])],
                                [_, Assertion] <- Captures ],
    FileInfo#finf{typeenv = maps:from_list(TypeDecls), 
              asserts = Asserts}.




% It completes the information from the input file.
%
% This function does not take comments into account, as these have already been 
% handled by get_metadata/2.

parse_file(FileName, Metadata) ->
    {parser, {ok, Forms}} = {parser, epp:parse_file(FileName, [])},
    lists:foldr(fun (Form, FileInfo) -> process_form(Form, FileInfo) end,
                 Metadata, Forms).



% This function classifies each declaration of the program.

process_form(Form, FileInfo) ->
    case type(Form) of

        % Module attributes
        attribute -> 
            case concrete(attribute_name(Form)) of
            
                % Module name
                % -module(ModName).
                
                module ->
                    [ModName] = attribute_arguments(Form),
                    FileInfo#finf{module = concrete(ModName)};
                    
                % Import from other module
                % -import(ModName, [Import_1, ..., Import_n])
                
                import ->
                    [ModName, ImportsList] = attribute_arguments(Form),
                    Imports = list_elements(ImportsList),
                    #finf{namespace = NSInit} = FileInfo,
                    
                    % It traverses the list of imported functions, and
                    % adds the binding F/A => Module
                    NSNew = lists:foldl(
                             fun(I, NS) -> 
                                FunName = arity_qualifier_body(I),
                                Arity = arity_qualifier_argument(I),
                                NS#{{concrete(FunName), concrete(Arity)} => concrete(ModName)}
                             end, NSInit, Imports),
                    FileInfo#finf{namespace = NSNew};
                    
                % Execution units to be translated. The program will translate also
                % their dependencies, except those included excluded by means of the 
                % -builtins attribute.
                %
                % -units([f/n, ...])
                    
                units -> 
                    #finf{units = Us} = FileInfo,
                    {bad_unit, [Funs]} = {bad_unit, attribute_arguments(Form)},
                    FileInfo#finf{units = concrete(Funs) ++ Us};
                    
                % Execution units to be excluded from the translation
                % -builtins([f/n, ...])
                    
                builtins ->
                    #finf{builtins = Bs} = FileInfo,
                    {bad_builtins, [Funs]} = {bad_builtins, attribute_arguments(Form)},
                    FileInfo#finf{builtins = concrete(Funs) ++ Bs};
                    
                _ -> FileInfo
            end;
            
        % Function definitions
            
        function -> 
            #finf{functions = Fs, typeenv = TypeEnv} = FileInfo,
            LineNo = get_pos(get_attrs(Form)),
            Name = concrete(function_name(Form)),
            Arity = function_arity(Form),
            % Only functions with one clause are permitted
            {multiple_clauses, [Clause]} = {multiple_clauses, function_clauses(Form)},
            Patterns = clause_patterns(Clause),
            % We include the type of each variable. We look in the environment
            Vars = [ {variable_literal(Var), 
                        maps:get({LineNo, variable_literal(Var)}, TypeEnv, none)}
                 || Var <- Patterns, type(Var) == variable ],
            Body = clause_body(Clause),
            FileInfo#finf{functions = Fs#{{Name,Arity} => {LineNo, Vars, Body}}};
        _ -> FileInfo
    end.


% This function obtains the dependencies of a given expression (or list of expressions).
% That is, it gathers the names and arities from the functions that are called from the
% expression.
get_calls(Node) when is_list(Node) ->
    F = fun(N, Ac) -> erl_syntax_lib:fold(fun acc_calls/2, Ac, N) end,
    lists:foldl(F, [], Node);
get_calls(Node) ->
    get_calls([Node]).


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
    


% This function returns the dependency graph of the program, starting
% from the execution units specified by -units(...).

% An edge f1 ---> f2 is created iff f1 calls f2, and f2 is neither an
% imported function nor a builtin.
get_dependency_graph(#finf{functions = Fs, builtins = Bs}) ->
    DepGraph = digraph:new([private]),
    FsList = maps:to_list(Fs),
    [ digraph:add_vertex(DepGraph, FunArity) || {FunArity, _} <- FsList],
    [ digraph:add_edge(DepGraph, FA1, FA2) || 
        {FA1, {_, _, Body}} <- FsList,
        FA2 <- get_calls(Body),
        not lists:member(FA2, Bs)],
    DepGraph.
    
% Given a list of declarations and a main expression, it build a
% letrec expression (if necessary)
build_letfun([], JMainExp) -> JMainExp;
build_letfun(JDefs, JMainExp) -> 
    #{ expType => letfun,
       defs => JDefs,
       main => JMainExp }.


% This function is needed because jsx transforms lists of characters
% into JSON arrays. That is why we convert strings into binaries whenever
% we need to.

to_bin(X) when is_atom(X) -> X;
to_bin(X) when is_list(X) -> list_to_binary(X).


lit_value_to_JSON(Lit) ->
    case type(Lit) of
        string -> to_bin(concrete(Lit));
        _ -> concrete(Lit)
    end.


% This function combines a function and an arity into a single string "Fun_Arity"
stringifyFA({Fun,Arity}) -> atom_to_list(Fun) ++ "_" ++ integer_to_list(Arity).



% Given a variable, it generates a JSON containing its name and its type.
% In order to get its type, we need a type environment.
variable_to_vartype(ExpVar, TypeEnv) ->
    LineNo = get_pos(get_attrs(ExpVar)),
    Type = maps:get({LineNo, variable_literal(ExpVar)}, TypeEnv, unknown),
    #{name => variable_name(ExpVar), type => to_bin(Type)}.


% It translates the LHS of a let binding into JSON
lhs_to_json(Exp, #finf{typeenv = TypeEnv}) ->
    case type(Exp) of
        variable -> variable_to_vartype(Exp, TypeEnv);
        tuple -> [ variable_to_vartype(SubExp, TypeEnv) 
            || SubExp <- tuple_elements(Exp), type(SubExp) =:= variable 
        ];
        _ -> unknown
    end.


% It translates a let binding  into JSON
binding_to_json(Binding, FileInfo) ->
    case type(Binding) of
        match_expr -> 
            #{lhs => lhs_to_json(match_expr_pattern(Binding), FileInfo),
              rhs => exp_to_json(match_expr_body(Binding), FileInfo)};
        _ -> unknown
    end.

% It translates a pattern into JSON.
% It uses an auxiliary function pattern_to_json/3 that contains a case
% distinction on the kind of the pattern.
pattern_to_json(Exp, FileInfo) -> pattern_to_json(type(Exp), Exp, FileInfo).

% Literal patterns: #{type => json}
pattern_to_json(map_expr, Exp, _FileInfo) ->
    [Field] = map_expr_fields(Exp),
    Type = map_field_exact_name(Field),
    Value = map_field_exact_value(Field),
    #{
        patType => literal,
        type => concrete(Type),
        value => lit_value_to_JSON(Value)
    };

% Constructor patterns without parameters
pattern_to_json(atom, Exp, _FileInfo) ->
    #{
        patType => constructor,
        name => concrete(Exp),
        args => []
    };

% Constructor patterns with parameters
pattern_to_json(list, Exp, FileInfo) ->
    ConsName = concrete(list_head(Exp)),
    % If the tail is not a tuple, we consider that it is a singleton tuple
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


% Variable patterns
pattern_to_json(variable, Exp, #finf{typeenv = TypeEnv}) ->
    JVar = variable_to_vartype(Exp, TypeEnv),
    JVar#{ patType => variable };    
    
% Tuple patterns    
pattern_to_json(tuple, Exp, FileInfo) ->
    #{
        patType => tuple,
        components => [pattern_to_json(P, FileInfo) || P <- tuple_elements(Exp)]
    };

pattern_to_json(_, Exp, _) -> 
    io:format("Unknown pattern: ~s~n", [erl_prettypr:format(Exp)]),
    #{patType => unknown}.



% This function translates a case branch into JSON
branch_to_json(Branch, FileInfo) -> 
    [Pat] = clause_patterns(Branch),
    Body = clause_body(Branch),
    #{
        pattern => pattern_to_json(Pat, FileInfo),
        exp => exp_to_json(Body, FileInfo)
    }.
    

% This function translates an expression (or a list of expressions) into JSON
% Again, it uses an auxiliary function exp_to_json/3 to perform case distinction on
% the kind of the expression.
exp_to_json(Exp, FileInfo) when is_list(Exp) -> exp_to_json(sequence, Exp, FileInfo);
exp_to_json(Exp, FileInfo) -> exp_to_json(type(Exp), Exp, FileInfo).

% Sequence of let bindings: zero bindings
exp_to_json(sequence, [Exp], FileInfo) -> exp_to_json(Exp, FileInfo);
% Sequence of let bindings: one or more bindings
exp_to_json(sequence, Exps, FileInfo) -> 
    % We separate the main expression from the bindings
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

% Literal expression: #{type => value}
exp_to_json(map_expr, Exp, _FileInfo) ->
    {singleton_maps, [Field]} = {singleton_maps, map_expr_fields(Exp)},
    LitType = map_field_assoc_name(Field),
    LitValue = map_field_assoc_value(Field),
    #{
        expType => literal, 
        type => concrete(LitType),
        value => lit_value_to_JSON(LitValue)
    };

% Variable expression.
exp_to_json(variable, Exp, _FileInfo) ->
    #{
        expType => variable,
        variable => variable_name(Exp)
    };    

% Tuple expression.
exp_to_json(tuple, Exp, FileInfo) ->
    #{
        expType => tuple,
        components => [exp_to_json(E, FileInfo) || E <- tuple_elements(Exp)]
    };
    
% Constructor expression (one or more arguments)
exp_to_json(list, Exp, FileInfo) ->
    ConsName = concrete(list_head(Exp)),
    % If the tail is not a tuple, we consider it as a singleton tuple.
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

% Constructor expression (no arguments)
exp_to_json(atom, Exp, _FileInfo) ->
    ConsName = 
        case concrete(Exp) of
            % We convert 'true' and 'false' into binaries, in order to
            % prevent jsx from transforming them into JSON booleans.
            true -> <<"true">>;
            false -> <<"false">>;
            C -> C
        end,
    #{
        expType => constructor,
        name => ConsName,
        args => []
    }; 

% Case distinction expressions
exp_to_json(case_expr, Exp, FileInfo) ->
    Discriminant = case_expr_argument(Exp),
    % We separate the default branch (if any) from the remaining ones.
    {Nondefault, Default} = lists:splitwith(
        fun(Branch) ->
            Pats = clause_patterns(Branch),
            not (length(Pats) =:= 1 andalso type(hd(Pats)) =:= underscore)
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
        

% Function applications
exp_to_json(application, Exp, #finf{namespace = NS} = FileInfo) ->
    Operator = application_operator(Exp),
    Args = application_arguments(Exp),
    JArgs = [ exp_to_json(Arg, FileInfo) || Arg <- Args ],
    case type(Operator) of
        % External function
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
                % Internal function (not imported)
                false -> #{
                            expType => application,
                            external => false,
                            name => to_bin(stringifyFA({FunName, Arity})),
                            arguments => JArgs
                         };
                % External function (imported via '-import')
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


% It transforms a function definition into JSON
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

% It constructs the JSON of a single execution unit.
build_json_for(#finf{module = M, functions = Fs} = FileInfo, DepGraph, {Fun,Arity} = FunArity) ->
    % We get the function definition.
    {_, VarTypes, Body} = maps:get(FunArity, Fs),
    
    % We obtain the function dependencies
    Deps = digraph_utils:reachable_neighbours([FunArity], DepGraph),
    
    % If the function depends on itself then it is recursive.
    % In this case we have to put its body into a separate binding of the letfun
    MainRecursive = lists:member(FunArity, Deps),
    JDepFuns = [ def_to_json(FA, maps:get(FA, Fs), FileInfo) || FA <- Deps ],
    
    MainExp = case MainRecursive of
                 % The main expression is just the definition of the function, if
                 % it is not recursive
                 false -> exp_to_json(Body, FileInfo);
                 
                 % Otherwise, we have already added the main body into the bindings
                 % of the letfun, so we just have to generate the main call
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


% Write a JSON into disk.
write_to_file(ModName, {F,A}, JSON) ->
    FileName = atom_to_list(ModName) ++ "_" 
                ++ atom_to_list(F) ++ "_" ++ integer_to_list(A) ++ ".json",
    {ok, Handle} = file:open(FileName, [write]),
    io:format(Handle, "~ts~n", [jsx:prettify(jsx:encode(JSON))]),
    io:format("Execution unit ~w/~w written into ~s~n", [F, A, FileName]).

% Main function
main([FileName]) ->
    % Obtain info from the source file
    Comments = erl_comment_scan:file(FileName),
    FileInfo = get_metadata(Comments, #finf{}),
    FileInfo2 = parse_file(FileName, FileInfo),
    
    % Build dependency graph
    DepGraph = get_dependency_graph(FileInfo2),

    % For each declared execution unit, we generate the pair {{Fun,Arity}, JSON}
    #finf{units = Us, module = ModName} = FileInfo2,
    JSONReps = [ {U, build_json_for(FileInfo2, DepGraph, U)} || U <- Us ],
    
    % Write the JSON of each execution unit into a separate file.
    [ write_to_file(ModName, U, JSON) || {U, JSON} <- JSONReps ],
    io:format("Done~n");
main(_) -> io:format("Usage: irparser <filename.erl>~n").    
    

