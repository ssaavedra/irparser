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
				    => {lineno(), [{varname(), vartype()}], [assertion()], expression()}}
	       }).
