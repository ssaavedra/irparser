-module(len).

-export([len/1]).

-units([len/1]).

-import(builtins, ['+'/2]).
-include("builtins.hrl").

len(Xs) ->                          % Xs :: list
    case Xs of
        nil -> #{int => 0};
        [cons|{_Y,Ys}] ->           % _Y :: int, Ys :: list
            L1 = len(Ys),           % L1 :: int
            '+'(L1, #{int => 1})
    end.


