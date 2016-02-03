-module(builtins).

-compile(export_all).

-include("builtins.hrl").

-spec 'len'(heap(), value()) -> ir_int().
len(H, #{loc := LOC}) -> #array{len = L} = maps:get(LOC, H), #{int => L}. 

-spec '<'(ir_int(), ir_int()) -> boolean().
'<'(#{int := X}, #{int := Y}) -> X < Y.

-spec '>='(ir_int(), ir_int()) -> boolean().
'>='(#{int := X}, #{int := Y}) -> X >= Y.

-spec '>'(ir_int(), ir_int()) -> boolean().
'>'(#{int := X}, #{int := Y}) -> X > Y.

-spec '&&'(boolean(), boolean()) -> boolean().
'&&'(X, Y) -> X andalso Y.

-spec 'sel-array'(heap(), value(), ir_int()) -> value().
'sel-array'(H, #{loc := LOC}, #{int := I}) ->
    #array{values = V} = maps:get(LOC,H),
    maps:get(I,V).

-spec '-'(ir_int(),ir_int()) -> ir_int().
'-'(#{int := X}, #{int := Y}) -> #{int => X - Y}.

-spec '+'(ir_int(),ir_int()) -> ir_int().
'+'(#{int := X}, #{int := Y}) -> #{int => X + Y}.

-spec 'mod-array'(heap(), value(), ir_int(), value()) -> heap().
'mod-array'(H, #{loc := LOC}, #{int := I}, V) ->
    Arr = maps:get(LOC, H),
    Vs1 = maps:update(I, V, Arr#array.values),
    maps:update(LOC, Arr#array{values = Vs1}, H).



