-record(array, {len :: non_neg_integer(), values :: #{non_neg_integer() => value()}}).

-type loc()        :: non_neg_integer().
-type ir_int()     :: #{int => integer()}.
-type heap_value() :: #array{}.
-type heap()       :: #{loc() => heap_value()}.
-type value()      :: ir_int() | #{bool => boolean()} | #{loc => loc()}.


