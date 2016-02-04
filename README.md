# Intermediate representation parser
The CAVI-ART verification platform provides an intermediate representation (IR) to which programs written in other languages can be translated. Program verification is subsequently carried out in this IR.
More details are given in the following paper: [LOSPTR 2015](http://link.springer.com/chapter/10.1007/978-3-319-27436-2_14)

The IR can be represented syntactically as a subset of Erlang. The irparser executable transforms a program written in this subset into a JSON representation, the latter being handled by other tools of the CAVI-ART platform.

##Build instructions
###Requirements
* Erlang/OTP 18 or later [[download here]](www.erlang.org).
* rebar3 [[download here]](www.rebar3.org).

###Download and compile
    > git clone git://github.com/manuelmontenegro/irparser
    > cd irparser
    > rebar3 compile
    > rebar3 escriptize
The latter command will create an executable `irparser` in the `_build/default/bin` directory.

##Example

The following example is an implementation of the insertion sort algorithm.

```erlang
    -module(insort).
    
    -export([inssort/2]).
    
    -units([inssort/2]).
    
    -import(builtins, ['<'/2, len/2, '>='/2, '>'/2, '&&'/2, 'sel-array'/3, '-'/2, '+'/2, 'mod-array'/4]).
    -include("builtins.hrl").
    
    
    f1(V,H) ->                              % V :: loc, I :: int, H :: heap
        I = #{int => 0},                    % I :: int
        f2(V,I,H).        
        
        
    f2(V,I,H) ->                            % V :: loc, I :: int, H :: heap
        X1 = 'len'(H,V),                    % X1 :: int
        B = '<'(I,X1),                      % B :: bool
        case B of
            true -> f3(V,I,H);
            false -> H
        end.
    
    f3(V,I,H) ->                            % V :: loc, I :: int, H :: heap
        J = '-'(I,#{int => 1}),             % J :: int
        f4(V,I,J,H).
    
    % -assert("X >= 3").
    f4(V,I,J,H) ->                          % V :: loc, I :: int, J :: int, H :: heap
        B1 = '>='(J, #{int => 0}),          % B1 :: bool
        case B1 of
            false -> f6(V,I,H);
            true -> 
                VJ = 'sel-array'(H,V,J),    % VJ :: int
                J1 = '+'(J,#{int => 1}),    % J1 :: int
                VJ1 = 'sel-array'(H,V,J1),  % VJ1 :: int
                B2 = '>'(VJ, VJ1),          % B2 :: bool
                case B2 of
                    false -> f6(V,I,H);
                    true -> f5(V,I,J,H)
                end
        end.
        
    f5(V,I,J,H) ->                          % V :: loc, I :: int, J :: int, H :: heap
        E = 'sel-array'(H,V,J),             % E :: int
        J1 = '+'(J, #{int => 1}),           % J1 :: int
        E2 = 'sel-array'(H,V,J1),           % E2 :: int
        H1 = 'mod-array'(H,V,J,E2),         % H1 :: loc
        H2 = 'mod-array'(H1,V,J1,E),        % H2 :: loc
        J2 = '-'(J,#{int => 1}),            % J2 :: int
        f4(V,I,J2,H2).
    
    f6(V,I,H) ->                            % V :: loc, I :: int, H :: heap
        I1 = '+'(I,#{int => 1}),            % I1 :: int
        f2(V,I1,H).    
            
    
        
    inssort(V,H) -> f1(V,H).                % V :: loc, H :: heap
    
    
    % TEST:
    
    -spec init_heap() -> heap().    
    init_heap() -> 
        #{1 => 
            #array{len = 5, 
                values = #{0 => #{int => 10},
                           1 => #{int => 7},
                           2 => #{int => 1},
                           3 => #{int => 4},
                           4 => #{int => 3}}
                  }
        }.
        
    t1() -> inssort(#{loc => 1}, init_heap()).
```    

## TO DO
* Description of the subset of Erlang supported.
* Description of the JSON representation.
