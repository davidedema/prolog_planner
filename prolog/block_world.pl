:- dynamic block/12.
:- dynamic count/2.
:- use_module(library(clpfd)).    % for CLP


%%%%% FACTS %%%%%

% block(ID, X, Y, Z, width, depth, height, orientation, touchL, touchH, shape, made by)
block(b1, 1, 0, 0, 1, 2, 1, 1, table, air, block, [b1]).
block(b2, 1, 1, 0, 1, 2, 1, 3, table, air, block, [b2]).
block(b3, 2, 0, 0, 1, 2, 1, 1, table, air, block, [b3]).
block(b4, 0, 3, 0, 1, 2, 1, 1, table, air, block, [b4]).
block(b5, 0, 0, 0, 4, 1, 4, 1, table, air, block, [b5]).

% count(ID, Counter)
count(s,1).

%%%%% RULES %%%%%

%%% FOR DEBUG %%%

print_block(Block) :- 
    block(Block, X, Y, Z, W, D, H, O, TL, TH, S, MB),
    format('Blocco ~w:~n', [Block]),
    format('Coordinate: ~w, ~w, ~w~n', [X, Y, Z]),
    format('Dimensions: ~w, ~w, ~w~n', [W, D, H]),
    format('Orientation: ~w~n', [O]),
    format('Touching Low: ~w~n', [TL]),
    format('Touching High: ~w~n', [TH]),
    format('Shape: ~w~n', [S]),
    format('Made By: ~w~n', [MB]).

%%% ACTIONS %%%
%% aggiungere xyz %%
rotate_block(Block, DesiredOrientation, NewOrientation) :-
    writeln('-----Rotate Block-----'),
    format('Block ~w has to have ~d orientation ~n', [Block,DesiredOrientation]),
    NewOrientation is DesiredOrientation.

move_block(Block, X, Y, Z, NX, NY, NZ) :-
    block(Block, X1, Y1, Z1, W, H, D, O, TL, TH, S, MB),
    writeln('-----Move block...-----'),
    format('Block ~w was in x:~2f y:~2f z:~2f ~n',[Block, X1, Y1, Z1]),
    format('Has to be moved in in x:~2f y:~2f z:~2f ~n', [X, Y, Z]),
    NX is X,
    NY is Y,
    NZ is Z.
    
%%% CHECKS %%%

all_diff(L) :-
    \+ (select(X,L,R), memberchk(X,R)).

stack(B1, B2) :-
    % blocks
    block(B1, X1, Y1, Z1, W1, H1, D1, O1, TL1, TH1, S1, MB1),
    block(B2, X2, Y2, Z2, W2, H2, D2, O2, TL2, TH2, S2, MB2),
    count(s, N),
    %% PRECONDITIONS %%
    all_diff([B1, B2]),
    
    W1 = W2,
    D1 = D2,
    CL2 = 'table',
    CH1 = 'air',
    S1 = 'block',
    S2 = 'block',
    
    %% POSTCONDITIONS %%
    ((\+ O1 = 1) -> rotate_block(B1, 1, NO1); NO1 = 1),
    ((\+ O2 = 1) -> rotate_block(B2, 1, NO2); NO2 = 1),
    
    ((X1 \= X2; Y1 \= Y2) -> move_block(B2, X1, Y1, (Z1+H1), NX2, NY2, NZ2);move_block(B2, X2, Y2, (Z1+H1), NX2, NY2, NZ2)),
    HighP is H1 + H2,
    writeln('-----Stacking blocks...-----'),
    format('Block ~w was in x:~2f y:~2f z:~2f ~n', [B1, X1, Y1, Z1]),
    format('Block ~w is in x:~2f y:~2f z:~2f ~n', [B2, X1, Y1, H1]),
    format('Stack is in x:~2f y:~2f and is ~2f tall ~n', [X1, Y1, HighP]),
    
    %% UPDATE KNOWLEDGE BASE %%
    retract(block(B1, X1, Y1, Z1, W1, H1, D1, O1, TL1, TH1, S1, MB1)),
    retract(block(B2, X2, Y2, Z2, W2, H2, D2, O2, TL2, TH2, S2, MB2)), 


    % 
    assertz(block(B1, X1, Y1, Z1, W1, H1, D1, NO1, TL1, B2, S1, MB1)),
    assertz(block(B2, NX2, NY2, NZ2, W2, H2, D2, NO2, B1, TH2, S2, MB2)),

    string_concat('s', N, PIL),
    retract(count(s, N)),
    N1 is N+1,
    assertz(count(s, N1)),
    assertz(block(PIL, X1, Y1, Z1, W1, HighP, D1, 1, TL1, TH2, block, [B1,B2])).
    


