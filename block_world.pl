:- dynamic block/13.
:- dynamic count/2.
:- use_module(library(clpr)).    % for CLP

% todo: REGOLA ESISTE!!!, MANCA CHECK SULLA COMPATIBILITÀ DELLE DIMENSIONI

%%%%% FACTS %%%%%

% block(ID, X, Y, Z, width, height, depth, orientation, touchL, touchH, shape, made by, linked)
block(b1, 1, 0, 0, 1, 2, 1, 1, table, air, block, [b1], 0).
block(b2, 1, 0, 1, 1, 2, 1, 3, table, air, block, [b2], 0).
block(b3, 2, 0, 0, 1, 2, 1, 1, table, air, block, [b3], 0).
block(b4, 0, 3, 0, 1, 2, 1, 1, table, air, block, [b4], 0).
block(b5, 0, 0, 0, 1, 2, 1, 1, table, air, block, [b5], 0).

% count(ID, Counter)
count(s,1).

%%%%% RULES %%%%%

%%% FOR DEBUG %%%

print_block(Block) :- 
    block(Block, X, Y, Z, W, D, H, O, TL, TH, S, MB, L),
    format('Block; ~w:~n', [Block]),
    format('Coordinate: ~w, ~w, ~w~n', [X, Y, Z]),
    format('Dimensions: ~w, ~w, ~w~n', [W, D, H]),
    format('Orientation: ~w~n', [O]),
    format('Touching Low: ~w~n', [TL]),
    format('Touching High: ~w~n', [TH]),
    format('Shape: ~w~n', [S]),
    format('Made By: ~w~n', [MB]),
    format('Linked: ~w~n', [L]).

%%% CHECKS %%%

all_diff(L) :-
    \+ (select(X,L,R), memberchk(X,R)).

list_length([], 0).

list_length([_|T], N) :-
    list_length(T, N1),
    N is N1 + 1.

%%% FOR RECURSIVE ROTATION %%%

rotate_list([], NO).

rotate_list([H|T], NO) :-
    rotate_compose(H, _, _, _, NO),
    rotate_list(T, NO).

rotate_compose(Block, X, Y, Z, NO) :-
    block(Block, X, Y, Z, W, H, D, O, TL, TH, S, MB, L),
    (list_length(MB, N), N > 1 -> rotate_list(MB, NO); NO = NO),
    retract(block(Block, X, Y, Z, W, H, D, O, TL, TH, S, MB, L)),
    assertz(block(Block, X, Y, Z, W, H, D, NO, TL, TH, S, MB, L)).

%%% FOR RECURSIVE MOTION %%%

move_list([], NX, NY, NZ).

move_list([H|T], NX, NY, NZ) :-
    block(H, X, Y, Z, W, H1, D, O, TL, TH, S, MB, L),
    Z1 is NZ+Z,
    move_compose(H, X, Y, Z, NX, NY, Z1),
    move_list(T, NX, NY, NZ).

move_compose(Block, X, Y, Z, NX, NY, NZ) :-
    block(Block, X, Y, Z, W, H, D, O, TL, TH, S, MB, L),
    L = 1,
    (list_length(MB, N), N > 1 -> move_list(MB, NX, NY, NZ); NX = NX, NY = NY, NZ = NZ),
    retract(block(Block, X, Y, Z, W, H, D, O, TL, TH, S, MB, L)),
    assertz(block(Block, NX, NY, NZ, W, H, D, O, TL, TH, S, MB, L)).

%%% FOR PILLAR CREATION %%%
find_blocks(Blocks) :-
    findall(block(ID,X,Y,Z,W,H,D,O,TL,TH,S,MB,0), block(ID,X,Y,Z,W,H,D,O,TL,TH,S,MB,0), Blocks).

get_valid_blocks(L, HighP, NewP, ValidBlocks) :- % Caso base: non ci sono blocchi disponibili
    NewP = HighP,
    exclude(var, ValidBlocks, CleanedValidBloks).

get_valid_blocks([block(ID,X,Y,Z,W,H,D,O,TL,TH,S,MB,L)|Rest], HighP, NewP, [Block|ValidBlocks]) :- % Se il blocco e` valido, lo aggiungo alla lista
    NewP1 is NewP + H,
    (NewP1 =< HighP -> Block = block(ID,X,Y,Z,W,H,D,O,TL,TH,S,MB,L), get_valid_blocks(Rest, HighP, NewP1, ValidBlocks); get_valid_blocks(Rest, HighP, NewP, ValidBlocks)).

stackRec([], B, X, Y, Z).

stackRec([H|T], B, X, Y, Z) :-
    block(B1, X1, Y1, Z1, W1, H1, D1, O1, TL1, TH1, S1, MB1, L1) = H,
    stack(B1, B, X, Y, Z, R),
    stackRec(T, R, X, Y, Z).



%%% ACTIONS %%%

rotate_block(Block, X, Y, Z, NO) :-
    block(Block, X, Y, Z, W, H, D, O, TL, TH, S, MB, L),
    (list_length(MB, N), N > 1 -> rotate_list(MB, NO); NO = NO),
    L is 0,
    writeln('-----Rotate Block-----'),
    format('Block ~w has the orientation ~d ~n', [Block, O]),
    format('Block ~w has to have ~d orientation ~n', [Block,NO]),
    retract(block(Block, X, Y, Z, W, H, D, O, TL, TH, S, MB, L)),
    assertz(block(Block, X, Y, Z, W, H, D, NO, TL, TH, S, MB, L)).

move_block(Block, X, Y, Z, NX, NY, NZ) :-
    block(Block, X, Y, Z, W, H, D, O, TL, TH, S, MB, L),
    (list_length(MB, N), N > 1 -> move_list(MB, NX, NY, NZ); NX = NX, NY = NY, NZ = NZ),
    L is 0,
    writeln('-----Move block...-----'),
    format('Block ~w was in x:~2f y:~2f z:~2f ~n',[Block, X, Y, Z]),
    format('Has to be moved in in x:~2f y:~2f z:~2f ~n', [NX, NY, NZ]),
    retract(block(Block, X, Y, Z, W, H, D, O, TL, TH, S, MB, L)),
    assertz(block(Block, NX, NY, NZ, W, H, D, O, TL, TH, S, MB, L)).

link(B1, B2, R) :-
    block(B1, X1, Y1, Z1, W1, H1, D1, O1, TL1, TH1, S1, MB1, L1),
    block(B2, X2, Y2, Z2, W2, H2, D2, O2, TL2, TH2, S2, MB2, L2),
    count(s, N),
    %% PRECONDITIONS %%
    all_diff([B1, B2]),
    L1 = 0,
    L2 = 0,
    X1 = X2,
    Y1 = Y2,
    Z1 is Z2 + H2,
    W1 = W2,
    D1 = D2,
    %% POSTCONDITIONS %%
    HighP is H1 + H2,
    retract(block(B1, X1, Y1, Z1, W1, H1, D1, O1, TL1, TH1, S1, MB1, L1)),
    retract(block(B2, X2, Y2, Z2, W2, H2, D2, O2, TL2, TH2, S2, MB2, L2)),
    assertz(block(B1, X1, Y1, Z1, W1, H1, D1, O1, B2, TH1, S1, MB1, 1)),
    assertz(block(B2, X2, Y2, Z2, W2, H2, D2, O2, TL2, B1, S2, MB2, 1)),
    string_concat('s', N, PIL),
    retract(count(s, N)),
    N1 is N+1,
    assertz(count(s, N1)),
    atom_string(STACK,PIL),
    R = STACK,
    assertz(block(STACK, X2, Y2, Z2, W1, HighP, D1, 1, TL2, TH1, block, [B1,B2],0)).

unlink(B, X, Y, Z, B1, B2) :-
    block(B, X, Y, Z, W, H, D, O, TL, TH, S, MB, L),
    %% PRECONDITIONS %%
    L = 0,
    (list_length(MB, N), N > 1),
    %% POSTCONDITIONS %%    
    select(B1, MB, MBN1),
    select(B2, MBN1, MBN12),
    retract(block(B1, X1, Y1, Z1, W1, H1, D1, O1, TL1, TH1, S1, MB1, L1)),
    retract(block(B2, X2, Y2, Z2, W2, H2, D2, O2, TL2, TH2, S2, MB2, L2)),
    retract(block(B, X, Y, Z, W, H, D, O, TL, TH, S, MB, L)),
    assertz(block(B1, X1, Y1, Z1, W1, H1, D1, O1, TL1, TH1, S1, MB1, 0)),
    assertz(block(B2, X2, Y2, Z2, W2, H2, D2, O2, TL2, TH2, S2, MB2, 0)).
    

stack(B1, B2, X, Y, Z, R) :-
    block(B1, X1, Y1, Z1, W1, H1, D1, O1, TL1, TH1, S1, MB1, L1),
    block(B2, X2, Y2, Z2, W2, H2, D2, O2, TL2, TH2, S2, MB2, L2),
    %% PRECONDITIONS %%
    all_diff([B1, B2]),
    W1 = W2,
    D1 = D2,
    TL1 = 'table',
    TH2 = 'air',
    %% POSTCONDITIONS %%
    NZ is Z + H2,
    (\+ O1 = 1 -> rotate_block(B1, X1, Y1, Z1, 1); true),
    (\+ O2 = 1 -> rotate_block(B2, X2, Y2, Z2, 1); true),
    move_block(B2, X2, Y2, Z2, X, Y, Z),
    move_block(B1, X1, Y1, Z1, X, Y, NZ),
    link(B1, B2, R).

pillar(X, Y, Z, High) :-
    %% PRECONDITIONS %%
    find_blocks(Blocks),
    get_valid_blocks(Blocks, High, 0, ValidBlocks), 
    %% POSTCONDITIONS %%
    nth0(0, ValidBlocks, BL1),
    nth0(1, ValidBlocks, BL2),
    block(B1, X1, Y1, Z1, W1, H1, D1, O1, TL1, TH1, S1, MB1, L1) = BL1,
    block(B2, X2, Y2, Z2, W2, H2, D2, O2, TL2, TH2, S2, MB2, L2) = BL2,
    stack(B1, B2, X, Y, Z, R),
    select(BL1, ValidBlocks, ValidBlocks1),
    select(BL2, ValidBlocks1, ValidBlocks2),
    stackRec(ValidBlocks2, R, X, Y, Z).



