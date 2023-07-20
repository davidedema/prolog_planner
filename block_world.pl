:- dynamic block/13.                % block is dynamic
:- dynamic action/1.                % action is dynamic
:- dynamic count/2.                 % count is dynamic
:- dynamic robots/1.                % the robots available
:- dynamic robots_used/1.           % the robots being used
:- use_module(library(clpr)).       % for CLP (For now not using)

%%%%% FACTS %%%%%

% block(ID, X, Y, Z, width, height, depth, orientation, touchL, touchH, shape, made by, linked)
% block(b1, 0.27, -0.26, 0.685, 0.05, 0.05, 0.05, 1, table, air, block, [b1], 0). 
% block(b2, 0.41, -0.26, 0.685, 0.05, 0.05, 0.05, 1, table, air, block, [b2], 0).
% block(b3, 0.27, , 0.685, 0.05, 0.05, 0.05, 1, table, air, block, [b3], 0).
% block(b4, 0.41, 3, 0.685, 0.05, 0.05, 0.05, 1, table, air, block, [b4], 0). 
% block(b1, 0.27, -0.26, 0.685, 0.05, 0.05, 0.05, 2, table, air, block, [b1], 0). 

block(b1, 1, 1, 1, 1, 1, 1, 1, block, air, block, [b1], 0).
block(b2, 1, 1, 0, 1, 1, 1, 1, table, block, block, [b2], 0).
block(b5, 9, 4, 0, 1, 1, 1, 1, table, air, block, [b5], 0).
block(b6, 11, 5, 0, 1, 2, 1, 1, table, air, block, [b6], 0).
block(b7, 13, 6, 1, 1, 2, 1, 3, table, air, block, [b7], 0).
block(b8, 15, 7, 0, 1, 2, 1, 1, table, air, block, [b8], 0).
block(b9, 17, 8, 0, 1, 2, 1, 1, table, air, block, [b9], 0). 

robots(r1).
robots(r2).
robots(r3).
robots(r4).

% count(ID, Counter)
count(s,1).

%%%%% RULES %%%%%

%%% FOR DEBUG %%%

% Print all the characteristics of a block
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

print_blocks([]).
print_blocks([H|T]) :-
    print_block(H),
    print_blocks(T).

assert_backtrack(Term) :-
    (
        assertz(Term, Ref);
        erase(Ref), 
        fail
    ).

%%% CHECKS %%%

% Check if the list elements are all different
all_diff(L) :-
    \+ (select(X,L,R), memberchk(X,R)).

% Return the length of a list
list_length([], 0).

list_length([_|T], N) :-
    list_length(T, N1),
    N is N1 + 1.

%%% FOR RECURSIVE ROTATION %%%

rotate_list([], NO).

rotate_list([H|T], NO) :-
    rotate_compose(H, _, _, _, NO),
    rotate_list(T, NO).

% Rotate a block that normally cannot be rotated (only for mantaining consistency)
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

% Move a block that normally cannot be moved (only for mantaining consistency)
move_compose(Block, X, Y, Z, NX, NY, NZ) :-
    block(Block, X, Y, Z, W, H, D, O, TL, TH, S, MB, L),
    L = 1,
    (list_length(MB, N), N > 1 -> move_list(MB, NX, NY, NZ); NX = NX, NY = NY, NZ = NZ),
    retract(block(Block, X, Y, Z, W, H, D, O, TL, TH, S, MB, L)),
    assertz(block(Block, NX, NY, NZ, W, H, D, O, TL, TH, S, MB, L)).

%%% FOR PILLAR CREATION %%%
% Return all blocks
find_blocks(Blocks) :-
    findall(ID, block(ID,X,Y,Z,W,H,D,O,TL,TH,S,MB,0), Blocks).

find_robots(Robots) :- 
    findall(ID, robot(ID), Robots).

% Return all blocks that satisfy the conditions
valid_blocks(Blocks, DesiredHeight, ResultBlocks, DesiredWidth, DesiredDepth) :-
    length(Blocks, N),               
    between(1, N, NumBlocks),        
    length(ResultBlocks, NumBlocks),  
    select_blocks(Blocks, ResultBlocks, DesiredHeight, DesiredWidth, DesiredDepth). 

find_comb([], 0, []).

find_comb([H|T], DesiredHeight, [H | FinalBlocks]) :-
    block(H, X2, Y2, Z2, W2, H2, D2, O2, TL2, TH2, S2, MB2, L2),
    NewHeight is DesiredHeight - H2,
    NewHeight >= 0,
    find_comb(T, NewHeight, FinalBlocks).

find_comb([_|T], DesiredHeight, FinalBlocks) :-
   find_comb(T, DesiredHeight, FinalBlocks). 

select_blocks(_, [], 0.0, DesiredWidth, DesiredDepth). 
select_blocks(_, [], 0, DesiredWidth, DesiredDepth). 
select_blocks(Blocks, [Block|Remaining], DesiredHeight, DesiredWidth, DesiredDepth) :-
    select(Block, Blocks, RemaningBlocks), 
    block(Block, _, _, _, DesiredWidth, Height, DesiredDepth, _, _, _, _, _, _), 
    DesiredHeight >= Height, 
    RemainingHeight is DesiredHeight - Height,
    select_blocks(RemaningBlocks, Remaining, RemainingHeight, DesiredWidth, DesiredDepth). 


% For recursive stacking
stackRec([], B, X, Y, Z).

stackRec([H|T], B, X, Y, Z) :-
    snapshot(stack(H, B, X, Y, Z, R)),
    stackRec(T, R, X, Y, Z).


%%% FOR PYTHON INTEGRATION
get_blocks(Blocks) :-
    findall(block(ID,X,Y,Z,W,H,D,O,TL,TH,S,MB,L), block(ID,X,Y,Z,W,H,D,O,TL,TH,S,MB,L), Blocks).

%%% FOR PLAN CREATION %%%

add_action(Action) :-
    assertz(action(Action)).

print_actions([]).
print_actions([H|T]) :-
    format("~w ", H),
    print_actions(T).

plan(Actions) :-
    findall(A, action(A), Actions),
    print_actions(Actions),
    retractall(action(_)).


%%% BASE ACTIONS %%%

rotate_block(Block, X, Y, Z, NO) :-
    block(Block, X, Y, Z, W, H, D, O, TL, TH, S, MB, L),
    (list_length(MB, N), N > 1 -> rotate_list(MB, NO); NO = NO),
    L is 0,
    /* writeln('-----Rotate Block-----'),
    format('Block ~w has the orientation ~d ~n', [Block, O]),
    format('Block ~w has to have ~d orientation ~n', [Block,NO]), */
    add_action(rotate(Block, X, Y, Z, NO)),
    retract(block(Block, X, Y, Z, W, H, D, O, TL, TH, S, MB, L)),
    assertz(block(Block, X, Y, Z, W, H, D, NO, TL, TH, S, MB, L)).

intersection([], _Block, _X, _Y, _Z) :-
    fail.

intersection([H|T], Block, X, Y, Z) :-
    (
        H \= Block,
        block(Block, _X1, _Y1, _Z1, W1, H1, D1, _O1, _TL1, _TH1, _S1, _MB1, _L1),
        block(H, X2, Y2, Z2, W2, H2, D2, _O2, _TL2, _TH2, _S2, _MB2, _L2),
        (
            (X >= X2, X =< X2+W2), (X+W1 >= X2, X+W1 =< X2+W2),
            (Y >= Y2, Y =< Y2+D2), (Y+D1 >= Y2, Y+D1 =< Y2+D2),
            (Z >= Z2, Z =< Z2+H2), (Z+H1 >= Z2, Z+H1 =< Z2+H2)
        )
    );
    intersection(T, Block, X, Y, Z).

grip_block(Block, X, Y, Z) :- 
    add_action(grip(Block, X, Y, Z)),
    retract(block(Block, X, Y, Z, W, H, D, O, TL, TH, S, MB, L)).

translate_block(Block, X, Y, Z, NX, NY, NZ) :-
    add_action(move(Block, X, Y, Z, NX, NY, NZ)).

release_block(Block, NX, NY, NZ, W, H, D, O, TL, TH, S, MB, L) :- 
    add_action(release(Block, NX, NY, NZ)),
    assertz(block(Block, NX, NY, NZ, W, H, D, O, TL, TH, S, MB, L)).

move_block(Block, X, Y, Z, NX, NY, NZ) :-
    move_block(Block, X, Y, Z, NX, NY, NZ, false).

move_block(Block, X, Y, Z, NX, NY, NZ, Safe) :-
    block(Block, X, Y, Z, W, H, D, O, TL, TH, S, MB, L),
    %% PRECONDITIONS %%
    (Safe; find_blocks(Blocks), not(intersection(Blocks,Block,NX,NY,NZ))),
    (list_length(MB, N), N > 1 -> move_list(MB, NX, NY, NZ); NX = NX, NY = NY, NZ = NZ),
    L is 0,
    %% EFFECTS %%
    grip_block(Block, X, Y, Z),
    translate_block(Block, X, Y, Z, NX, NY, NZ),
    release_block(Block, NX, NY, NZ, W, H, D, O, TL, TH, S, MB, L).

%move_block(Block, X, Y, Z, NX, NY, NZ, Safe) :-
%    block(Block, X, Y, Z, W, H, D, O, TL, TH, S, MB, L),
%    %% PRECONDITIONS %%
%    (Safe; find_blocks(Blocks), not(intersection(Blocks,Block,NX,NY,NZ))),
%    (list_length(MB, N), N > 1 -> move_list(MB, NX, NY, NZ); NX = NX, NY = NY, NZ = NZ),
%    L is 0,
%    % add_action(move(Block, X, Y, Z, NX, NY, NZ)),
%    %% EFFECTS %%
%    add_action(grip(Block, X, Y, Z)),
%    retract(block(Block, X, Y, Z, W, H, D, O, TL, TH, S, MB, L)),
%    add_action(move(Block, X, Y, Z, NX, NY, NZ)),
%    add_action(release(Block, NX, NY, NZ)),
%    assertz(block(Block, NX, NY, NZ, W, H, D, O, TL, TH, S, MB, L)).

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
    Z1 is Z2 - H2,
    W1 = W2,
    D1 = D2,
    %% POSTCONDITIONS %%
    HighP is H1 + H2,
    add_action(link(B1, B2)),
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
    add_action(unlink(B, B1, B2)),
    retract(block(B1, X1, Y1, Z1, W1, H1, D1, O1, TL1, TH1, S1, MB1, L1)),
    retract(block(B2, X2, Y2, Z2, W2, H2, D2, O2, TL2, TH2, S2, MB2, L2)),
    retract(block(B, X, Y, Z, W, H, D, O, TL, TH, S, MB, L)),
    assertz(block(B1, X1, Y1, Z1, W1, H1, D1, O1, TL1, TH1, S1, MB1, 0)),
    assertz(block(B2, X2, Y2, Z2, W2, H2, D2, O2, TL2, TH2, S2, MB2, 0)).
    

%% COMPOSED ACTIONS %%

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
    NZ is Z - H2,
    (\+ O1 = 1 -> rotate_block(B1, X1, Y1, Z1, 1); true),
    (\+ O2 = 1 -> rotate_block(B2, X2, Y2, Z2, 1); true),
    (\+ (X2 = X, Y2 = Y, Z2 = Z) -> move_block(B2, X2, Y2, Z2, X, Y, Z); true),
    move_block(B1, X1, Y1, Z1, X, Y, NZ),
    link(B1, B2, R).

pillar(X, Y, Z, High, Width, Depth, Actions) :-
    %% PRECONDITIONS %%
    find_blocks(Blocks),
    valid_blocks(Blocks, High, ValidBlocks, Width, Depth),
    print_blocks(ValidBlocks),
    %% POSTCONDITIONS %%
    nth0(0, ValidBlocks, B1),
    nth0(1, ValidBlocks, B2),
    stack(B1, B2, X, Y, Z, R),
    select(BL1, ValidBlocks, ValidBlocks1),
    select(BL2, ValidBlocks1, ValidBlocks2),
    stackRec(ValidBlocks2, R, X, Y, Z),
    plan(Actions).

pillar_from_blocks(X, Y, Z, ValidBlocks, A):-
    nth0(0, ValidBlocks, B1),
    nth0(1, ValidBlocks, B2),
    stack(B1, B2, X, Y, Z, R),
    select(BL1, ValidBlocks, ValidBlocks1),
    select(BL2, ValidBlocks1, ValidBlocks2),
    stackRec(ValidBlocks2, R, X, Y, Z),
    plan(Actions).

all_pillars(X, Y, Z, Height, Width, Depth, Actions) :-
    find_blocks(Blocks),
    find_comb(Blocks, Height, FinalBlocks),
    snapshot(pillar_from_blocks(X,Y,Z,FinalBlocks,Actions)).
    
