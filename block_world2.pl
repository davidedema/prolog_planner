:- dynamic block/9.                % block is dynamic
:- dynamic action/4.                % action is dynamic
:- dynamic plan/1.                  % plan is dynamic
:- dynamic count/2.                 % count is dynamic
:- dynamic agent/1.                 % the list of all robots 
:- dynamic unavailable/1.            % unavailable agents or blocks
:- dynamic appointed/2.
:- use_module(library(clpr)).       % for CLP (For now not using)

%%%%% FACTS %%%%%

% block(ID, X, Y, Z, width, height, depth, orientation, touchL, touchH, shape, made by, linked)
% block(b1, 0.27, -0.26, 0.685, 0.05, 0.05, 0.05, 1, table, air, block, [b1], 0). 
% block(b2, 0.41, -0.26, 0.685, 0.05, 0.05, 0.05, 1, table, air, block, [b2], 0).
% block(b3, 0.27, , 0.685, 0.05, 0.05, 0.05, 1, table, air, block, [b3], 0).
% block(b4, 0.41, 3, 0.685, 0.05, 0.05, 0.05, 1, table, air, block, [b4], 0). 
% block(b1, 0.27, -0.26, 0.685, 0.05, 0.05, 0.05, 2, table, air, block, [b1], 0). 

% block(b1, 1, 1, 1, 1, 1, 1, 1, block, air, block, [b1], 0).
% block(b2, 1, 1, 0, 1, 1, 1, 1, table, block, block, [b2], 0).
% block(b5, 9, 4, 0, 1, 1, 1, 1, table, air, block, [b5], 0).
% block(b6, 11, 5, 0, 1, 2, 1, 1, table, air, block, [b6], 0).
% block(b7, 13, 6, 1, 1, 2, 1, 3, table, air, block, [b7], 0).
% block(b8, 15, 7, 0, 1, 2, 1, 1, table, air, block, [b8], 0).
% block(b9, 17, 8, 0, 1, 2, 1, 1, table, air, block, [b9], 0). 

% block(ID, X, Y, Z, W, H, D, O, Status) 
% Status:
% - 0 Free 
% - 1 Not free -> another block is ontop;
% - 2 Gripped
% - 3 Moving
block(b1, 1, 1, 1, 1, 1, 1, 1, 0).
block(b2, 1, 1, 0, 1, 1, 1, 1, 1).
block(b5, 9, 4, 0, 1, 1, 1, 1, 1).
block(b6, 11, 5, 0, 1, 2, 1, 1, 1).
block(b7, 13, 6, 1, 1, 2, 1, 3, 1).
block(b8, 15, 7, 0, 1, 2, 1, 1, 1).
block(b9, 17, 8, 0, 1, 2, 1, 1, 1). 

agent(a1).
agent(a2).
agent(a3).
agent(a4).

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

%%% CHECKS %%%

% Check if the list elements are all different
all_diff(L) :-
  \+ (select(X,L,R), memberchk(X,R)).

% Return the length of a list
list_length([], 0).

list_length([_|T], N) :-
  list_length(T, N1),
  N is N1 + 1.

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

find_comb_blocks([], 0, []).

find_comb_blocks([H|T], DesiredHeight, [H | FinalBlocks]) :-
  block(H, X2, Y2, Z2, W2, H2, D2, O2, TL2, TH2, S2, MB2, L2),
  NewHeight is DesiredHeight - H2,
  NewHeight >= 0,
  find_comb_blocks(T, NewHeight, FinalBlocks).

find_comb_blocks([_|T], DesiredHeight, FinalBlocks) :-
 find_comb_blocks(T, DesiredHeight, FinalBlocks). 

find_comb_robots_rec([], []).

find_comb_robots_rec([H|T], [H | Permutation]) :-
  robot(H),
  find_comb_robots_rec(T, Permutation)
  .

find_comb_robots_rec([_|T], Permutation) :-
  find_comb_robots_rec(T, Permutation)
  .

find_comb_robots(Robots, Perm) :-
  find_comb_robots_rec(Robots, TempPerm),
  length(TempPerm, Len), 
  (
    (Len == 0, fail);
    (Len == 1, [H | _] = TempPerm, Perm = [H, H]);
    (Len > 1, Perm = TempPerm)
  )
  .


select_blocks(_, [], 0.0, DesiredWidth, DesiredDepth). 
select_blocks(_, [], 0, DesiredWidth, DesiredDepth). 
select_blocks(Blocks, [Block|Remaining], DesiredHeight, DesiredWidth, DesiredDepth) :-
  select(Block, Blocks, RemaningBlocks), 
  block(Block, _, _, _, DesiredWidth, Height, DesiredDepth, _, _, _, _, _, _), 
  DesiredHeight >= Height, 
  RemainingHeight is DesiredHeight - Height,
  select_blocks(RemaningBlocks, Remaining, RemainingHeight, DesiredWidth, DesiredDepth). 


%%% FOR PYTHON INTEGRATION
get_blocks(Blocks) :-
  findall(block(ID,X,Y,Z,W,H,D,O,TL,TH,S,MB,L), block(ID,X,Y,Z,W,H,D,O,TL,TH,S,MB,L), Blocks).

%%% FOR PLAN CREATION %%%

add_action(Action, Arguments, Preconditions, Effects) :-
  assertz(plan([Action, Arguments, Preconditions, Effects])).

print_actions([]).
print_actions([H|T]) :-
  (is_list(H) -> 
    print_actions(H); 
    format('~w ', H)
  ),
  format('~n'),
  print_actions(T).

print_plan() :-
  findall(A, plan(A), Plan),
  print_actions(Plan),
  retractall(plan(_)).

%%% BASE ACTIONS %%%

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

grip_block(Block, Robot, X, Y, Z) :- 
  retract(robot(Robot)),
  add_action(grip(Block, Robot, X, Y, Z)),
  retract(block(Block, X, Y, Z, W, H, D, O, TL, TH, S, MB, L)).

move_block_pre(Block, Robot, X, Y, Z) :-
  block(Block, X, Y, Z, _, _, _, _, _, TC, _, _, _),
  TC = air,
  robot(Robots). % The robot specified must be free, i.e. must exist

move_block_eff(Block, Robot, X, Y, Z):-
  add_action(move(Robot, X, Y, Z)), 
  retract(robot(Robot)) % The robot used is now not free
  .

release_block(Block, Robot, NX, NY, NZ, W, H, D, O, TL, TH, S, MB, L) :- 
  add_action(release(Block, NX, NY, NZ)),
  assertz(block(Block, NX, NY, NZ, W, H, D, O, TL, TH, S, MB, L)),
  assertz(robot(Robot)).

move_block(Block, Robot, X, Y, Z, NX, NY, NZ) :-
  move_block(Block, Robot, X, Y, Z, NX, NY, NZ, false).

move_block(Block, Robot, X, Y, Z, NX, NY, NZ, Safe) :-
  block(Block, X, Y, Z, W, H, D, O, TL, TH, S, MB, L),
  %% PRECONDITIONS %%
  (Safe; find_blocks(Blocks), not(intersection(Blocks,Block,NX,NY,NZ))),
  (list_length(MB, N), N > 1 -> move_list(MB, NX, NY, NZ); NX = NX, NY = NY, NZ = NZ),
  L is 0,
  %% EFFECTS %%
  move_action(Robot, X, Y, Z),
  grip_block(Block, Robot, X, Y, Z),
  % add_action(grip(Block, X, Y, Z)),
  % retract(block(Block, X, Y, Z, W, H, D, O, TL, TH, S, MB, L)),
  move_block(Block, Robot, NX, NY, NZ),
  % add_action(move(Block, X, Y, Z, NX, NY, NZ)),
  release_block(Block, Robot, NX, NY, NZ, W, H, D, O, TL, TH, S, MB, L)
  % add_action(release(Block, NX, NY, NZ)),
  % assertz(block(Block, NX, NY, NZ, W, H, D, O, TL, TH, S, MB, L))
  .

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
  

action( move_arm, 
        [Agent, Block, X, Y, Z], 
        [], 
        [add_appointed(Agent, Block)]
      ).
action( grip, 
        [Agent, Block], 
        [appointed(Agent, Block), block(Block,_,_,_,_,_,_,_,1)], 
        [change_block_status(Block, 2)]
      ).
action( move_block, 
        [Agent, Block, FX, FY, FZ], 
        [block(Block,_,_,_,_,_,_,_,2), appointed(Agent, Block)], 
        [change_block_status(Block, 3)]
      ).
action( release, 
        [Agent, Block, FX, FY, FZ], 
        [appointed(Agent, Block), block(Block,_,_,_,_,_,_,_,3)], 
        [change_block_coord(Block, FX, FY, FZ), del_appointed(Agent, Block), add_agent(Agent)]
      ).

choose_action(Name, Agent, Block, _, _, _, Preconditions, Effects) :-
  action(Name, [Agent, Block], Preconditions, Effects).
choose_action(Name, Agent, Block, X, Y, Z, Preconditions, Effects) :-
  action(Name, [Agent, Block, X, Y, Z], Preconditions, Effects).

validate_preconditions([]).
validate_preconditions([H|T]) :-
  H, 
  validate_preconditions(T).

del_agent(Agent) :-
  retract(agent(Agent)).
del_appointed(Agent, Block) :-
  retract(appointed(Agent, Block)).

add_agent(Agent) :-
  assertz(agent(Agent)).
add_appointed(Agent, Block) :-
  \+ appointed(Agent, Block),
  assertz(appointed(Agent, Block)).

change_block_status(Block, Status) :-
  retract(block(Block, X, Y, Z, W, D, H, O, _S)),
  assertz(block(Block, X, Y, Z, W, D, H, O, Status)).

change_block_coord(Block, FX, FY, FZ) :-
  retract(block(Block, _X, _Y, _Z, W, D, H, O, _S)),
  assertz(block(Block, FX, FY, FZ, W, D, H, O, 1)).

apply_effects([]).
apply_effects([add_agent(A)|T]) :-
  add_agent(A),
  apply_effects(T)
  .
apply_effects([del_agent(A)|T]) :-
  del_agent(A),
  apply_effects(T)
  .
apply_effects([change_block_status(B, S)|T]) :-
  change_block_status(B, S),
  apply_effects(T)
  .
apply_effects([change_block_coord(B, FX, FY, FZ)|T]) :-
  change_block_coord(B, FX, FY, FZ),
  apply_effects(T)
  .
apply_effects([add_appointed(A,B)|T]) :-
  add_appointed(A,B),
  apply_effects(T)
  .
apply_effects([del_appointed(A,B)|T]) :-
  del_appointed(A,B),
  apply_effects(T)
  .

check_pillar(_Moved, []).
check_pillar(Moved, [H|T]) :-
  member(H, Moved),
  check_pillar(Moved, T).


pillar(_, _, _FX, _FY, _FZ, Moved, WouldMove) :-
  list_length(Moved, Len), Len > 0,
  check_pillar(Moved, WouldMove).

pillar([HB|Blocks], [HA|Agents], FX, FY, FZ, Moved, WouldMove) :-
  \+ member(HB, Moved) ->
  (
    choose_action(Name, HA, HB, FX, FY, FZ, Preconditions, Effects),
    validate_preconditions(Preconditions),
    apply_effects(Effects),
    (Name = release -> append([Moved, [HB]], NewMoved); NewMoved = Moved),
    add_action(Name, [HA, HB], Preconditions, Effects),
    (
      pillar([HB|Blocks], [HA|Agents], FX, FY, FZ, NewMoved, WouldMove);
      pillar([HB|Blocks], [Agents], FX, FY, FZ, NewMoved, WouldMove);
      pillar([Blocks], [HA|Agents], FX, FY, FZ, NewMoved, WouldMove);
      pillar([Blocks], [Agents], FX, FY, FZ, NewMoved, WouldMove)
    )
  );
  pillar([Blocks], [HA|Agents], FX, FY, FZ, NewMoved, WouldMove)
  .


% pillar([HB|Blocks], [HA|Agents], FX, FY, FZ, Moved, WouldMove) :-
%   (
%     %If 
%     (
%       \+ member(HB, Moved), 
%       block(HB, _, _, _, _, _, _, _, _),
%       agent(HA)
%     ) -> 
%     %Then 
%     (
%       choose_action(Name, HA, HB, FX, FY, FZ, Preconditions, Effects),
%       validate_preconditions(Preconditions),
%       apply_effects(Effects),
%       (Name = release -> append([Moved, [HB]], NewMoved); NewMoved = Moved),
%       add_action(Name, [HA, HB], Preconditions, Effects),
%       (
%         pillar([HB|Blocks], [HA|Agents], FX, FY, FZ, NewMoved, WouldMove);
%       )
%     ) ;
%     %Else
%     (
%       %If 
%       (\+ (block(HB, _, _, _, _, _, _, _, _); member(HB, Moved)) ->
%       %Then 
%       (
%         \+ agent(HA) -> 
%         pillar([Blocks], [Agents], FX, FY, FZ, Moved, WouldMove);
%         pillar([Blocks], [HA|Agents], FX, FY, FZ, Moved, WouldMove)
%       );
%       %Else
%       (
%         \+ agent(HA) -> 
%         pillar([HB|Blocks], [Agents], FX, FY, FZ, Moved, WouldMove);
%         pillar([HB|Blocks], [HA|Agents], FX, FY, FZ, Moved, WouldMove)
%       )
%     )
%   ).
  
pillar(Blocks, Agents, FX, FY, FZ) :-
  pillar(Blocks, Agents, FX, FY, FZ, [], Blocks).

test :- pillar([b2,b5],[a1,a2],10,10,0), print_plan().
