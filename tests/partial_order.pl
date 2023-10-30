:- include('../planner.pl').

action(
  myAction(A, B),
  [],
  [],
  [],
  [],
  [add(available(A)), add(clear(B))]
).

% Partial order of no preconditions over no actions should be an empty list
test0 :- 
  partial_order([], [], T),
  T == [].

% Partial order of a precondition over an empty list of actions, should be an empty list
test1 :- 
  partial_order([available(a1)], [], T),
  T == [].

% Partial order of a precondition over a list of actions containing the correct action, should be the position of the action
test2 :- 
  partial_order([available(a1)], [myAction(a1,b2)], T),
  T == [1].

% Partial order of a precondition over a list of actions not containing the correct action, should an empty list
test3 :- 
  partial_order([available(a1)], [myAction(a2,b2)], T),
  T == [].

% Partial order of a precondition over a list of actions containing multiple times the correct effects, should be the positions of the actions
test4 :- 
  partial_order([available(a1)], [myAction(a1, b2), myAction(a2, b2), myAction(a1,b2), myAction(a2, b2), myAction(a2, b2)], T),
  T == [5,3].

% Partial order of a list of preconditions over a list of actions containing multiple times the correct effects, should be the positions of the actions
test5 :- 
  partial_order([available(a1), clear(b3)], [myAction(a2, b3), myAction(a2, b2), myAction(a1,b2), myAction(a2, b2), myAction(a2, b2)], T),
  T == [5,3].

test :- 
  write('Running test0: '), test0, write('passed'), nl,
  write('Running test1: '), test1, write('passed'), nl,
  write('Running test2: '), test2, write('passed'), nl,
  write('Running test3: '), test3, write('passed'), nl,
  write('Running test4: '), test4, write('passed'), nl,
  write('Running test5: '), test5, write('passed'), nl,
  true.

