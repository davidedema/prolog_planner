:- include('../planner.pl').

% The lists are empty -> false
test0 :- \+achiever([], [], []).

% Predicate over empty list -> false
test1 :- \+achiever([available(a1)], [], []).

% Correct predicate over correct list -> true
test2 :- achiever([available(a2)], [add(available(a2))], []).

% Correct predicate over list containing wrong predicate -> false 
test3 :- \+achiever([available(a2)], [add(clear(a2))], []).

% Correct predicate over list containing correct predicate but wrong argument -> false
test4 :- \+achiever([available(a2)], [add(available(a1))], []).

% Correct predicate over list containing multiple predicates of which one correct -> true
test5 :- achiever([available(a2)], [add(clear(b2)), add(available(a2)), del(clear(b1))], []).

% Correct predicate over list containing multiple predicates of which none correct -> false
test6 :- \+achiever([available(a2)], [add(clear(b2)), add(available(a1)), add(clear(b1))], []).

% List of predicates over list containing multiple predicates of which none correct -> false
test7 :- \+achiever([available(a2), clear(b3)], [add(clear(b2)), add(available(a1)), add(clear(b1))], []).

% List of predicates over list containing multiple predicates of which one correct -> true
test8 :- achiever([available(a2), clear(b3)], [add(clear(b2)), add(available(a1)), add(clear(b3))], []).

test :-
  write("Running test0"), nl, test0,
  write("Runnign test1"), nl, test1,
  write("Running test2"), nl, test2,
  write("Running test3"), nl, test3,
  write("Running test4"), nl, test4,
  write("Runnign test5"), nl, test5,
  write("Runnign test6"), nl, test6,
  write("Runnign test7"), nl, test7,
  write("Runnign test8"), nl, test8.
