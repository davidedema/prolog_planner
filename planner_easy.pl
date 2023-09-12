%%%%%%%%% Simple Prolog Planner %%%%%%%%
%%%
%%% This is one of the example programs from the textbook:
%%%
%%% Artificial Intelligence: 
%%% Structures and strategies for complex problem solving
%%%
%%% by George F. Luger and William A. Stubblefield
%%% 
%%% Corrections by Christopher E. Davis (chris2d@cs.unm.edu)
%%%
%%% These programs are copyrighted by Benjamin/Cummings Publishers.
%%%
%%% We offer them for use, free of charge, for educational purposes only.
%%%
%%% Disclaimer: These programs are provided with no warranty whatsoever as to
%%% their correctness, reliability, or any other property.  We have written 
%%% them for specific educational purposes, and have made no effort
%%% to produce commercial quality computer programs.  Please do not expect 
%%% more of them then we have intended.
%%%
%%% This code has been tested with SWI-Prolog (Multi-threaded, Version 5.2.13)
%%% and appears to function as intended.

:- [adts].

:- dynamic ontable/3.
:- dynamic available/1.
:- dynamic on/4.
:- dynamic clear/1.
:- dynamic pos/2.
:- discontiguous action/6.

pos(1,2).
pos(1,3).
pos(2,1).
pos(2,3).

ground_g([]).
ground_g([H|T]) :-
	assertz(H),
	ground_g(T).

partial_order(_Preconditions, [], [], -1).

partial_order(Preconditions, [S|T], [I|Times], I) :-
	conditions_met(Preconditions, S),
	NewI is I-1,
	partial_order(Preconditions, T, Times, NewI).

partial_order(Preconditions, [S|T], Times, I) :-
	\+conditions_met(Preconditions, S),
	NewI is I-1,
	partial_order(Preconditions, T, Times, NewI).

partial_order(Preconditions, States, Times) :-
	length(States, NStates),
	N is NStates - 1,
	partial_order(Preconditions, States, Times, N).

is_final_state([], _Goal).

is_final_state([diff(X, X1, Y, Y1)|T], Goal) :-
	\+((X == X1, Y == Y1)),
	is_final_state(T, Goal).

is_final_state([H|T], Goal) :-
	is_final_state(T, Goal),
	member_set(H, Goal),
	H.

verify([]).
verify([H|T]) :-
	H,
	verify(T),
	H.

plan(State, Goal, _, Actions, Times, _MaxDepth) :- 	
	equal_set(State, Goal),
	write('actions are'), nl,
	%reverse_print_stack(Actions),
	reverse_print_actions_times(Actions, Times)
	%true
	.

plan(State, Goal, Been_list, Actions, Times, MaxDepth) :- 	
	length(Actions, Len), Len < MaxDepth,
	action(Name, PreconditionsT, PreconditionsF, FinalConditionsF, Effects, Verify),
	verify(Verify),
	conditions_met(PreconditionsT, State),
	conditions_not_met(PreconditionsF, State),
	conditions_not_met(FinalConditionsF, Goal),
	change_state(State, Effects, Child_state),
	testPlan(Child_state, Goal, Been_list, Name, Actions, PreconditionsT, Times, MaxDepth)
	.	

testPlan(Child_state, Goal, Been_list, Name, Actions, PreconditionsT, Times, MaxDepth) :-
	\+equal_set(Child_state, Goal),
	not(member_state(Child_state, Been_list)),
	stack(Child_state, Been_list, New_been_list),
	stack(Name, Actions, New_actions),
	partial_order(PreconditionsT, New_been_list, Time),
	stack(Time, Times, New_Times),
	plan(Child_state, Goal, New_been_list, New_actions, New_Times, MaxDepth).

testPlan(Child_state, Goal, Been_list, Name, Actions, PreconditionsT, Times, MaxDepth) :-
	equal_set(Child_state, Goal),
	stack(Child_state, Been_list, New_been_list),
	stack(Name, Actions, New_actions),
	partial_order(PreconditionsT, New_been_list, Time),
	stack(Time, Times, New_Times),
	plan(Child_state, Goal, New_been_list, New_actions, New_Times, MaxDepth).

change_state(S, [], S).
change_state(S, [add(P)|T], S_new) :-	
	change_state(S, T, S2),
	add_to_set(P, S2, S_new),
	!.
change_state(S, [del(P)|T], S_new) :-	change_state(S, T, S2),
	remove_from_set(P, S2, S_new),
	!.

conditions_met([], _S).
conditions_met([H|T], S) :- 
	member_set(H,S),
	conditions_met(T, S).

conditions_not_met([], _).
conditions_not_met([H|T], S) :- 
	\+member_set(H, S),
	conditions_not_met(T, S)
	.

member_state(S, [H|_]) :- 	equal_set(S, H).
member_state(S, [_|T]) :- 	member_state(S, T).

reverse_print_stack(S) :- 	empty_stack(S).
reverse_print_stack(S) :- 	stack(E, Rest, S), 
	reverse_print_stack(Rest),
	write(E), nl.

reverse_print_actions_times(Actions, Times) :-
	length_stack(Actions, Len), length_stack(Times, Len),
	I is Len,
	reverse_print_actions_times(Actions, Times, I).

reverse_print_actions_times(Actions, Times, _I) :-
	empty_stack(Actions),
	empty_stack(Times).

reverse_print_actions_times(Actions, Times, I) :-
	stack(M, TActions, Actions), 
	stack(T, TTimes, Times),
	NewI is I - 1,
	reverse_print_actions_times(TActions, TTimes, NewI),
	format("[~w]\t~w ~w~n", [I, M, T]).

diffPos(X, X1, _Y, _Y1) :-
	X \== X1.
diffPos(X, X1, Y, Y1) :-
	X == X1, Y \== Y1.
diffPos(X, X1, Y, Y1) :-
	X == X1, Y == Y1, fail.

action(
	grip_on_start(A, B), 
	[on(B, B1, X, Y), available(A), clear(B)],
	[gripped(_, B), gripping(_, B)],
	[on(B, B1, X, Y)],
	[del(available(A)), add(gripping(A, B))],
	[]
).
action(
	grip_ontable_start(A, B), 
	[ontable(B, X, Y), available(A), clear(B)],
	[gripped(_, B), gripping(_, B)],
	[ontable(B, X, Y)],
	[del(available(A)), add(gripping(A, B))],
	[]
).
action(
	grip_buffer_start(A, B), 
	[buffer(B, _X, _Y), available(A), clear(B)],
	[gripped(_, B), gripping(_, B)],
	[],
	[del(available(A)), add(gripping(A, B))],
	[]
).
action(
	grip_end(A, B), 
	[gripping(A, B)],
	[notavalidpredicate(A)],
	[notavalidpredicate(A)],
	[del(clear(B)), del(gripping(A, B)), add(gripped(A, B))],
	[]
).

action(
	release_start(A, B),
	[moved(A, B, X, Y, X1, Y1), gripped(A, B)],
	[],
	[],
	[
		del(moved(A, B, X, Y, X1, Y1)), del(gripped(A, B)),
		add(releasing(A, B))
	],
	[]
).

action(
	release_end(A, B),
	[releasing(A, B)],
	[],
	[],
	[
		del(releasing(A, B)),
		add(clear(B)), add(available(A))
	],
	[]
).

action(
	move_block_on_start(A, B, X, Y, X1, Y1),
	[on(B, B1, X, Y)],
	[],
	[],
	[
		del(on(B, B1, X, Y)),
		add(clear(B1)), add(moving(A, B, X, Y, X1, Y1))
	],
	[]
).
action(
	move_block_ontable_start(A, B, X, Y, X1, Y1),
	[ontable(B, X, Y), gripped(A, B)],
	[],
	[],
	[
		del(ontable(B, X, Y)) ,
		add(moving(A, B, X, Y, X1, Y1))
	],
	[]
).
action(
	move_block_buffer_start(A, B, X, Y, X1, Y1),
	[buffer(B, X, Y), gripped(A, B)],
	[],
	[],
	[
		del(buffer(B, X, Y)),
		add(moving(A, B, X, Y, X1, Y1))
	],
	[]
).

action(
	move_block_to_table_end(A, B, X, Y, X1, Y1),
	[moving(A, B, X, Y, X1, Y1)],
	[],
	[],
	[
		del(moving(A, B, X, Y, X1, Y1)),
		add(ontable(B, X1, Y1)), add(moved(A, B, X, Y, X1, Y1))
	],
	[ontable(B, X1, Y1), diffPos(X, X1, Y, Y1)]
).
action(
	move_block_to_on_end(A, B, X, Y, X1, Y1),
	[moving(A, B, X, Y, X1, Y1), clear(B1)],
	[],
	[],
	[
		del(moving(A, B, X, Y, X1, Y1)), del(clear(B1)),
		add(on(B, B1, X1, Y1)), add(moved(A, B, X, Y, X1, Y1))
	],
	[on(B, B1, X1, Y1), diffPos(X, X1, Y, Y1)]
).
action(
	move_block_to_buffer_end(A, B, X, Y, X1, Y1),
	[moving(A, B, X, Y, X1, Y1)],
	[buffer(_B, X1, Y1)],
	[],
	[
		del(moving(A, B, X, Y, X1, Y1)),
		add(buffer(B, X1, Y1)), add(moved(A, B, X, Y, X1, Y1))
	],
	[pos(X1, Y1), diffPos(X, X1, Y, Y1)]
).


test_plan_result(R, _S, _G, _StateList, _Actions, _Times, _MaxTime) :-
	R.
test_plan_result(R, S, G, _StateList, _Actions, _Times, MaxTime) :-
	\+ R,
	try_plan(S, G, [S], [], [], MaxTime).

try_plan(S, G, StateList, Actions, Times, MaxTime) :-
	NewMaxTime is MaxTime + 10, 
	write("Testing for "), write(NewMaxTime), nl,
	NewMaxTime < 50,
	(plan(S, G, StateList, Actions, Times, NewMaxTime) -> Res = true; Res = false),
	test_plan_result(Res, S, G, StateList, Actions, Times, NewMaxTime).

go(S, G) :- 
	retractall(ontable(_, _, _)),
	retractall(on(_, _, _, _)),
	retractall(clear(_)),
	retractall(available(_)),
	ground_g(G), 
	plan(S, G, [S], [], [], 20).
%try_plan(S, G, [S], [], [], 0).

% from b1, b2 on the table to b2,b1 stacked.
test01 :- go(
						[available(a1), available(a2), ontable(b1, 2, 2), clear(b1)],
 	          [available(a1), available(a2), ontable(b1, 3, 3), clear(b1)]
 	        ).

test0 :- go(
						[available(a1), ontable(b1, 2, 2), clear(b1)],
 	          [available(a1), ontable(b1, 3, 3), clear(b1)]
 	        ).

test1 :- go(
						[available(a1), available(a2), ontable(b1, 2, 2), ontable(b2, 1, 1), clear(b1), clear(b2)],
 	          [available(a1), available(a2), ontable(b2, 3, 3), on(b1, b2, 3, 3), clear(b1)]
 	        ).

% from b2,b1 stacked to b1, b2 on the table.
test2 :- go(
						[available(a1), available(a2), available(a3), ontable(b2, 1, 1), on(b1, b2, 1, 1), clear(b1)],
 	          [available(a1), available(a2), available(a3), ontable(b1, 2, 2), ontable(b2, 3, 3), clear(b1), clear(b2)]
 	        ).

% from b2,b1 stacked and b3 on the table to b1,b2,b3 stacked.
test3 :- go(
 	          [available(a1), available(a2), available(a3), ontable(b2, 1, 1), on(b1, b2, 1, 1), clear(b1), ontable(b3, 2, 2), clear(b3)],
						[available(a1), available(a2), available(a3), ontable(b1, 3, 3), on(b2, b1, 3, 3), on(b3, b2, 3, 3), clear(b3)]
 	        ).

% from b1,b2,b3 stacked to b1,b3 stacked and b2 on the table
test4 :-  go(
							[available(a1), available(a2), available(a3), ontable(b1, 1, 1), on(b2, b1, 1, 1), on(b3, b2, 1, 1), clear(b3)],
							[available(a1), available(a2), available(a3), ontable(b1, 1, 1), on(b3, b1, 1, 1), ontable(b2, 2, 2), clear(b2), clear(b3)]
						).

test5 :-  go(
							[available(a1), available(a2), available(a3), ontable(b1, 1, 1), ontable(b2, 2, 2), ontable(b3, 3, 3), ontable(b4, 4, 4), clear(b1), clear(b2), clear(b3), clear(b4)],
							[available(a1), available(a2), available(a3), ontable(b1, 1, 1), on(b2, b1, 1, 1), ontable(b3, 3, 3), on(b4, b3, 3, 3), clear(b2), clear(b4)]
						).

test6 :-  go(
							[available(a1), ontable(b1, 1, 1), on(b2, b1, 1, 1), clear(b2)],
							[available(a1), ontable(b1, 2, 2), on(b2, b1, 2, 2), clear(b2)]
						).

test :- test0. 
testNoTrace :- test. 
testTrace :- leash(-all), trace, testNoTrace. 

testSmallTrace :- 
	trace(action, all),
	trace(conditions_met, all), 
	trace(conditions_not_met, all), 
	trace(plan, all),
	trace(stack, all),
	trace(is_final_state, all),
	trace(testPlan, all),
	trace(stack, all),
	test.


/*
action(
	move_block_to_table_end(A, B, X, Y, X1, Y1),
	[gripped(A, B), moving(A, B, X, Y, X1, Y1)],
	[],
	[],
	[
		del(moving(A, B, X, Y, X1, Y1)), del(gripped(A, B)),
		add(available(A)), add(ontable(B, X1, Y1)), add(clear(B))
	],
	[ontable(B, X1, Y1), diffPos(X, X1, Y, Y1)]
).
action(
	move_block_to_on_end(A, B, X, Y, X1, Y1),
	[gripped(A, B), moving(A, B, X, Y, X1, Y1), clear(B1)],
	[],
	[],
	[
		del(moving(A, B, X, Y, X1, Y1)), del(clear(B1)), del(gripped(A, B)),
		add(available(A)), add(on(B, B1, X1, Y1)), add(clear(B))
	],
	[on(B, B1, X1, Y1), diffPos(X, X1, Y, Y1)]
).
action(
	move_block_to_buffer_end(A, B, X, Y, X1, Y1),
	[gripped(A, B), moving(A, B, X, Y, X1, Y1)],
	[buffer(_B, X1, Y1)],
	[],
	[
		del(moving(A, B, X, Y, X1, Y1)), del(gripped(A, B)),
		add(buffer(B, X1, Y1)), add(clear(B)), add(available(A))
	],
	[pos(X1, Y1), diffPos(X, X1, Y, Y1)]
).
*/
