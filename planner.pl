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

:- include('adts.pl').
:- include('actions.pl').
:- include('kb.pl').

:- dynamic ontable/3.
:- dynamic available/1.
:- dynamic on/4.
:- dynamic clear/1.
:- discontiguous action/6.

ground_g([]).
ground_g([_H|T]) :-
	% assertz(H),
	ground_g(T).

achiever([HP|_TP], [add(HP)|_TE], _).
achiever([_HP|TP], [], E) :-
	achiever(TP, E, []).
achiever([HP|TP], [HE|TE], U) :-
	append([HE], U, NewU),
	achiever([HP|TP], TE, NewU).

partial_order(_PT, [], [], 0).

partial_order(PT, [AH|AT], [I|Times], I) :-
  action(AH, _, _, _, _, E),
  achiever(PT, E, []),
  NewI is I-1,
  partial_order(PT, AT, Times, NewI).

partial_order(PT, [AH|AT], Times, I) :-
  action(AH, _, _, _, _, E),
  \+achiever(PT, E, []),
  NewI is I-1,
  partial_order(PT, AT, Times, NewI).

partial_order(PT, Actions, Times) :-
  length(Actions, NActions),
  N is NActions,
  partial_order(PT, Actions, Times, N).

verify([]).
verify([H|T]) :-
	H,
	verify(T),
	H.

plan(State, Goal, _, Actions, Times, _MaxDepth, Actions, Times) :- 	
	equal_set(State, Goal),
	write('actions are'), nl,
	%reverse_print_stack(Actions),
	reverse_print_actions_times(Actions, Times)
	%true
	.

plan(State, Goal, Been_list, Actions, Times, MaxDepth, RetActions, RetTimes) :- 	
	length(Actions, Len), Len < MaxDepth,
	action(Name, PreconditionsT, PreconditionsF, FinalConditionsF, Verify, Effects),
	verify(Verify),
	conditions_met(PreconditionsT, State),
	conditions_not_met(PreconditionsF, State),
	conditions_not_met(FinalConditionsF, Goal),
	change_state(State, Effects, Child_state),
	testPlan(Child_state, Goal, Been_list, Name, Actions, PreconditionsT, Times, MaxDepth, RetActions, RetTimes)
	.	

testPlan(Child_state, Goal, Been_list, Name, Actions, PreconditionsT, Times, MaxDepth, RetActions, RetTimes) :-
	\+equal_set(Child_state, Goal),
	not(member_state(Child_state, Been_list)),
	stack(Child_state, Been_list, New_been_list),
	stack(Name, Actions, New_actions),
	partial_order(PreconditionsT, Actions, Time),
	stack(Time, Times, New_Times),
	plan(Child_state, Goal, New_been_list, New_actions, New_Times, MaxDepth, RetActions, RetTimes).

testPlan(Child_state, Goal, Been_list, Name, Actions, PreconditionsT, Times, MaxDepth, RetActions, RetTimes) :-
	equal_set(Child_state, Goal),
	stack(Child_state, Been_list, New_been_list),
	stack(Name, Actions, New_actions),
	partial_order(PreconditionsT, Actions, Time),
	stack(Time, Times, New_Times),
	plan(Child_state, Goal, New_been_list, New_actions, New_Times, MaxDepth, RetActions, RetTimes).

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

diffPos(X, X1, _Y, _Y1) :-
	X \== X1.
diffPos(X, X1, Y, Y1) :-
	X == X1, Y \== Y1.
diffPos(X, X1, Y, Y1) :-
	X == X1, Y == Y1, fail.

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

test_plan_result(R, _S, _G, _StateList, _Actions, _Times, _MaxTime, _RetActins, _RetTimes) :-
	R.
test_plan_result(R, S, G, _StateList, _Actions, _Times, MaxTime, RetActions, RetTimes) :-
	\+ R,
	try_plan(S, G, [S], [], [], MaxTime, RetActions, RetTimes).

try_plan(S, G, StateList, Actions, Times, MaxTime, RetActions, RetTimes) :-
	NewMaxTime is MaxTime + 10, 
	write("Testing for "), write(NewMaxTime), nl,
	NewMaxTime < 50,
	(plan(S, G, StateList, Actions, Times, NewMaxTime, RetActions, RetTimes) -> Res = true; Res = false),
	test_plan_result(Res, S, G, StateList, Actions, Times, NewMaxTime, RetActions, RetTimes).

go(S, G, Actions, Times) :- 
	retractall(ontable(_, _, _)),
	retractall(on(_, _, _, _)),
	retractall(clear(_)),
	retractall(available(_)),
	ground_g(G), 
	plan(S, G, [S], [], [], 10, Actions, Times).

go(S, G, Actions, Times, MaxDepth) :- 
	retractall(ontable(_, _, _)),
	retractall(on(_, _, _, _)),
	retractall(clear(_)),
	retractall(available(_)),
	ground_g(G), 
	plan(S, G, [S], [], [], MaxDepth, Actions, Times).
 	%try_plan(S, G, [S], [], [], 0, Actions, Times).


