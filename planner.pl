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

plan(State, Goal, _, Actions, Times) :- 	
				equal_set(State, Goal), 
				write('actions are'), nl,
				%reverse_print_stack(Actions),
				reverse_print_actions_times(Actions, Times)
				%true
				.

plan(State, Goal, Been_list, Actions, Times) :- 	
				action(Name, PreconditionsT, PreconditionsF, FinalConditionsF, Effects),
				conditions_met(PreconditionsT, State),
				conditions_not_met(PreconditionsF, State),
				conditions_not_met(FinalConditionsF, Goal),
				change_state(State, Effects, Child_state),
				not(member_state(Child_state, Been_list)),
				stack(Child_state, Been_list, New_been_list),
				stack(Name, Actions, New_actions),
				partial_order(PreconditionsT, New_been_list, Time),
				stack(Time, Times, New_Times),
				plan(Child_state, Goal, New_been_list, New_actions, New_Times)
				.	

change_state(S, [], S).
change_state(S, [add(P)|T], S_new) :-	change_state(S, T, S2),
					add_to_set(P, S2, S_new), !.
change_state(S, [del(P)|T], S_new) :-	change_state(S, T, S2),
					remove_from_set(P, S2, S_new), !.
				% change_state(S, [diff(A,B)|T], S) :- A \== B, change_state(S, T, S).


conditions_met(P, S) :- subset(P, S).

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

action(
			grip_ontable_start(A, B), 
			[ontable(B, X, Y), available(A), clear(B)],
			[gripped(_, B), gripping(_, B)],
			[ontable(B, X, Y)],
			[del(available(A)), add(gripping(A, B))]
		).
action(
			grip_on_start(A, B), 
			[on(B, B1, X, Y), available(A), clear(B)],
			[gripped(_, B), gripping(_, B)],
			[on(B, B1, X, Y)],
			[del(available(A)), add(gripping(A, B))]
		).
action(
			grip_end(A, B), 
			[gripping(A, B)],
			[notavalidpredicate(A)],
			[notavalidpredicate(A)],
			[del(clear(B)), del(gripping(A, B)), add(gripped(A, B))]
		).
 
action(
			move_block_on_start(A, B, X, Y, X1, Y1),
			[gripped(A, B), on(B, B1, X, Y)],
			[notavalidpredicate(A)],
			[notavalidpredicate(A)],
			[del(on(B, B1, X, Y)), add(clear(B1)), add(translating(A, B, X, Y, X1, Y1))]
		).
action(
			move_block_ontable_start(A, B, X, Y, X1, Y1),
			[gripped(A, B), ontable(B, X, Y)],
			[notavalidpredicate(A)],
			[notavalidpredicate(A)],
			[del(ontable(B, X, Y)), add(translating(A, B, X, Y, X1, Y1))]
		).
action(
			move_block_end(A, B, X, Y, X1, Y1),
			[translating(A, B, X, Y, X1, Y1)],
			[notavalidpredicate(A)],
			[notavalidpredicate(A)],
			[del(translating(A, B, X, Y, X1, Y1)), add(moving(A, B, X1, Y1))]
		).

move(
			release(A, B),
			[moving(A, B, X1, Y1)],
			[notavalidpredicate(A)],
			[notavalidpredicate(A)],
			[del(gripped(A, B)), del(moving(A, B, X1, Y1)), add(available(A)), add(ontable(B, X1, Y1)), add(clear(B))]
		).

	% move(
	% 			release_start(A, B),
	% 			[moving(A, B, X1, Y1)],
	% 			[notavalidpredicate(A)],
	% 			[notavalidpredicate(A)],
	% 			[del(moving(A, B, X1, Y1)), add(releasing(A, B, X1, Y1))]
	% 		).
	% move(
	% 			release_end(A, B),
	% 			[releasing(A, B, X1, Y1)],
	% 			[notavalidpredicate(A)],
	% 			[notavalidpredicate(A)],
	% 			[del(releasing(A, B, X1, Y1)), del(gripped(A, B)), add(available(A)), add(ontable(B, X1, Y1)), add(clear(B))]
	% 		).

action(
			stack_start(A, B1, B2),
			[moving(A, B1, X1, Y1), clear(B2)],
			[gripping(_A, B2)],
			[notavalidpredicate(A)],
			[del(clear(B2)), del(moving(A, B1, X1, Y1)), add(stacking(A, B1, B2, X1, Y1))]
		).
action(
			stack_end(A, B1, B2),
			[stacking(A, B1, B2, X1, Y1)],
			[notavalidpredicate(A)],
			[notavalidpredicate(A)],
			[del(gripped(A, B1)), del(stacking(A, B1, B2, X1, Y1)), add(available(A)), add(on(B1, B2, X1, Y1)), add(clear(B1))]
		).
	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%action(
% 			grip_ontable(A, B), 
% 			[ontable(B, X, Y), available(A), clear(B)],
% 			[gripped(_A, B)],
% 			[ontable(B, X, Y)],
% 			[del(available(A)), del(clear(B)), add(gripped(A, B))]
% 		).
%action(
% 			grip_on(A, B), 
% 			[on(B, B1, X, Y), available(A), clear(B)],
% 			[gripped(_A, B)],
% 			[on(B, B1, X, Y)],
% 			[del(available(A)), del(clear(B)), add(gripped(A, B))]
% 		).
%action(
% 			move_block_on(A, B, X, Y, X1, Y1),
% 			[gripped(A, B), on(B, B1, X, Y)],
% 			[notavalidpredicate(A)],
% 			[notavalidpredicate(A)],
% 			[del(clear(B)), del(on(B, B1, X, Y)), add(clear(B1)), add(moving(A, B, X1, Y1))]
% 		).
%action(
% 			move_block_ontable(A, B, X, Y, X1, Y1),
% 			[gripped(A, B), ontable(B, X, Y)],
% 			[notavalidpredicate(A)],
% 			[notavalidpredicate(A)],
% 			[del(ontable(B, X, Y)), add(moving(A, B, X1, Y1))]
% 		).
%action(
% 			stack(A, B1, B2),
% 			[moving(A, B1, X1, Y1), clear(B2)],
% 			[gripping(_A, B2)],
% 			[notavalidpredicate(A)],
% 			[del(gripped(A, B1)), del(moving(A, B1, X1, Y1)), del(clear(B2)), add(available(A)), add(on(B1, B2, X1, Y1)), add(clear(B1))]
% 		).
%action(
% 			release(A, B),
% 			[moving(A, B, X1, Y1)],
% 			[notavalidpredicate(A)],
% 			[notavalidpredicate(A)],
% 			[del(gripped(A, B)), del(moving(A, B, X1, Y1)), add(available(A)), add(ontable(B, X1, Y1)), add(clear(B))]
% 		).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

go(S, G) :- plan(S, G, [S], [], []).

% From b1, b2 on the table to b2,b1 stacked.
test1 :- go(
						[available(a1), available(a2), ontable(b1, 2, 2), ontable(b2, 1, 1), clear(b1), clear(b2)],
 	          [available(a1), available(a2), ontable(b2, 3, 3), on(b1, b2, 3, 3), clear(b1)]
 	        ).

% From b2,b1 stacked to b1, b2 on the table.
test2 :- go(
						[available(a1), available(a2), available(a3), ontable(b2, 1, 1), on(b1, b2, 1, 1), clear(b1)],
 	          [available(a1), available(a2), available(a3), ontable(b1, 2, 2), ontable(b2, 3, 3), clear(b1), clear(b2)]
 	        ).

% From b2,b1 stacked and b3 on the table to b1,b2,b3 stacked.
test3 :- go(
 	          [available(a1), available(a2), available(a3), ontable(b2, 1, 1), on(b1, b2, 1, 1), clear(b1), ontable(b3, 2, 2), clear(b3)],
						[available(a1), available(a2), available(a3), ontable(b1, 3, 3), on(b2, b1, 3, 3), on(b3, b2, 3, 3), clear(b3)]
 	        ).

% From b1,b2,b3 stacked to b1,b3 stacked and b2 on the table
test4 :-  go(
							[availble(a1), available(a2), availble(a3), ontable(b1, 1, 1), on(b2, b1, 1, 1), on(b3, b2, 1, 1), clear(b3)],
							[availble(a1), available(a2), availble(a3), ontable(b1, 1, 1), on(b3, b1, 1, 1), ontable(b2, 2, 2), clear(b2), clear(b3)]
						).

test :- test1.

