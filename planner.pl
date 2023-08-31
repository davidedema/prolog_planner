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

plan(State, Goal, _, Moves, Times) :- 	
				equal_set(State, Goal), 
				write('moves are'), nl,
				%reverse_print_stack(Moves),
				reverse_print_moves_times(Moves, Times)
				%true
				.
plan(State, Goal, Been_list, Moves, Times) :- 	
				move(Name, PreconditionsT, PreconditionsF, FinalConditionsF, Actions),
				conditions_met(PreconditionsT, State),
				conditions_not_met(PreconditionsF, State),
				conditions_not_met(FinalConditionsF, Goal),
				change_state(State, Actions, Child_state),
				not(member_state(Child_state, Been_list)),
				stack(Child_state, Been_list, New_been_list),
				stack(Name, Moves, New_moves),
				partial_order(PreconditionsT, New_been_list, Time),
				stack(Time, Times, New_Times),
				plan(Child_state, Goal, New_been_list, New_moves, New_Times)
				.	

change_state(S, [], S).
change_state(S, [add(P)|T], S_new) :-	change_state(S, T, S2),
					add_to_set(P, S2, S_new), !.
change_state(S, [del(P)|T], S_new) :-	change_state(S, T, S2),
					remove_from_set(P, S2, S_new), !.
change_state(S, [diff(A,B)|T], S) :- A \== B, change_state(S, T, S).

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

reverse_print_moves_times(Moves, Times) :-
				length_stack(Moves, Len), length_stack(Times, Len),
				I is Len,
				reverse_print_moves_times(Moves, Times, I).

reverse_print_moves_times(Moves, Times, _I) :-
				empty_stack(Moves),
				empty_stack(Times).

reverse_print_moves_times(Moves, Times, I) :-
				stack(M, TMoves, Moves), 
				stack(T, TTimes, Times),
				NewI is I - 1,
				reverse_print_moves_times(TMoves, TTimes, NewI),
				format("[~w]\t~w ~w~n", [I, M, T]).

move(
			grip_ontable_start(A, B), 
			[ontable(B, X, Y), available(A), clear(B)],
			[gripped(_, B), gripping(_, B)],
			[ontable(B, X, Y)],
			[del(available(A)), add(gripping(A, B))]
		).
move(
			grip_on_start(A, B), 
			[on(B, B1, X, Y), available(A), clear(B)],
			[gripped(_, B), gripping(_, B)],
			[on(B, B1, X, Y)],
			[del(available(A)), add(gripping(A, B))]
		).
move(
			grip_end(A, B), 
			[gripping(A, B)],
			[notavalidpredicate(A)],
			[notavalidpredicate(A)],
			[del(clear(B)), del(gripping(A, B)), add(gripped(A, B))]
		).
 
	% move(
	% 			move_block_on_start(A, B, X, Y, X1, Y1),
	% 			[gripped(A, B), on(B, B1, X, Y)],
	% 			[notavalidpredicate(A)],
	% 			[notavalidpredicate(A)],
	% 			[del(on(B, B1, X, Y)), add(clear(B1)), add(moving(A, B, X, Y, X1, Y1))]
	% 		).
	% move(
	% 			move_block_ontable_start(A, B, X, Y, X1, Y1),
	% 			[gripped(A, B), ontable(B, X, Y)],
	% 			[notavalidpredicate(A)],
	% 			[notavalidpredicate(A)],
	% 			[del(ontable(B, X, Y)), add(moving(A, B, X, Y, X1, Y1))]
	% 		).
	% move(
	% 			move_block_end(A, B, X, Y, X1, Y1),
	% 			[moving(A, B, X, Y, X1, Y1)],
	% 			[notavalidpredicate(A)],
	% 			[notavalidpredicate(A)],
	% 			[del(moving(A, B, X, Y, X1, Y1)), add(moved(A, B, X1, Y1))]
	% 		).
move(
			move_block_on(A, B, X, Y, X1, Y1),
			[gripped(A, B), on(B, B1, X, Y)],
			[notavalidpredicate(A)],
			[notavalidpredicate(A)],
			[del(clear(B)), del(on(B, B1, X, Y)), add(clear(B1)), add(moving(A, B, X1, Y1))]
		).
move(
			move_block_ontable(A, B, X, Y, X1, Y1),
			[gripped(A, B), ontable(B, X, Y)],
			[notavalidpredicate(A)],
			[notavalidpredicate(A)],
			[del(ontable(B, X, Y)), add(moving(A, B, X1, Y1))]
		).
	
move(
			stack(A, B1, B2),
			[moving(A, B1, X1, Y1), clear(B2)],
			[notavalidpredicate(A)],
			[notavalidpredicate(A)],
			[del(gripped(A, B1)), del(moving(A, B1, X1, Y1)), del(clear(B2)), add(available(A)), add(on(B1, B2, X1, Y1)), add(clear(B1))]
		).
move(
			release(A, B),
			[moving(A, B, X1, Y1)],
			[notavalidpredicate(A)],
			[notavalidpredicate(A)],
			[del(gripped(A, B)), del(moving(A, B, X1, Y1)), add(available(A)), add(ontable(B, X1, Y1)), add(clear(B))]
		).

	% move(
	% 			move_block_on_start(A, B, X, Y, X1, Y1),
	% 			[gripped(A, B), on(B, B1, X, Y)],
	% 			[notavalidpredicate(A)],
	% 			[notavalidpredicate(A)],
	% 			[del(on(B, B1, X, Y)), add(clear(B1)), add(moving(A, B, X, Y, X1, Y1))]
	% 		).
	% move(
	% 			move_block_ontable_start(A, B, X, Y, X1, Y1),
	% 			[gripped(A, B), ontable(B, X, Y)],
	% 			[notavalidpredicate(A)],
	% 			[notavalidpredicate(A)],
	% 			[del(ontable(B, X, Y)), add(moving(A, B, X, Y, X1, Y1))]
	% 		).
	% move(
	% 			move_block_end(A, B, X, Y, X1, Y1),
	% 			[moving(A, B, X, Y, X1, Y1)],
	% 			[notavalidpredicate(A)],
	% 			[notavalidpredicate(A)],
	% 			[del(moving(A, B, X, Y, X1, Y1)), add(moved(A, B, X1, Y1))]
	% 		).
	% 
	% move(
	% 			release_start(A, B),
	% 			[moved(A, B, X1, Y1)],
	% 			[notavalidpredicate(A)],
	% 			[notavalidpredicate(A)],
	% 			[del(moved(A, B, X1, Y1)), add(releasing(A, B, X1, Y1))]
	% 		).
	% move(
	% 			release(A, B),
	% 			[releasing(A, B, X1, Y1)],
	% 			[notavalidpredicate(A)],
	% 			[notavalidpredicate(A)],
	% 			[del(releasing(A, B, X1, Y1)), del(gripped(A, B)), add(available(A)), add(ontable(B, X1, Y1)), add(clear(B))]
	% 		).
	% 
	% move(
	% 			stack_start(A, B1, B2),
	% 			[moved(A, B1, X1, Y1), clear(B2)],
	% 			[notavalidpredicate(A)],
	% 			[notavalidpredicate(A)],
	% 			[del(clear(B2)), add(stacking(A, B1, B2, X1, Y1))]
	% 		).
	% move(
	% 			stack_end(A, B1, B2),
	% 			[stacking(A, B1, B2, X1, Y1)],
	% 			[notavalidpredicate(A)],
	% 			[notavalidpredicate(A)],
	% 			[del(gripped(A, B1)), del(stacking(A, B1, B2, X1, Y1)), add(available(A)), add(on(B1, B2, X1, Y1)), add(clear(B1))]
	% 		).

go(S, G) :- plan(S, G, [S], [], []).

% From a, b on the table to b,a stacked.
test1 :- go(
						[available(a1), available(a2), ontable(a, 2, 2), ontable(b, 1, 1), clear(a), clear(b)],
 	          [available(a1), available(a2), ontable(b, 3, 3), on(a, b, 3, 3), clear(a)]
 	        ).

% From b,a stacked to a, b on the table.
test2 :- go(
						[available(a1), available(a2), available(a3), ontable(b, 1, 1), on(a, b, 1, 1), clear(a)],
 	          [available(a1), available(a2), available(a3), ontable(a, 2, 2), ontable(b, 3, 3), clear(a), clear(b)]
 	        ).

% From b,a stacked and c on the table to a,b,c stacked.
test3 :- go(
 	          [available(a1), available(a2), available(a3), ontable(b, 1, 1), on(a, b, 1, 1), clear(a), ontable(c, 2, 2), clear(c)],
						[available(a1), available(a2), available(a3), ontable(a, 3, 3), on(b, a, 3, 3), on(c, b, 3, 3), clear(c)]
 	        ).

% From a,b,c stacked to a,c stacked and b on the table in random position
test4 :-  go(
							[availble(a1), available(a2), availble(a3), ontable(a, 1, 1), on(b, a, 1, 1), on(c,b, 1, 1), clear(c)],
							[availble(a1), available(a2), availble(a3), ontable(a, 1, 1), on(c, a, 1, 1), ontable(b, 2, 2), clear(b), clear(c)]
						).

test :- test2.

traverse_list([], Prev) :- write(Prev).
traverse_list([X|Rest], Prev) :-
	traverse_list(Rest, X),
	write(Prev).

traverse_list([X|Rest]) :- traverse_list(Rest, X).

reverse_list([], []).
reverse_list([X|T], Reversed) :-
	reverse_list(T, ReversedRest),
	append(ReversedRest, [X], Reversed).

/* sample moves
move(pickup(X), [handempty, clear(X), on(X, Y)], 
		[del(handempty), del(clear(X)), del(on(X, Y)), 
				 add(clear(Y)),	add(holding(X))]).

move(pickup(X), [handempty, clear(X), ontable(X)], 
		[del(handempty), del(clear(X)), del(ontable(X)), 
				 add(holding(X))]).

move(putdown(X), [holding(X)], 
		[del(holding(X)), add(ontable(X)), add(clear(X)), 
				  add(handempty)]).

move(stack(X, Y), [holding(X), clear(Y)], 
		[del(holding(X)), del(clear(Y)), add(handempty), add(on(X, Y)),
				  add(clear(X))]).


testold :- go([handempty, ontable(b), ontable(c), on(a, b), clear(c), clear(a)],
 	          [handempty, ontable(c), on(a,b), on(b, c), clear(a)]).
*/
