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
plan(State, Goal, _, Moves) :- 	
				equal_set(State, Goal), 
				write('moves are'), nl,
				reverse_print_stack(Moves).
plan(State, Goal, Been_list, Moves) :- 	
				move(Name, PreconditionsT, PreconditionsF, Actions),
				conditions_met(PreconditionsT, State),
				conditions_not_met(PreconditionsF, State),
				change_state(State, Actions, Child_state),
				not(member_state(Child_state, Been_list)),
				stack(Child_state, Been_list, New_been_list),
				stack(Name, Moves, New_moves),
			plan(Child_state, Goal, New_been_list, New_moves).

change_state(S, [], S).
change_state(S, [add(P)|T], S_new) :-	change_state(S, T, S2),
					add_to_set(P, S2, S_new), !.
change_state(S, [del(P)|T], S_new) :-	change_state(S, T, S2),
					remove_from_set(P, S2, S_new), !.
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

move(
			grip(A, B), 
			[available(A), clear(B)],
			[inposition(B), gripped(_A, B)],
			[del(available(A)), add(gripped(A, B)), del(clear(B))]
		).
move(
			move_block(A, B),
			[gripped(A, B), on(B, B1)],
			[notavalidpredicate(A)],
			[del(gripped(A, B)), add(clear(B1)), del(clear(B)), del(on(B, B1)),add(moving(A, B))]
		).
move(
			move_block(A, B),
			[gripped(A, B), ontable(B)],
			[notavalidpredicate(A)],
			[del(gripped(A, B)), add(moving(A, B))]
		).
move(
			stack(A, B1, B2),
			[moving(A, B1), clear(B2), inposition(B2)],
			[notavalidpredicate(A)],
			[del(moving(A, B1)), add(available(A)), del(clear(B2)), add(on(B1, B2)), add(clear(B1)), 
				add(inposition(B1)), del(inposition(B2))]
		).
move(
			release(A, B),
			[moving(A, B)],
			[notavalidpredicate(A)],
			[del(moving(A, B)), add(available(A)), add(clear(B)), add(ontable(B)),
				add(inposition(B))]
		).

go(S, G) :- plan(S, G, [S], []).

% From a, b on the table to b,a stacked.
test :- go([available(a1), available(a2), ontable(a), ontable(b), clear(a), clear(b)],
 	          [available(a1), available(a2), ontable(b), on(a,b), clear(a), inposition(a)]).

% From b,a stacked to a, b on the table.
% test :- go(
%  	          [available(a1), available(a2), available(a3), ontable(b), on(a,b), clear(a)],
% 						[available(a1), available(a2), available(a3), ontable(a), ontable(b), clear(a), clear(b), inposition(a), inposition(b)]
%  	        ).

% From b,a stacked and c on the table to a,b,c stacked.
% test :- go(
%  	          [available(a1), available(a2), available(a3), ontable(b), on(a,b), clear(a), ontable(c), clear(c)],
% 						[available(a1), available(a2), available(a3), ontable(a), on(b,a), on(c,b), clear(c), inposition(c)]
%  	        ).

/* sample moves */

% move(pickup(X), [handempty, clear(X), on(X, Y)], 
% 		[del(handempty), del(clear(X)), del(on(X, Y)), 
% 				 add(clear(Y)),	add(holding(X))]).
% 
% move(pickup(X), [handempty, clear(X), ontable(X)], 
% 		[del(handempty), del(clear(X)), del(ontable(X)), 
% 				 add(holding(X))]).
% 
% move(putdown(X), [holding(X)], 
% 		[del(holding(X)), add(ontable(X)), add(clear(X)), 
% 				  add(handempty)]).
% 
% move(stack(X, Y), [holding(X), clear(Y)], 
% 		[del(holding(X)), del(clear(Y)), add(handempty), add(on(X, Y)),
% 				  add(clear(X))]).


testold :- go([handempty, ontable(b), ontable(c), on(a, b), clear(c), clear(a)],
 	          [handempty, ontable(c), on(a,b), on(b, c), clear(a)]).
