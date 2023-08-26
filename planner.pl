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
				move(Name, Preconditions, Actions),
				conditions_met(Preconditions, State),
				change_state(State, Actions, Child_state),
				not(member_state(Child_state, Been_list)),
				stack(Child_state, Been_list, New_been_list),
				stack(Name, Moves, New_moves),
			plan(Child_state, Goal, New_been_list, New_moves),!.

change_state(S, [], S).
change_state(S, [add(P)|T], S_new) :-	change_state(S, T, S2),
					add_to_set(P, S2, S_new), !.
change_state(S, [del(P)|T], S_new) :-	change_state(S, T, S2),
					remove_from_set(P, S2, S_new), !.
conditions_met(P, S) :- subset(P, S).


member_state(S, [H|_]) :- 	equal_set(S, H).
member_state(S, [_|T]) :- 	member_state(S, T).

reverse_print_stack(S) :- 	empty_stack(S).
reverse_print_stack(S) :- 	stack(E, Rest, S), 
				reverse_print_stack(Rest),
		 		write(E), nl.


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

move(
			grip(A, B), 
			[available(A), clear(B), on(B, B1)],
			[del(available(A)), del(clear(B)), del(on(B, B1)), add(clear(B1)), add(gripped(A, B))]
		).
move(
			grip(A, B), 
			[available(A), clear(B), ontable(B)],
			[del(available(A)), del(clear(B)), del(ontable(B)), add(gripped(A, B))]
		).
move(
			move_block(A, B),
			[gripped(A, B)],
			[del(gripped(A, B)), add(moving(A, B))]
		).
move(
			release(A, B),
			[moving(A, B)],
			[del(moving(A, B)), add(available(A)), add(clear(B)), add(ontable(B))]
		).
move(
			stack(A, B1, B2),
			[moving(A, B1), clear(B2)],
			[del(moving(A, B1)), add(available(A)), del(clear(B2)), add(on(B1, B2)), add(clear(B1))]
		).

go(S, G) :- plan(S, G, [S], []).
test :- go([available(a1), available(a2), ontable(a), ontable(b), on(c,a), clear(c), clear(b)],
 	          [available(a1), available(a2), ontable(b), on(a,b), clear(a), ontable(c), clear(c)]).
  
testold :- go([handempty, ontable(b), ontable(c), on(a, b), clear(c), clear(a)],
 	          [handempty, ontable(c), on(a,b), on(b, c), clear(a)]).
