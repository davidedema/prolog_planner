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
				move(Name, PreconditionsT, PreconditionsF, FinalConditionsF, Actions),
				conditions_met(PreconditionsT, State),
				%conditions_met(FinalConditionsT),
				conditions_not_met(PreconditionsF, State),
				conditions_not_met(FinalConditionsF, Goal),
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
			grip_ontable(A, B), 
			[ontable(B, X, Y), available(A), clear(B)],
			[gripped(_A, B)],
			[ontable(B, X, Y)],
			[del(available(A)), add(gripped(A, B)), del(clear(B))]
		).
move(
			grip_on(A, B), 
			[on(B, B1, X, Y), available(A), clear(B)],
			[gripped(_A, B)],
			[on(B, B1, X, Y)],
			[del(available(A)), add(gripped(A, B)), del(clear(B))]
		).
move(
			move_aside_ontable(A, B, X, Y, X1, Y1),
			[gripped(A, B), ontable(B, X, Y)],
			[ontable(_B, X1, Y1)],
			[notavalidpredicate(A)],
			[
				del(gripped(A, B)), del(ontable(B, X, Y)),  
				add(ontable(B, X1, Y1)), add(available(A)), add(clear(B))
			]
		).
move(
			move_aside_on(A, B, X, Y, X1, Y1),
			[gripped(A, B), on(B, B1, X, Y)],
			[ontable(B, X1, Y1)],
			[notavalidpredicate(A)],
			[ 
				del(gripped(A, B)), del(on(B, B1, X, Y)),
				add(clear(B1)), add(ontable(B, X1, Y1)), add(available(A))
			]
		).
move(
			move_block_on(A, B, X, Y, X1, Y1),
			[gripped(A, B), on(B, B1, X, Y)],
			[notavalidpredicate(A), ontable(_B, X1, Y1)],
			[notavalidpredicate(A)],
			[del(gripped(A, B)), del(clear(B)), del(on(B, B1, X, Y)), add(clear(B1)), add(moving(A, B, X1, Y1))]
		).
move(
			move_block_ontable(A, B, X, Y, X1, Y1),
			[gripped(A, B), ontable(B, X, Y)],
			[notavalidpredicate(A)],
			[notavalidpredicate(A)],
			[del(gripped(A, B)), del(ontable(B, X, Y)), add(moving(A, B, X1, Y1))]
		).
move(
			stack(A, B1, B2),
			[moving(A, B1, X1, Y1), clear(B2)],
			[notavalidpredicate(A)],
			[notavalidpredicate(A)],
			[del(moving(A, B1, X1, Y1)), del(clear(B2)), add(available(A)), add(on(B1, B2, X1, Y1)), add(clear(B1))]
		).
move(
			release(A, B),
			[moving(A, B, X1, Y1)],
			[notavalidpredicate(A), ontable(_B, X1, Y1)],
			[notavalidpredicate(A)],
			[del(moving(A, B, X1, Y1)), add(available(A)), add(ontable(B, X1, Y1)), add(clear(B))]
		).

go(S, G) :- plan(S, G, [S], []).

% From a, b on the table to b,a stacked.
% test :- go( [available(a1), available(a2), ontable(a, 2, 2), ontable(b, 1, 1), clear(a), clear(b)],
%  	          [available(a1), available(a2), ontable(b, 3, 3), on(a, b, 3, 3), clear(a)]
%  	        ).

% From b,a stacked to a, b on the table.
test :- go(
						[available(a1), available(a2), available(a3), ontable(b, 1, 1), on(a, b, 1, 1), clear(a)],
 	          [available(a1), available(a2), available(a3), ontable(a, 2, 2), ontable(b, 3, 3), clear(a), clear(b)]
 	        ).

% From b,a stacked and c on the table to a,b,c stacked.
% test :- go(
%  	          [available(a1), available(a2), available(a3), ontable(b), on(a,b), clear(a), ontable(c), clear(c)],
% 						[available(a1), available(a2), available(a3), ontable(a), on(b,a), on(c,b), clear(c), inposition(c)]
%  	        ).

% From a,b,c stacked to a,c stacked and c on the table in random position
% test :-  go(
% 							[availble(a1), available(a2), availble(a3), ontable(a, 1, 1), on(b,a), on(c,b), clear(c)],
% 							[availble(a1), available(a2), availble(a3), on(b,a), ontable(a, 1, 1), clear(b), clear(c), ontable(c, X, Y)]
% 						).

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
