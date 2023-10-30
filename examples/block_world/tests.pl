% from b1, b2 on the table to b2,b1 stacked.
test01(Actions, Times) :- 
	go(
		[available(a1), available(a2), ontable(b1, 2, 2), clear(b1)],
 	  [available(a1), available(a2), ontable(b1, 3, 3), clear(b1)],
 	  Actions,
 	  Times
 	).

test0(Actions, Times) :- 
	go(
		[available(a1), ontable(b1, 2, 2), clear(b1)],
 	  [available(a1), ontable(b1, 3, 3), clear(b1)],
 	  Actions,
 	  Times
 	).

test1(Actions, Times) :- 
	go(
		[available(a1), available(a2), ontable(b1, 2, 2), ontable(b2, 1, 1), clear(b1), clear(b2)],
 	  [available(a1), available(a2), ontable(b2, 3, 3), on(b1, b2, 3, 3), clear(b1)],
 	  Actions,
 	  Times
 	).

% from b2,b1 stacked to b1, b2 on the table.
test2(Actions, Times) :- 
	go(
		[available(a1), available(a2), available(a3), ontable(b2, 1, 1), on(b1, b2, 1, 1), clear(b1)],
 	  [available(a1), available(a2), available(a3), ontable(b1, 2, 2), ontable(b2, 3, 3), clear(b1), clear(b2)],
 	  Actions,
 	  Times
 	).

% from b2,b1 stacked and b3 on the table to b1,b2,b3 stacked.
test3(Actions, Times) :- 
	go(
 	  [available(a1), available(a2), available(a3), ontable(b2, 1, 1), on(b1, b2, 1, 1), clear(b1), ontable(b3, 2, 2), clear(b3)],
		[available(a1), available(a2), available(a3), ontable(b1, 3, 3), on(b2, b1, 3, 3), on(b3, b2, 3, 3), clear(b3)],
 	  Actions,
 	  Times
 	).

% from b1,b2,b3 stacked to b1,b3 stacked and b2 on the table
test4(Actions, Times) :- 
	go(
		[available(a1), available(a2), available(a3), ontable(b1, 1, 1), on(b2, b1, 1, 1), on(b3, b2, 1, 1), clear(b3)],
		[available(a1), available(a2), available(a3), ontable(b1, 1, 1), on(b3, b1, 1, 1), ontable(b2, 2, 2), clear(b2), clear(b3)],
 	  Actions,
 	  Times
	).

test5(Actions, Times) :- 
	go(
		[available(a1), available(a2), available(a3), ontable(b1, 1, 1), ontable(b2, 2, 2), ontable(b3, 3, 3), ontable(b4, 4, 4), clear(b1), clear(b2), clear(b3), clear(b4)],
		[available(a1), available(a2), available(a3), ontable(b1, 1, 1), on(b2, b1, 1, 1), ontable(b3, 3, 3), on(b4, b3, 3, 3), clear(b2), clear(b4)],
 	  Actions,
 	  Times
	).

test6(Actions, Times) :- 
	go(
		[available(a1), ontable(b1, 1, 1), on(b2, b1, 1, 1), clear(b2)],
		[available(a1), ontable(b1, 2, 2), on(b2, b1, 2, 2), clear(b2)],
 	  Actions,
 	  Times
	).

test7(Actions, Times) :- 
	go(
		[available(a1), available(a2), ontable(b3, 1, 0), ontable(b1, 1, 1), on(b2, b1, 1, 1), clear(b2), clear(b3)],
		[available(a1), available(a2), ontable(b2, 1, 3), ontable(b1, 1, 2), on(b3, b1, 1, 2), clear(b2), clear(b3)],
 	  Actions,
 	  Times
	).

test(Action, Times) :- test6(Action, Times). 
testNoTrace :- test(_A, _T). 
testTrace :- leash(-all), trace, testNoTrace. 

testSmallTrace :- 
	trace(action, all),
	trace(conditions_met, all), 
	trace(conditions_not_met, all), 
	trace(partial_order, all),
	trace(achiever, all),
	trace(plan, all),
	trace(stack, all),
	trace(testPlan, all),
	trace(stack, all),
	test(_A, _T).

testNew :-
	trace(action, all), 
	trace(partial_order, all), 
	trace(conditions_met, all), 
	test7(_A, _T).

