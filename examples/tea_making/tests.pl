testCase(Actions, Times) :- 
	go(
	  [available(robot), at(robot, kitchen), at(teabag, kitchen), at(kettle, kitchen), at(cup, kitchen), empty(kettle), empty(cup), tea(notready)],
	  [at(robot, kitchen), available(robot), at(cup, kitchen), at(kettle, kitchen), full(cup), tea(prepared)],
 	  Actions,
 	  Times
	).


testNoTrace :- testCase(_A, _T).
testTrace :- leash(-all),trace,testCase(_A,_T).
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
	testCase(_A, _T).


