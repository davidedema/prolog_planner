testCase(Actions, Times) :-
    go(
        [agent(a1), agent(a2), busy(agent(a1), cell(1,1)), busy(agent(a2), cell(2,2))],
        [agent(a1), agent(a2), busy(agent(a1), cell(2,3)), busy(agent(a2), cell(1,3))],
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
