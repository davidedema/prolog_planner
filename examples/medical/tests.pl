test1(Actions, Times) :- 
    go(
        [available(a1), available(a2), position_set(standing), toScan(bP1), toScan(bP2)],
        [
            available(a1), available(a2), 
            position_set(supine), 
            high_level_scanned, 
            mdl(m2), 
            scanned(bP1), scanned(bP2)
        ],
        Actions,
        Times
    ).

testSetPos(Actions, Times) :- 
    go(
        [available(a1), available(a2), position_set(standing), toScan(bP1), toScan(bP2)],
        [available(a1), available(a2), position_set(supine), toScan(bP1), toScan(bP2)],
        Actions,
        Times,
        5
    ).

testHighLevelScan(Actions, Times) :- 
    go(
        [available(a1), available(a2), position_set(standing), toScan(bP1), toScan(bP2)],
        [available(a1), available(a2), high_level_scanned, position_set(supine), toScan(bP1), toScan(bP2)],
        Actions,
        Times,
        5
    ).

testModelSelection(Actions, Times) :- 
    go(
        [available(a1), available(a2), position_set(standing), toScan(bP1), toScan(bP2)],
        [available(a1), available(a2), model_set(m3), high_level_scanned, position_set(supine), toScan(bP1), toScan(bP2)],
        Actions,
        Times,
        5
    ).

testFindBodySegmentsBP1(Actions, Times) :- 
    go(
        [available(a1), available(a2), position_set(standing), toScan(bP1), toScan(bP2)],
        [available(a1), available(a2), model_set(m1), high_level_scanned, position_set(supine), identified_area(bP1), toScan(bP2)],
        Actions,
        Times,
        10
    ).

testFindBodySegments(Actions, Times) :- 
    go(
        [available(a1), available(a2), position_set(standing), toScan(bP1), toScan(bP2)],
        [available(a1), available(a2), model_set(m1), high_level_scanned, position_set(supine), identified_area(bP1), identified_area(bP2)],
        Actions,
        Times,
        10
    ).

testDetailedVideoScannedBP2(Actions, Times) :- 
    go(
        [available(a1), available(a2), position_set(standing), toScan(bP1), toScan(bP2)],
        [available(a1), available(a2), model_set(m1), high_level_scanned, position_set(supine), identified_area(bP1), identified_area(bP2), detailed_video_scanned(a1, bP2)],
        Actions,
        Times,
        10
    ).

testDetailedVideoScanned(Actions, Times) :- 
    go(
        [available(a1), available(a2), position_set(standing), toScan(bP1), toScan(bP2)],
        [available(a1), available(a2), model_set(m1), high_level_scanned, position_set(supine), identified_area(bP1), identified_area(bP2), detailed_video_scanned(a1, bP1), detailed_video_scanned(a1, bP2)],
        Actions,
        Times,
        10
    ).

testPalpationBP1(Actions, Times) :- 
    go(
        [available(a1), available(a2), position_set(standing), toScan(bP1), toScan(bP2)],
        [available(a1), available(a2), model_set(m1), high_level_scanned, position_set(supine), identified_area(bP1), identified_area(bP2), detailed_video_scanned(a1, bP1), detailed_video_scanned(a1, bP2), palpated(a1, bP1)],
        Actions,
        Times,
        15
    ).

testPalpationBP1Cross(Actions, Times) :- 
    go(
        [available(a1), available(a2), position_set(standing), toScan(bP1), toScan(bP2)],
        [available(a1), available(a2), model_set(m1), high_level_scanned, position_set(supine), identified_area(bP1), identified_area(bP2), detailed_video_scanned(a1, bP1), detailed_video_scanned(a1, bP2), palpated(a2, bP1)],
        Actions,
        Times,
        15
    ).


testPalpationBP2(Actions, Times) :- 
    go(
        [available(a1), available(a2), position_set(standing), toScan(bP1), toScan(bP2)],
        [available(a1), available(a2), model_set(m1), high_level_scanned, position_set(supine), identified_area(bP1), identified_area(bP2), detailed_video_scanned(a1, bP1), detailed_video_scanned(a1, bP2), palpated(a1, bP2)],
        Actions,
        Times,
        15
    ).

testPalpationBP2Cross(Actions, Times) :- 
    go(
        [available(a1), available(a2), position_set(standing), toScan(bP1), toScan(bP2)],
        [available(a1), available(a2), model_set(m1), high_level_scanned, position_set(supine), identified_area(bP1), identified_area(bP2), detailed_video_scanned(a1, bP1), detailed_video_scanned(a1, bP2), palpated(a2, bP2)],
        Actions,
        Times,
        15
    ).

testPalpation(Actions, Times) :- 
    go(
        [available(a1), available(a2), position_set(standing), toScan(bP1), toScan(bP2)],
        [available(a1), available(a2), model_set(m1), high_level_scanned, position_set(supine), identified_area(bP1), identified_area(bP2), detailed_video_scanned(a1, bP1), detailed_video_scanned(a1, bP2), palpated(a1, bP2), palpated(a1, bP1)],
        Actions,
        Times,
        15
    ).

testPalpationCross(Actions, Times) :- 
    go(
        [available(a1), available(a2), position_set(standing), toScan(bP1), toScan(bP2)],
        [available(a1), available(a2), model_set(m1), high_level_scanned, position_set(supine), identified_area(bP1), identified_area(bP2), detailed_video_scanned(a1, bP1), detailed_video_scanned(a1, bP2), palpated(a2, bP1), palpated(a2, bP2)],
        Actions,
        Times,
        15
    ).

testPalpationSim(Actions, Times) :- 
    go(
        [available(a1), available(a2), position_set(standing), toScan(bP1), toScan(bP2)],
        [available(a1), available(a2), model_set(m1), high_level_scanned, position_set(supine), identified_area(bP1), identified_area(bP2), detailed_video_scanned(a1, bP1), detailed_video_scanned(a1, bP2), palpated(a2, bP1), palpated(a1, bP2)],
        Actions,
        Times,
        15
    ).

testPalpationNoVideo(Actions, Times) :- 
    go(
        [available(a1), available(a2), position_set(standing), toScan(bP1), toScan(bP2)],
        [available(a1), available(a2), model_set(m1), high_level_scanned, position_set(supine), identified_area(bP1), identified_area(bP2), palpated(a1, bP2), palpated(a1, bP1)],
        Actions,
        Times,
        10
    ).

testPalpationSimNoVideo(Actions, Times) :- 
    go(
        [available(a1), available(a2), position_set(standing), toScan(bP1), toScan(bP2)],
        [available(a1), available(a2), model_set(m1), high_level_scanned, position_set(supine), identified_area(bP1), identified_area(bP2), palpated(a2, bP1), palpated(a1, bP2)],
        Actions,
        Times,
        10
    ).

testMarkedBP1(Actions, Times) :-
    go(
        [available(a1), available(a2), position_set(standing), toScan(bP1), toScan(bP2)],
        [available(a1), available(a2), model_set(m1), high_level_scanned, position_set(supine), marked(bP1), toScan(bP1)],
        Actions,
        Times,
        10
    ).



testCase(A, T) :- test2(A, T).

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
