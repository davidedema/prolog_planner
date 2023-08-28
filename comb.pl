combine([], []).
combine([X | Rest], [X | Combination]) :-
    combine(Rest, Combination).
combine([_ | Rest], Combination) :-
    combine(Rest, Combination).
