% see(Block, X, Y)
see(a, 2, 5).
see(d, 5, 5).
see(e, 5, 2).

% on(Block, BlockOrTable)
on(a, b).
on(b, c).
on(c, table).
on(d, table).
on(e, table).

% trovare z blocco in ricorsiva

z(Block, 0) :-
    on(Block, table).

z(Block, Z) :-
    on(Block, Block1),
    z(Block1, Z1),
    Z is Z1 + 1.

% trovare xy blocchi non visibili

xy(Block, X, Y) :-
    see(Block, X, Y).

xy(Block, X, Y) :-
    on(Block1, Block),
    xy(Block1, X, Y).

% vedere se blocchi sono impilati

up(B1,B2) :-
    on(B1,B2).

up(B1,B2) :-
    on(B1,B3),
    up(B3,B2).
