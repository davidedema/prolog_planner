action(
	grip_on_start(A, B), 
	[on(B, B1, X, Y), available(A), clear(B)],
	[gripped(_, B), gripping(_, B)],
	[on(B, B1, X, Y)],
	[],
	[del(available(A)), add(gripping(A, B))]
).
action(
	grip_ontable_start(A, B), 
	[ontable(B, X, Y), available(A), clear(B)],
	[gripped(_, B), gripping(_, B)],
	[ontable(B, X, Y)],
	[],
	[del(available(A)), add(gripping(A, B))]
).
action(
	grip_buffer_start(A, B), 
	[buffer(B, _X, _Y), available(A), clear(B)],
	[gripped(_, B), gripping(_, B)],
	[],
	[],
	[del(available(A)), add(gripping(A, B))]
).
action(
	grip_end(A, B), 
	[gripping(A, B)],
	[notavalidpredicate(A)],
	[notavalidpredicate(A)],
	[],
	[del(clear(B)), del(gripping(A, B)), add(gripped(A, B))]
).

action(
	release_start(A, B),
	[moved(A, B, X, Y, X1, Y1), gripped(A, B)],
	[],
	[],
	[],
	[
		del(moved(A, B, X, Y, X1, Y1)), del(gripped(A, B)),
		add(releasing(A, B))
	]
).

action(
	release_end(A, B),
	[releasing(A, B)],
	[],
	[],
	[],
	[
		del(releasing(A, B)),
		add(clear(B)), add(available(A))
	]
).

action(
	move_block_on_start(A, B, X, Y, X1, Y1),
	[on(B, B1, X, Y), gripped(A, B)],
	[],
	[],
	[],
	[
		del(on(B, B1, X, Y)),
		add(clear(B1)), add(moving(A, B, X, Y, X1, Y1))
	]
).
action(
	move_block_ontable_start(A, B, X, Y, X1, Y1),
	[ontable(B, X, Y), gripped(A, B)],
	[],
	[],
	[],
	[
		del(ontable(B, X, Y)) ,
		add(moving(A, B, X, Y, X1, Y1))
	]
).
action(
	move_block_buffer_start(A, B, X, Y, X1, Y1),
	[buffer(B, X, Y), gripped(A, B)],
	[],
	[],
	[],
	[
		del(buffer(B, X, Y)),
		add(moving(A, B, X, Y, X1, Y1))
	]
).

action(
	move_block_to_table_end(A, B, X, Y, X1, Y1),
	[moving(A, B, X, Y, X1, Y1)],
	[],
	[],
	[ontable(B, X1, Y1), diffPos(X, X1, Y, Y1)],
	[
		del(moving(A, B, X, Y, X1, Y1)),
		add(ontable(B, X1, Y1)), add(moved(A, B, X, Y, X1, Y1))
	]
).
action(
	move_block_to_on_end(A, B, X, Y, X1, Y1),
	[moving(A, B, X, Y, X1, Y1), clear(B1)],
	[],
	[],
	[on(B, B1, X1, Y1), diffPos(X, X1, Y, Y1)],
	[
		del(moving(A, B, X, Y, X1, Y1)), del(clear(B1)),
		add(on(B, B1, X1, Y1)), add(moved(A, B, X, Y, X1, Y1))
	]
).
action(
	move_block_to_buffer_end(A, B, X, Y, X1, Y1),
	[moving(A, B, X, Y, X1, Y1)],
	[buffer(_B, X1, Y1)],
	[],
	[pos(X1, Y1), diffPos(X, X1, Y, Y1)],
	[
		del(moving(A, B, X, Y, X1, Y1)),
		add(buffer(B, X1, Y1)), add(moved(A, B, X, Y, X1, Y1))
	]
).

