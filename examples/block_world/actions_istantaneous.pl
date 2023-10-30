% This actions indicates the gripping of a block B by an agent A.
% It needs to verify that there are two blocks stacked one on top of the other,
% that the agent is available and that the block B is movable, i.e., there is
% nothing on top of it. It also must check that the block is not being gripped
% by other agents. The effects of the actions are to remove the agent from the 
% list of available agents, to remove the clearness of the block and to add
% that the block is being gripped by the agent A.
action(
	grip_on(A, B), 
	[on(B, B1, X, Y), available(A), clear(B)],
	[gripped(_, B), gripping(_, B)],
	[],
	[],
	[del(available(A)), del(clear(B)), add(gripped(A, B))]
).

% This actions indicates the gripping of a block B by an agent A.
% It needs to verify that block B is on top of the table,
% that the agent is available and that the block B is movable, i.e., there is
% nothing on top of it. It also must check that the block is not being gripped
% by other agents. The effects of the actions are to remove the agent from the 
% list of available agents, to remove the clearness of the block and to add
% that the block is being gripped by the agent A.
action(
	grip_ontable(A, B), 
	[ontable(B, X, Y), available(A), clear(B)],
	[gripped(_, B), gripping(_, B)],
	[],
	[],
	[del(available(A)), del(clear(B)), add(gripped(A, B))]
).

% This actions indicates that the agent A is releasing the block B. To do this, 
% it needs to check that the block has been moved to the correct position and
% that it is gripped. As effects, it will remove the moved and gripped
% predicate, it will state that the block is clear again and that the agent is 
% available.
action(
	release(A, B),
	[moved(A, B, X, Y, X1, Y1), gripped(A, B)],
	[],
	[],
	[],
	[del(moved(A, B, X, Y, X1, Y1)), del(gripped(A, B)), add(clear(B)), add(available(A))]
).

% This actions indicates that we are moving a block that was on top of another. 
% To do this, we need to check that the block was actually on top of another
% block B1 and that the agent has already gripped the block B. As the effects, 
% it will delete the predicate that states that the blocks are ontop of each
% other and it will add predicates to state that block B is moving and that the
% block that was below is now clear to be picked-up.
action(
	move_block_from_on(A, B, X, Y, X1, Y1),
	[on(B, B1, X, Y), gripped(A, B)],
	[],
	[],
	[],
	[
		del(on(B, B1, X, Y)),
		add(clear(B1)), add(moving(A, B, X, Y, X1, Y1))
	]
).
% This actions indicates that we are moving a block that was on the table.
% To do this, we need to check that the block was actually on the table. As the
% effects, it will delete the predicate that states that the block was on the 
% table and it will add a predicate to state that block B is moving.
action(
	move_block_from_ontable(A, B, X, Y, X1, Y1),
	[ontable(B, X, Y), gripped(A, B)],
	[],
	[],
	[],
	[
		del(ontable(B, X, Y)) ,
		add(moving(A, B, X, Y, X1, Y1))
	]
).
% This action indicates that the movement puts the block on the table in a
% certain position. Before being planned, it checks that the block is actually
% being moved. As effects, it adds that it has been moved, also it removes that
% it was moving and it adds that it is on the table. 
action(
	move_block_to_table(A, B, X, Y, X1, Y1),
	[moving(A, B, X, Y, X1, Y1)],
	[],
	[],
	[],
	[
		del(moving(A, B, X, Y, X1, Y1)),
		add(ontable(B, X1, Y1)), add(moved(A, B, X, Y, X1, Y1))
	]
).
% This action indicates that the movement puts the block on top of another
% block. Before being planned, it checks that the block is actually
% being moved and that the block ontop of which the B should be stacked, is
% free. As effects, it adds that it has been moved, also it removes that
% it was moving and it adds that it is on the table and it removes the
% predicate that states that B1 is clear. 
action(
	move_block_to_on(A, B, X, Y, X1, Y1),
	[moving(A, B, X, Y, X1, Y1), clear(B1)],
	[],
	[],
	[],
	[
		del(moving(A, B, X, Y, X1, Y1)), del(clear(B1)),
		add(on(B, B1, X1, Y1)), add(moved(A, B, X, Y, X1, Y1))
	]
).
