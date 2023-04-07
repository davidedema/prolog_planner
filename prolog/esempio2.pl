% see(Block, X, Y)
see(a, 1, 1).
see(b, 2, 1).
see(c, 3, 1).

% face(Block, Face)
face(a, up).
face(b, side).
face(c, bottom).

% is_stackable(Block).
is_stackable(Block) :-
    face(Block, up).

is_stackable(Block) :-
    rotate(Block, RotBlock),
    is_stackable(RotBlock).

% rotate(Block, RotBlock)   
rotate(Block, RotBlock) :-
    face(Block, side),
    face(RotBlock, up).
    Block = RotBlock.

rotate(Block, RotBlock) :-
    face(Block, bottom),
    face(RotBlock, up),
    Block = RotBlock.

    
