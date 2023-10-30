:- discontiguous cell/2.
:- discontiguous adjacent/2.

cell(1,1).
cell(1,2).
cell(1,3).
cell(2,1).
cell(2,2).
cell(2,3).
cell(3,1).
cell(3,2).
cell(3,3).
            

adjacent(cell(1,1), cell(1,1)).
adjacent(cell(1,1), cell(1,2)).
adjacent(cell(1,1), cell(2,1)).
adjacent(cell(1,2), cell(1,2)).
adjacent(cell(1,2), cell(1,3)).
adjacent(cell(1,2), cell(2,2)).
adjacent(cell(1,2), cell(1,1)).
adjacent(cell(1,3), cell(1,3)).
adjacent(cell(1,3), cell(2,3)).
adjacent(cell(1,3), cell(1,2)).
adjacent(cell(2,1), cell(2,1)).
adjacent(cell(2,1), cell(2,2)).
adjacent(cell(2,1), cell(3,1)).
adjacent(cell(2,1), cell(1,1)). 
adjacent(cell(2,2), cell(2,2)).
adjacent(cell(2,2), cell(2,3)).
adjacent(cell(2,2), cell(3,2)).
adjacent(cell(2,2), cell(1,2)).
adjacent(cell(2,2), cell(2,1)). 
adjacent(cell(2,3), cell(2,3)).
adjacent(cell(2,3), cell(3,3)).
adjacent(cell(2,3), cell(2,2)).
adjacent(cell(2,3), cell(1,3)).
adjacent(cell(3,1), cell(3,1)).
adjacent(cell(3,1), cell(3,2)).
adjacent(cell(3,1), cell(2,1)).
adjacent(cell(3,2), cell(3,2)).
adjacent(cell(3,2), cell(3,1)).
adjacent(cell(3,2), cell(3,3)).
adjacent(cell(3,2), cell(2,2)).
adjacent(cell(3,3), cell(3,3)).
adjacent(cell(3,3), cell(3,2)).
adjacent(cell(3,3), cell(2,3)).