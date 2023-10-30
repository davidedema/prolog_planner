action(
    move_to_cell(agent(A), cell(Xi, Yi), cell(Xf, Yf)),
    [   
        busy(agent(A), cell(Xi, Yi))
    ],
    [busy(_, cell(Xf, Yf))],
    [],
    [cell(Xi, Yi), cell(Xf, Yf), adjacent(cell(Xi, Yi), cell(Xf, Yf))],
    [add(busy(agent(A), cell(Xf, Yf))), del(busy(agent(A), cell(Xi, Yi)))]
).

action(
    stay(agent(A), cell(X, Y)),
    [busy(agent(A), cell(X, Y))],
    [],
    [],
    [cell(X, Y), adjacent(cell(X, Y), cell(X, Y))],
    []
).
