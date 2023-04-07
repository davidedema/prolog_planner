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

    


% robe ancora da testare ma stra utili

% Fatti
blocco(b1, altezza(20), larghezza(20), profondita(20), posizione(verticale)).
blocco(b2, altezza(20), larghezza(20), profondita(20), posizione(verticale)).
contatto(b1, b2, superficie_superiore, superficie_inferiore).

% Regole
pilastro(X, Y) :-
    blocco(X, altezza(H1), larghezza(L1), profondita(P1), posizione(P1X)),
    blocco(Y, altezza(H2), larghezza(L2), profondita(P2), posizione(P2X)),
    X \= Y,
    contatto(X, Y, superficie_superiore, superficie_inferiore),
    L1 = L2,
    P1 = P2,
    H1 + H2 =< altezza_massima_pilastro,
    (
        (P1X == verticale, P2X == verticale);
        (P1X == orizzontale, P2X == verticale, ruota_blocco(X));
        (P1X == verticale, P2X == orizzontale, ruota_blocco(Y));
        (P1X == orizzontale, P2X == orizzontale, ruota_blocco(X), ruota_blocco(Y))
    ).

ruota_blocco(Blocco) :-
    retract(blocco(Blocco, _, _, _, posizione(P))),
    (P == verticale -> NP = orizzontale ; NP = verticale),
    assertz(blocco(Blocco, _, _, _, posizione(NP))).
