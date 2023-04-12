% Fatti
blocco(b1, larghezza(1), altezza(2), profondita(1), posizione(verticale)).
blocco(b2, larghezza(1), altezza(2), profondita(1), posizione(verticale)).
blocco(b3, larghezza(1), altezza(2), profondita(1), posizione(verticale)).
blocco(b4, larghezza(4), altezza(2), profondita(4), posizione(verticale)).
blocco(b5, larghezza(1), altezza(2), profondita(1), posizione(verticale)).

% Regole
% un blocco è impilabile se in posizione verticale
impilabile(Blocco) :-
    blocco(Blocco, _, _, _, posizione(verticale)).

pilastro(B1, B2) :-
    blocco(B1, larghezza(L1), altezza(H1), profondita(P1), posizione(P1X)),
    blocco(B2, larghezza(L2), altezza(H2), profondita(P2), posizione(P2X)),
    B1 \= B2,
    L1 = L2,
    P1 = P2,
    impilabile(B1),
    impilabile(B2),
    AltezzaP is H1 + H2.

stessa_dim(B1,B2) :-
    blocco(B1, larghezza(L1), altezza(H1), profondita(P1), posizione(P1X)),
    blocco(B2, larghezza(L2), altezza(H2), profondita(P2), posizione(P2X)),
    B1 \= B2,
    L1 = L2,
    H1 = H2,
    P1 = P2.

porta(B1, B2, B3) :-
    blocco(B1, larghezza(L1), altezza(H1), profondita(P1), posizione(P1X)),
    blocco(B2, larghezza(L2), altezza(H2), profondita(P2), posizione(P2X)),
    blocco(B3, larghezza(L3), altezza(H3), profondita(P3), posizione(P3X)),
    B1 \= B2,
    B1 \= B3,
    B2 \= B3,
    impilabile(B1),
    impilabile(B2),
    stessa_dim(B1, B2),
    L3 >= 3.

porta(B1, B2, B3) :-
    blocco(B1, larghezza(L1), altezza(H1), profondita(P1), posizione(P1X)),
    blocco(B2, larghezza(L2), altezza(H2), profondita(P2), posizione(P2X)),
    blocco(B3, larghezza(L3), altezza(H3), profondita(P3), posizione(P3X)),
    B1 \= B2,
    B1 \= B3,
    B2 \= B3,
    impilabile(B2),
    impilabile(B3),
    stessa_dim(B2, B3),
    L1 >= 3.

porta(B1, B2, B3) :-
    blocco(B1, larghezza(L1), altezza(H1), profondita(P1), posizione(P1X)),
    blocco(B2, larghezza(L2), altezza(H2), profondita(P2), posizione(P2X)),
    blocco(B3, larghezza(L3), altezza(H3), profondita(P3), posizione(P3X)),
    B1 \= B2,
    B1 \= B3,
    B2 \= B3,
    impilabile(B1),
    impilabile(B3),
    stessa_dim(B1, B3),
    L2 >= 3.

tavolo(B1, B2, B3, B4, B5) :-
    blocco(B1, larghezza(L1), altezza(H1), profondita(P1), posizione(P1X)),
    blocco(B2, larghezza(L2), altezza(H2), profondita(P2), posizione(P2X)),
    blocco(B3, larghezza(L3), altezza(H3), profondita(P3), posizione(P3X)),
    blocco(B4, larghezza(L4), altezza(H4), profondita(P4), posizione(P4X)),
    blocco(B5, larghezza(L5), altezza(H5), profondita(P5), posizione(P5X)),
    B1 \= B2,
    B1 \= B3,
    B1 \= B4,
    B1 \= B5,
    B2 \= B3,
    B2 \= B4,
    B2 \= B5,
    B3 \= B4,
    B3 \= B5,
    B4 \= B5,
    impilabile(B1),
    impilabile(B2),
    impilabile(B3),
    impilabile(B4),
    stessa_dim(B1, B2),
    stessa_dim(B1, B3),
    stessa_dim(B1, B4),
    L5 >= 4,
    P5 >= 4.

tavolo(B1, B2, B3, B4, B5) :-
    blocco(B1, larghezza(L1), altezza(H1), profondita(P1), posizione(P1X)),
    blocco(B2, larghezza(L2), altezza(H2), profondita(P2), posizione(P2X)),
    blocco(B3, larghezza(L3), altezza(H3), profondita(P3), posizione(P3X)),
    blocco(B4, larghezza(L4), altezza(H4), profondita(P4), posizione(P4X)),
    blocco(B5, larghezza(L5), altezza(H5), profondita(P5), posizione(P5X)),
    B1 \= B2,
    B1 \= B3,
    B1 \= B4,
    B1 \= B5,
    B2 \= B3,
    B2 \= B4,
    B2 \= B5,
    B3 \= B4,
    B3 \= B5,
    B4 \= B5,
    impilabile(B2),
    impilabile(B3),
    impilabile(B4),
    impilabile(B5),
    stessa_dim(B2, B3),
    stessa_dim(B2, B4),
    stessa_dim(B2, B5),
    L1 >= 4,
    P1 >= 4.
    
tavolo(B1, B2, B3, B4, B5) :-
    blocco(B1, larghezza(L1), altezza(H1), profondita(P1), posizione(P1X)),
    blocco(B2, larghezza(L2), altezza(H2), profondita(P2), posizione(P2X)),
    blocco(B3, larghezza(L3), altezza(H3), profondita(P3), posizione(P3X)),
    blocco(B4, larghezza(L4), altezza(H4), profondita(P4), posizione(P4X)),
    blocco(B5, larghezza(L5), altezza(H5), profondita(P5), posizione(P5X)),
    B1 \= B2,
    B1 \= B3,
    B1 \= B4,
    B1 \= B5,
    B2 \= B3,
    B2 \= B4,
    B2 \= B5,
    B3 \= B4,
    B3 \= B5,
    B4 \= B5,
    impilabile(B1),
    impilabile(B3),
    impilabile(B4),
    impilabile(B5),
    stessa_dim(B1, B3),
    stessa_dim(B1, B4),
    stessa_dim(B1, B5),
    L2 >= 4,
    P2 >= 4.

tavolo(B1, B2, B3, B4, B5) :-
    blocco(B1, larghezza(L1), altezza(H1), profondita(P1), posizione(P1X)),
    blocco(B2, larghezza(L2), altezza(H2), profondita(P2), posizione(P2X)),
    blocco(B3, larghezza(L3), altezza(H3), profondita(P3), posizione(P3X)),
    blocco(B4, larghezza(L4), altezza(H4), profondita(P4), posizione(P4X)),
    blocco(B5, larghezza(L5), altezza(H5), profondita(P5), posizione(P5X)),
    B1 \= B2,
    B1 \= B3,
    B1 \= B4,
    B1 \= B5,
    B2 \= B3,
    B2 \= B4,
    B2 \= B5,
    B3 \= B4,
    B3 \= B5,
    B4 \= B5,
    impilabile(B1),
    impilabile(B2),
    impilabile(B4),
    impilabile(B5),
    stessa_dim(B1, B2),
    stessa_dim(B1, B4),
    stessa_dim(B1, B5),
    L3 >= 4,
    P3 >= 4.

tavolo(B1, B2, B3, B4, B5) :-
    blocco(B1, larghezza(L1), altezza(H1), profondita(P1), posizione(P1X)),
    blocco(B2, larghezza(L2), altezza(H2), profondita(P2), posizione(P2X)),
    blocco(B3, larghezza(L3), altezza(H3), profondita(P3), posizione(P3X)),
    blocco(B4, larghezza(L4), altezza(H4), profondita(P4), posizione(P4X)),
    blocco(B5, larghezza(L5), altezza(H5), profondita(P5), posizione(P5X)),
    B1 \= B2,
    B1 \= B3,
    B1 \= B4,
    B1 \= B5,
    B2 \= B3,
    B2 \= B4,
    B2 \= B5,
    B3 \= B4,
    B3 \= B5,
    B4 \= B5,
    impilabile(B1),
    impilabile(B2),
    impilabile(B3),
    impilabile(B5),
    stessa_dim(B1, B2),
    stessa_dim(B1, B3),
    stessa_dim(B1, B5),
    L4 >= 4,
    P4 >= 4.
