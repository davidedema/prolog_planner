%TODO:  aggiungere posizione blocco, rotazione e spostamento
%       rendere la cosa più dichiarativa
%       e ci siamo quasi 

% Pilastro completato, domani finisco porta e se riesco tavolo

% Fatti
blocco(b1, x(1), y(0), z(0), larghezza(1), altezza(2), profondita(1), orientamento(verticale)).
blocco(b2, x(1), y(0), z(0), larghezza(1), altezza(2), profondita(1), orientamento(verticale)).
blocco(b3, x(2), y(0), z(0), larghezza(1), altezza(2), profondita(1), orientamento(verticale)).
blocco(b4, x(0), y(0), z(0), larghezza(4), altezza(2), profondita(4), orientamento(verticale)).
blocco(b5, x(0), y(0), z(0), larghezza(1), altezza(2), profondita(1), orientamento(verticale)).

% Regole

% un blocco è impilabile se in orientamento verticale
impilabile(Blocco) :-
    blocco(Blocco, _, _, _, _, _, _, orientamento(verticale)).

impilabile(Blocco) :-
    blocco(Blocco, _, _, _, _, _, _, orientamento(laterale)),
    ruota_blocco(Blocco, 90).

ruota_blocco(Blocco, Angolo) :-
    format('Blocco ~w ruotato di ~d gradi, ora è in orientamento verticale ~n', [Blocco,Angolo]).

muovi_blocco(Blocco, X, Y, Z) :-
    blocco(Blocco, x(X1), y(Y1), z(Z1), larghezza(L1), altezza(H1), profondita(P1), orientamento(P1X)),
    format('Blocco ~w si trovava in x:~d y:~d z:~d ~n',[Blocco, X1, Y1, Z1]),
    format('Ora si trova in x:~d y:~d z:~d ~n', [X, Y, Z]).

/* check_posizione(Blocco1, Blocco2) :-
    blocco(Blocco1, x(X1), y(Y1), z(Z1), larghezza(L1), altezza(H1), profondita(P1), orientamento(P1X)),
    blocco(Blocco2, x(X2), y(Y2), z(Z2), larghezza(L2), altezza(H2), profondita(P2), orientamento(P2X)),
    ((X1=:=X2) -> (Y1\=Y2);
    (Y1=:=Y2) -> (X1\=X2);
    (X1=:=X2, Y1=:=Y2) -> (Z1\=Z2)). */

pilastro(B1, B2) :-
    blocco(B1, x(X1), y(Y1), z(Z1), larghezza(L1), altezza(H1), profondita(P1), orientamento(P1X)),
    blocco(B2, x(X2), y(Y2), z(Z2), larghezza(L2), altezza(H2), profondita(P2), orientamento(P2X)),
    B1 \= B2,
    L1 = L2,
    P1 = P2,
    ((X1 \= X2; Y1 \= Y2) -> muovi_blocco(B2, X1, Y1, (Z1+H1));muovi_blocco(B2, X2, Y2, Z1+H1)),
    impilabile(B1),
    impilabile(B2),
    NZ2 is Z1 + H1,
    AltezzaP is H1 + H2,
    format('Il blocco ~w si trova in x:~d y:~d z:~d ~n', [B1, X1, Y1, Z1]),
    format('Il blocco ~w si trova in x:~d y:~d z:~d ~n', [B2, X1, Y1, H1]),
    format('Il pilastro si trova in x:~d y:~d ed è alto ~d ~n', [X1, Y1, AltezzaP]).

stessa_dim(B1,B2) :-
    blocco(B1, larghezza(L1), altezza(H1), profondita(P1), orientamento(P1X)),
    blocco(B2, larghezza(L2), altezza(H2), profondita(P2), orientamento(P2X)),
    B1 \= B2,
    L1 = L2,
    H1 = H2,
    P1 = P2.

porta(B1, B2, B3) :-
    blocco(B1, larghezza(L1), altezza(H1), profondita(P1), orientamento(P1X)),
    blocco(B2, larghezza(L2), altezza(H2), profondita(P2), orientamento(P2X)),
    blocco(B3, larghezza(L3), altezza(H3), profondita(P3), orientamento(P3X)),
    B1 \= B2,
    B1 \= B3,
    B2 \= B3,
    impilabile(B1),
    impilabile(B2),
    stessa_dim(B1, B2),
    L3 >= 3.
    

porta(B1, B2, B3) :-
    blocco(B1, larghezza(L1), altezza(H1), profondita(P1), orientamento(P1X)),
    blocco(B2, larghezza(L2), altezza(H2), profondita(P2), orientamento(P2X)),
    blocco(B3, larghezza(L3), altezza(H3), profondita(P3), orientamento(P3X)),
    B1 \= B2,
    B1 \= B3,
    B2 \= B3,
    impilabile(B2),
    impilabile(B3),
    stessa_dim(B2, B3),
    L1 >= 3.

porta(B1, B2, B3) :-
    blocco(B1, larghezza(L1), altezza(H1), profondita(P1), orientamento(P1X)),
    blocco(B2, larghezza(L2), altezza(H2), profondita(P2), orientamento(P2X)),
    blocco(B3, larghezza(L3), altezza(H3), profondita(P3), orientamento(P3X)),
    B1 \= B2,
    B1 \= B3,
    B2 \= B3,
    impilabile(B1),
    impilabile(B3),
    stessa_dim(B1, B3),
    L2 >= 3.

tavolo(B1, B2, B3, B4, B5) :-
    blocco(B1, larghezza(L1), altezza(H1), profondita(P1), orientamento(P1X)),
    blocco(B2, larghezza(L2), altezza(H2), profondita(P2), orientamento(P2X)),
    blocco(B3, larghezza(L3), altezza(H3), profondita(P3), orientamento(P3X)),
    blocco(B4, larghezza(L4), altezza(H4), profondita(P4), orientamento(P4X)),
    blocco(B5, larghezza(L5), altezza(H5), profondita(P5), orientamento(P5X)),
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
    blocco(B1, larghezza(L1), altezza(H1), profondita(P1), orientamento(P1X)),
    blocco(B2, larghezza(L2), altezza(H2), profondita(P2), orientamento(P2X)),
    blocco(B3, larghezza(L3), altezza(H3), profondita(P3), orientamento(P3X)),
    blocco(B4, larghezza(L4), altezza(H4), profondita(P4), orientamento(P4X)),
    blocco(B5, larghezza(L5), altezza(H5), profondita(P5), orientamento(P5X)),
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
    blocco(B1, larghezza(L1), altezza(H1), profondita(P1), orientamento(P1X)),
    blocco(B2, larghezza(L2), altezza(H2), profondita(P2), orientamento(P2X)),
    blocco(B3, larghezza(L3), altezza(H3), profondita(P3), orientamento(P3X)),
    blocco(B4, larghezza(L4), altezza(H4), profondita(P4), orientamento(P4X)),
    blocco(B5, larghezza(L5), altezza(H5), profondita(P5), orientamento(P5X)),
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
    blocco(B1, larghezza(L1), altezza(H1), profondita(P1), orientamento(P1X)),
    blocco(B2, larghezza(L2), altezza(H2), profondita(P2), orientamento(P2X)),
    blocco(B3, larghezza(L3), altezza(H3), profondita(P3), orientamento(P3X)),
    blocco(B4, larghezza(L4), altezza(H4), profondita(P4), orientamento(P4X)),
    blocco(B5, larghezza(L5), altezza(H5), profondita(P5), orientamento(P5X)),
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
    blocco(B1, larghezza(L1), altezza(H1), profondita(P1), orientamento(P1X)),
    blocco(B2, larghezza(L2), altezza(H2), profondita(P2), orientamento(P2X)),
    blocco(B3, larghezza(L3), altezza(H3), profondita(P3), orientamento(P3X)),
    blocco(B4, larghezza(L4), altezza(H4), profondita(P4), orientamento(P4X)),
    blocco(B5, larghezza(L5), altezza(H5), profondita(P5), orientamento(P5X)),
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
