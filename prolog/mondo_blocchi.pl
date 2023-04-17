% TODO:  aggiungere posizione blocco, rotazione e spostamento
%       rendere la cosa più dichiarativa
%       e ci siamo quasi 

% Fatti
blocco(b1, x(1), y(0), z(0), larghezza(1), altezza(2), profondita(1), orientamento(verticale)).
blocco(b2, x(1), y(1), z(0), larghezza(1), altezza(2), profondita(1), orientamento(laterale)).
blocco(b3, x(2), y(0), z(0), larghezza(1), altezza(2), profondita(1), orientamento(verticale)).
blocco(b4, x(0), y(3), z(0), larghezza(1), altezza(2), profondita(1), orientamento(verticale)).
blocco(b5, x(0), y(0), z(0), larghezza(4), altezza(1), profondita(4), orientamento(verticale)).

% Regole

% un blocco è impilabile se in orientamento verticale
impilabile(Blocco) :-
    blocco(Blocco, _, _, _, _, _, _, orientamento(verticale)).

impilabile(Blocco) :-
    blocco(Blocco, _, _, _, _, _, _, orientamento(laterale)),
    ruota_blocco(Blocco, 90).

ruota_blocco(Blocco, Angolo) :-
    writeln('-----Ruotando blocco...-----'),
    format('Blocco ~w ruotato di ~d gradi, ora è in orientamento verticale ~n', [Blocco,Angolo]).

muovi_blocco(Blocco, X, Y, Z) :-
    blocco(Blocco, x(X1), y(Y1), z(Z1), larghezza(L1), altezza(H1), profondita(P1), orientamento(P1X)),
    writeln('-----Spostando blocco...-----'),
    format('Blocco ~w si trovava in x:~d y:~d z:~d ~n',[Blocco, X1, Y1, Z1]),
    format('Ora si trova in x:~d y:~d z:~d ~n', [X, Y, Z]).

pilastro(B1, B2) :-
    % blocchi
    blocco(B1, x(X1), y(Y1), z(Z1), larghezza(L1), altezza(H1), profondita(P1), orientamento(P1X)),
    blocco(B2, x(X2), y(Y2), z(Z2), larghezza(L2), altezza(H2), profondita(P2), orientamento(P2X)),
    % controllo che i blocchi siano diversi
    B1 \= B2,
    % controllo compatibilità dimensione blocchi
    L1 = L2,
    P1 = P2,
    % controllo che i blocchi siano in posizione corretta
    ((X1 \= X2; Y1 \= Y2) -> muovi_blocco(B2, X1, Y1, (Z1+H1));muovi_blocco(B2, X2, Y2, Z1+H1)),
    % controllo che i blocchi siano impilabili
    impilabile(B1),
    impilabile(B2),
    NZ2 is Z1 + H1,
    AltezzaP is H1 + H2,
    writeln('-----Creazione pilastro...-----'),
    format('Il blocco ~w si trova in x:~d y:~d z:~d ~n', [B1, X1, Y1, Z1]),
    format('Il blocco ~w si trova in x:~d y:~d z:~d ~n', [B2, X1, Y1, H1]),
    format('Il pilastro si trova in x:~d y:~d ed è alto ~d ~n', [X1, Y1, AltezzaP]).

porta(B1, B2, B3) :-
    % blocchi
    blocco(B1, x(X1), y(Y1), z(Z1), larghezza(L1), altezza(H1), profondita(P1), orientamento(P1X)),
    blocco(B2, x(X2), y(Y2), z(Z2), larghezza(L2), altezza(H2), profondita(P2), orientamento(P2X)),
    blocco(B3, x(X3), y(Y3), z(Z3), larghezza(L3), altezza(H3), profondita(P3), orientamento(P3X)),
    % controllo che i blocchi siano diversi
    B1 \= B2,
    B1 \= B3,
    B2 \= B3,
    % controllo compatibilità dimensione blocchi
    L1 = L2,
    P1 = P2,
    H1 = H2,
    L3 >=3,
    % controllo che i blocchi siano impilabili
    impilabile(B1),
    impilabile(B2),
    impilabile(B3),
    % controllo che i blocchi siano in posizione corretta
    ((X2 \= (X1+L3-1); Y2 \= Y1 ) -> muovi_blocco(B2, X1+L3-1, Y1, Z1)),
    ((X3 \= (X1+(L3/2)-1); Y3\=Y1) -> muovi_blocco(B3, X1+(L3/2)-1, Y1, H1+1)),
    writeln('-----Creazione porta...-----'),
    format('Il blocco ~w si trova in x:~d y:~d z:~d ~n', [B1, X1, Y1, Z1]),
    format('Il blocco ~w si trova in x:~d y:~d z:~d ~n', [B2, X1+L3-1, Y1, Z2]),
    format('Il blocco ~w si trova in x:~d y:~d z:~d ~n', [B3, X1+(L3/2)-1, Y1, H1+1]),
    format('La porta si trova in x:~d y:~d z:~d ~n', [X1+(L3/2)-1, Y1, Z1]),
    format('La porta è alta ~d ~n', [H1]).
    

tavolo(B1, B2, B3, B4, B5) :-
    % blocchi
    blocco(B1, x(X1), y(Y1), z(Z1), larghezza(L1), altezza(H1), profondita(P1), orientamento(P1X)),
    blocco(B2, x(X2), y(Y2), z(Z2), larghezza(L2), altezza(H2), profondita(P2), orientamento(P2X)),
    blocco(B3, x(X3), y(Y3), z(Z3), larghezza(L3), altezza(H3), profondita(P3), orientamento(P3X)),
    blocco(B4, x(X4), y(Y4), z(Z4), larghezza(L4), altezza(H4), profondita(P4), orientamento(P4X)),
    blocco(B5, x(X5), y(Y5), z(Z5), larghezza(L5), altezza(H5), profondita(P5), orientamento(P5X)),
    % controllo che i blocchi siano diversi
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
    % controllo che i blocchi siano impilabili
    impilabile(B1),
    impilabile(B2),
    impilabile(B3),
    impilabile(B4),
    % controllo compatibilità dimensione blocchi
    L1 = L2,
    L1 = L3,
    L1 = L4,
    P1 = P2,
    P1 = P3,
    P1 = P4,
    H1 = H2,
    H1 = H3,
    H1 = H4,
    L5 >= 4,
    P5 >= 4,
    % controllo che i blocchi siano in posizione corretta
    ((X2 \= (X1+L5-1); Y2 \= Y1) -> muovi_blocco(B2, X1+L5-1, Y1, Z1)),
    ((X3 \= X1; Y3 \= (Y1+P5-1)) -> muovi_blocco(B3, X1, Y1+P5-1, Z1)),
    ((X4 \= (X1+L5-1); Y4 \= (Y1+P5-1)) -> muovi_blocco(B4, X1+L5-1, Y1+P5-1, Z1)),
    ((X5 \= (X1+(L5/2)-1); Y5 \= (Y1+(P5/2)-1)) -> muovi_blocco(B5, X1+(L5/2)-1, Y1+(P5/2)-1, H1+1)),
    writeln('-----Creazione tavolo...-----'),
    format('Il blocco ~w si trova in x:~d y:~d z:~d ~n', [B1, X1, Y1, Z1]),
    format('Il blocco ~w si trova in x:~d y:~d z:~d ~n', [B2, X1+L5-1, Y1, Z2]),
    format('Il blocco ~w si trova in x:~d y:~d z:~d ~n', [B3, X1, Y1+P5-1, Z3]),
    format('Il blocco ~w si trova in x:~d y:~d z:~d ~n', [B4, X1+L5-1, Y1+P5-1, Z4]),
    format('Il blocco ~w si trova in x:~d y:~d z:~d ~n', [B5, X1+(L5/2)-1, Y1+(P5/2)-1, H1+1]),
    format('Il tavolo si trova in x:~d y:~d z:~d ~n', [X1+(L5/2)-1, Y1+(P5/2)-1, Z1]),
    format('Il tavolo è alto ~d ~n', [H1]).

