% TODO: FARE UNICO RETRACT E ASSERT PER TUTTA LA COSTRUZIONE, RENDERE MENO IMPERATIVO

:- dynamic blocco/11.

% PER ORIENTAMENTO INDICHIAMO LA FACCIA SU CUI SI APPOGGIA IL BLOCCO
% 1 = fondo
% 2 = sopra
% 3 = lato
% 4 = lato 
% 5 = lato
% 6 = lato 

% Fatti
blocco(b1, x(1), y(0), z(0), larghezza(1), altezza(2), profondita(1), orientamento(1), contattol(tavolo), contattoh(aria), shape(parallelepipedo)).
blocco(b2, x(1), y(1), z(0), larghezza(1), altezza(2), profondita(1), orientamento(3), contattol(tavolo), contattoh(aria), shape(parallelepipedo)).
blocco(b3, x(2), y(0), z(0), larghezza(1), altezza(2), profondita(1), orientamento(1), contattol(tavolo), contattoh(aria), shape(parallelepipedo)).
blocco(b4, x(0), y(3), z(0), larghezza(1), altezza(2), profondita(1), orientamento(1), contattol(tavolo), contattoh(aria), shape(parallelepipedo)).
blocco(b5, x(0), y(0), z(0), larghezza(4), altezza(1), profondita(4), orientamento(1), contattol(tavolo), contattoh(aria), shape(parallelepipedo)).

% Regole
% print blocco
print_blocco(Blocco) :-
    blocco(Blocco, x(X), y(Y), z(Z), larghezza(L), altezza(H), profondita(P), orientamento(O), contattol(CL), contattoh(CH), shape(S)),
    format('Blocco ~w: ~n', [Blocco]),
    format('x: ~d y: ~d z: ~d ~n', [X, Y, Z]),
    format('Larghezza: ~d Altezza: ~d Profondità: ~d ~n', [L, H, P]),
    format('Orientamento: ~d ~n', [O]),
    format('Contatto Basso: ~w Contatto Alto: ~w ~n', [CL, CH]),
    format('Shape: ~w ~n', [S]).

% un blocco è impilabile se in orientamento verticale
impilabile(Blocco) :-
    blocco(Blocco, _, _, _, _, _, _, orientamento(1), _, _, _).

impilabile(Blocco) :-
    blocco(Blocco, x(X), y(Y), z(Z), larghezza(L), altezza(H), profondita(P), orientamento(O), contattol(CL), contattoh(CH), shape(S)),
    (O is 3; O is 4; O is 5; O is 6),
    ruota_blocco(Blocco, 90),
    retract(blocco(Blocco, x(X), y(Y), z(Z), larghezza(L), altezza(H), profondita(P), orientamento(O), contattol(CL), contattoh(CH), shape(S))),
    assertz(blocco(Blocco, x(X), y(Y), z(Z), larghezza(L), altezza(H), profondita(P), orientamento(1), contattol(CL), contattoh(CH), shape(S))).

ruota_blocco(Blocco, Angolo) :-
    writeln('-----Ruotare Blocco-----'),
    format('Blocco ~w deve essere ruotato di ~d gradi ~n', [Blocco,Angolo]).

muovi_blocco(Blocco, X, Y, Z) :-
    blocco(Blocco, x(X1), y(Y1), z(Z1), larghezza(L), altezza(H), profondita(P), orientamento(O), contattol(CL), contattoh(CH), shape(S)),
    writeln('-----Spostare blocco...-----'),
    format('Blocco ~w si trovava in x:~d y:~d z:~d ~n',[Blocco, X1, Y1, Z1]),
    format('Bisogna muoverlo in in x:~d y:~d z:~d ~n', [X, Y, Z]),
    retract(blocco(Blocco, x(X1), y(Y1), z(Z1), larghezza(L), altezza(H), profondita(P), orientamento(O), contattol(CL), contattoh(CH), shape(S))),
    assertz(blocco(Blocco, x(X), y(Y), z(Z), larghezza(L), altezza(H), profondita(P), orientamento(O), contattol(CL), contattoh(CH), shape(S))).
    

all_diff(L) :-
    \+ (select(X,L,R), memberchk(X,R)).


% B1 sotto B2 sopra
pilastro(B1, B2) :-
    % blocchi
    blocco(B1, x(X1), y(Y1), z(Z1), larghezza(L1), altezza(H1), profondita(P1), orientamento(O1), contattol(CL1), contattoh(CH1), shape(S1)),
    blocco(B2, x(X2), y(Y2), z(Z2), larghezza(L2), altezza(H2), profondita(P2), orientamento(O2), contattol(CL2), contattoh(CH2), shape(S2)),
    % PRECONDIZIONI
    % controllo che i blocchi siano diversi
    all_diff([B1, B2]),
    % controllo compatibilità dimensione blocchi
    L1 = L2,
    P1 = P2,
    % controllo che i blocchi siano orientati correttamente
    % orientamento non è 1, allora ruota blocco, eliminare implilabile che è ridondante
    impilabile(B1),
    impilabile(B2),
    CL2 = 'tavolo',
    CH1 = 'aria',
    S1 = 'parallelepipedo',
    S2 = 'parallelepipedo',
    % controllo che i blocchi siano in posizione corretta
    % POSTCONDIZIONI
    ((X1 \= X2; Y1 \= Y2) -> muovi_blocco(B2, X1, Y1, (Z1+H1));muovi_blocco(B2, X2, Y2, Z1+H1)),
    NZ2 is Z1 + H1,
    AltezzaP is H1 + H2,
    writeln('-----Creazione pilastro...-----'),
    format('Il blocco ~w si trova in x:~d y:~d z:~d ~n', [B1, X1, Y1, Z1]),
    format('Il blocco ~w si trova in x:~d y:~d z:~d ~n', [B2, X1, Y1, H1]),
    format('Il pilastro si trova in x:~d y:~d ed è alto ~d ~n', [X1, Y1, AltezzaP]),
    % AGGIORNAMENTO KNOWLEDGE BASE
    retract(blocco(B1, x(X1), y(Y1), z(Z1), larghezza(L1), altezza(H1), profondita(P1), orientamento(O1), contattol(CL1), contattoh(CH1), shape(S1))),
    retract(blocco(B2, x(X2), y(Y2), z(Z2), larghezza(L2), altezza(H2), profondita(P2), orientamento(O2), contattol(CL2), contattoh(CH2), shape(S2))), % FIXME: C'È SALVATO VECCHIO BLOCCO QUINDI NON TROVA QUESTO DA CANCELLARE %
    assertz(blocco(B1, x(X1), y(Y1), z(Z1), larghezza(L1), altezza(H1), profondita(P1), orientamento(O1), contattol(CL1), contattoh(B2), shape(S1))),
    assertz(blocco(B2, x(X2), y(Y2), z(Z2), larghezza(L2), altezza(H2), profondita(P2), orientamento(O2), contattol(B1), contattoh(CH2), shape(S2))).
    

porta(B1, B2, B3) :-
    % blocchi
    blocco(B1, x(X1), y(Y1), z(Z1), larghezza(L1), altezza(H1), profondita(P1), orientamento(O1), contattol(CL1), contattoh(CH1), shape(S1)),
    blocco(B2, x(X2), y(Y2), z(Z2), larghezza(L2), altezza(H2), profondita(P2), orientamento(O2), contattol(CL2), contattoh(CH2), shape(S2)),
    blocco(B3, x(X3), y(Y3), z(Z3), larghezza(L3), altezza(H3), profondita(P3), orientamento(O3), contattol(CL3), contattoh(CH3), shape(S3)),
    % controllo che i blocchi siano diversi
    all_diff([B1, B2, B3]),
    % controllo compatibilità dimensione blocchi
    L1 = L2,
    P1 = P2,
    H1 = H2,
    L3 >=3,
    % controllo che i blocchi siano impilabili
    impilabile(B1),
    impilabile(B2),
    impilabile(B3),
    CH1 = 'aria',
    CH2 = 'aria',
    CL3 = 'tavolo',
    % controllo che i blocchi siano in posizione corretta
    ((X2 \= (X1+L3-1); Y2 \= Y1 ) -> muovi_blocco(B2, X1+L3-1, Y1, Z1)),
    ((X3 \= (X1+(L3/2)-1); Y3\=Y1) -> muovi_blocco(B3, X1+(L3/2)-1, Y1, H1+1)),
    writeln('-----Creazione porta...-----'),
    format('Il blocco ~w si trova in x:~d y:~d z:~d ~n', [B1, X1, Y1, Z1]),
    format('Il blocco ~w si trova in x:~d y:~d z:~d ~n', [B2, X1+L3-1, Y1, Z2]),
    format('Il blocco ~w si trova in x:~d y:~d z:~d ~n', [B3, X1+(L3/2)-1, Y1, H1+1]),
    format('La porta si trova in x:~d y:~d z:~d ~n', [X1+(L3/2)-1, Y1, Z1]),
    format('La porta è alta ~d ~n', [H1]),
    retract(blocco(B1, x(X1), y(Y1), z(Z1), larghezza(L1), altezza(H1), profondita(P1), orientamento(O1), contattol(CL1), contattoh(CH1), shape(S1))),
    retract(blocco(B2, x(X2), y(Y2), z(Z2), larghezza(L2), altezza(H2), profondita(P2), orientamento(O2), contattol(CL2), contattoh(CH2), shape(S2))),
    retract(blocco(B3, x(X3), y(Y3), z(Z3), larghezza(L3), altezza(H3), profondita(P3), orientamento(O3), contattol(CL3), contattoh(CH3), shape(S3))),
    assertz(blocco(B1, x(X1), y(Y1), z(Z1), larghezza(L1), altezza(H1), profondita(P1), orientamento(O1), contattol(CL1), contattoh(B3), shape(S1))),
    assertz(blocco(B2, x(X1+L3-1), y(Y1), z(Z2), larghezza(L2), altezza(H2), profondita(P2), orientamento(O2), contattol(CL1), contattoh(B3), shape(S2))),
    assertz(blocco(B3, x(X1+(L3/2)-1), y(Y1), z(H1+1), larghezza(L3), altezza(H3), profondita(P3), orientamento(O3), contattol([B1,B2]), contattoh(CH3), shape(S3))).

tavolo(B1, B2, B3, B4, B5) :-
    % blocchi
    blocco(B1, x(X1), y(Y1), z(Z1), larghezza(L1), altezza(H1), profondita(P1), orientamento(O1), contattol(CL1), contattoh(CH1), shape(S1)),
    blocco(B2, x(X2), y(Y2), z(Z2), larghezza(L2), altezza(H2), profondita(P2), orientamento(O2), contattol(CL2), contattoh(CH2), shape(S2)),
    blocco(B3, x(X3), y(Y3), z(Z3), larghezza(L3), altezza(H3), profondita(P3), orientamento(O3), contattol(CL3), contattoh(CH3), shape(S3)),
    blocco(B4, x(X4), y(Y4), z(Z4), larghezza(L4), altezza(H4), profondita(P4), orientamento(O4), contattol(CL4), contattoh(CH4), shape(S4)),
    blocco(B5, x(X5), y(Y5), z(Z5), larghezza(L5), altezza(H5), profondita(P5), orientamento(O5), contattol(CL5), contattoh(CH5), shape(S5)),
    % controllo che i blocchi siano diversi
    all_diff([B1, B2, B3, B4, B5]),
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
    L5 >= 3,
    P5 >= 3,
    % controllo che i blocchi siano impilabili
    impilabile(B1),
    impilabile(B2),
    impilabile(B3),
    impilabile(B4),
    impilabile(B5),
    CH1 = 'aria',
    CH2 = 'aria',
    CH3 = 'aria',
    CH4 = 'aria',
    CL5 = 'tavolo',
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
    format('Il tavolo è alto ~d ~n', [H1]),
    retract(blocco(B5, x(X5), y(Y5), z(Z5), larghezza(L5), altezza(H5), profondita(P5), orientamento(O5), contattol(CL5), contattoh(CH5), shape(S5))),
    retract(blocco(B4, x(X4), y(Y4), z(Z4), larghezza(L4), altezza(H4), profondita(P4), orientamento(O4), contattol(CL4), contattoh(CH4), shape(S4))),
    retract(blocco(B3, x(X3), y(Y3), z(Z3), larghezza(L3), altezza(H3), profondita(P3), orientamento(O3), contattol(CL3), contattoh(CH3), shape(S3))),
    retract(blocco(B2, x(X2), y(Y2), z(Z2), larghezza(L2), altezza(H2), profondita(P2), orientamento(O2), contattol(CL2), contattoh(CH2), shape(S2))),
    retract(blocco(B1, x(X1), y(Y1), z(Z1), larghezza(L1), altezza(H1), profondita(P1), orientamento(O1), contattol(CL1), contattoh(CH1), shape(S1))),
    assertz(blocco(B5, x(X1+(L5/2)-1), y(Y1+(P5/2)-1), z(Z1), larghezza(L5), altezza(H1), profondita(P5), orientamento(O5), contattol([B1,B2,B3,B4]), contattoh(CH5), shape(S5))),
    assertz(blocco(B4, x(X1+L5-1), y(Y1+P5-1), z(Z4), larghezza(L5), altezza(H1), profondita(P5), orientamento(O4), contattol(CL4), contattoh(B5), shape(S4))),
    assertz(blocco(B3, x(X1), y(Y1+P5-1), z(Z3), larghezza(L5), altezza(H1), profondita(P5), orientamento(O3), contattol(CL3), contattoh(B5), shape(S3))),
    assertz(blocco(B2, x(X1+L5-1), y(Y1), z(Z2), larghezza(L5), altezza(H1), profondita(P5), orientamento(O2), contattol(CL2), contattoh(B5), shape(S2))),
    assertz(blocco(B1, x(X1), y(Y1), z(Z1), larghezza(L5), altezza(H1), profondita(P5), orientamento(O1), contattol(CL1), contattoh(B5), shape(S1))).
