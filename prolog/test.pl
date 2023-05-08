% TODO: FARE UNICO RETRACT E ASSERT PER TUTTA LA COSTRUZIONE, RENDERE MENO IMPERATIVO

:- dynamic blocco/12.
:- dynamic count/2.

% PER ORIENTAMENTO INDICHIAMO LA FACCIA SU CUI SI APPOGGIA IL BLOCCO
% 1 = fondo
% 2 = sopra
% 3 = lato
% 4 = lato 
% 5 = lato
% 6 = lato 

% Fatti
blocco(b1, x(1), y(0), z(0), larghezza(1), altezza(2), profondita(1), orientamento(1), contattol(tavolo), contattoh(aria), shape(parallelepipedo), composto_da([b1])).
blocco(b2, x(1), y(1), z(0), larghezza(1), altezza(2), profondita(1), orientamento(3), contattol(tavolo), contattoh(aria), shape(parallelepipedo), composto_da([b2])).
blocco(b3, x(2), y(0), z(0), larghezza(1), altezza(2), profondita(1), orientamento(1), contattol(tavolo), contattoh(aria), shape(parallelepipedo), composto_da([b3])).
blocco(b4, x(0), y(3), z(0), larghezza(1), altezza(2), profondita(1), orientamento(1), contattol(tavolo), contattoh(aria), shape(parallelepipedo), composto_da([b4])).
blocco(b5, x(0), y(0), z(0), larghezza(4), altezza(1), profondita(4), orientamento(1), contattol(tavolo), contattoh(aria), shape(parallelepipedo), composto_da([b5])).

count(p,1).

% Regole
% print blocco
print_blocco(Blocco) :-
    blocco(Blocco, x(X), y(Y), z(Z), larghezza(L), altezza(H), profondita(P), orientamento(O), contattol(CL), contattoh(CH), shape(S), composto_da(C)),
    format('Blocco ~w: ~n', [Blocco]),
    format('x: ~2f y: ~2f z: ~2f ~n', [X, Y, Z]),
    format('Larghezza: ~2f Altezza: ~2f Profondità: ~2f ~n', [L, H, P]),
    format('Orientamento: ~d ~n', [O]),
    format('Contatto Basso: ~w Contatto Alto: ~w ~n', [CL, CH]),
    format('Shape: ~w ~n', [S]).

ruota_blocco(Blocco, OrientamentoDesiderato, NewOrientamento) :-
    writeln('-----Ruotare Blocco-----'),
    format('Blocco ~w deve essere ruotato con orientamento ~d ~n', [Blocco,OrientamentoDesiderato]),
    NewOrientamento is OrientamentoDesiderato.

muovi_blocco(Blocco, X, Y, Z, NX, NY, NZ) :-
    blocco(Blocco, x(X1), y(Y1), z(Z1), larghezza(L), altezza(H), profondita(P), orientamento(O), contattol(CL), contattoh(CH), shape(S), composto_da(C)),
    writeln('-----Spostare blocco...-----'),
    format('Blocco ~w si trovava in x:~2f y:~2f z:~2f ~n',[Blocco, X1, Y1, Z1]),
    format('Bisogna muoverlo in in x:~2f y:~2f z:~2f ~n', [X, Y, Z]),
    NX is X,
    NY is Y,
    NZ is Z.
    

all_diff(L) :-
    \+ (select(X,L,R), memberchk(X,R)).


% B1 sotto B2 sopra
pilastro(B1, B2) :-
    % blocchi
    blocco(B1, x(X1), y(Y1), z(Z1), larghezza(L1), altezza(H1), profondita(P1), orientamento(O1), contattol(CL1), contattoh(CH1), shape(S1), composto_da(C1)),
    blocco(B2, x(X2), y(Y2), z(Z2), larghezza(L2), altezza(H2), profondita(P2), orientamento(O2), contattol(CL2), contattoh(CH2), shape(S2), composto_da(C2)),
    count(p, N),
    % PRECONDIZIONI
    % controllo che i blocchi siano diversi
    all_diff([B1, B2]),
    % controllo compatibilità dimensione blocchi
    L1 = L2,
    P1 = P2,
    % controllo che i blocchi siano orientati correttamente
    % orientamento non è 1, allora ruota blocco
    ((\+ O1 = 1) -> ruota_blocco(B1, 1, NO1); NO1 = 1),
    ((\+ O2 = 1) -> ruota_blocco(B2, 1, NO2); NO2 = 1),
    CL2 = 'tavolo',
    CH1 = 'aria',
    S1 = 'parallelepipedo',
    S2 = 'parallelepipedo',
    % controllo che i blocchi siano in posizione corretta
    % POSTCONDIZIONI
    ((X1 \= X2; Y1 \= Y2) -> muovi_blocco(B2, X1, Y1, (Z1+H1), NX2, NY2, NZ2);muovi_blocco(B2, X2, Y2, Z1+H1), NX2, NY2, NZ2),
    AltezzaP is H1 + H2,
    writeln('-----Creazione pilastro...-----'),
    format('Il blocco ~w si trova in x:~2f y:~2f z:~2f ~n', [B1, X1, Y1, Z1]),
    format('Il blocco ~w si trova in x:~2f y:~2f z:~2f ~n', [B2, X1, Y1, H1]),
    format('Il pilastro si trova in x:~2f y:~2f ed è alto ~2f ~n', [X1, Y1, AltezzaP]),
    % AGGIORNAMENTO KNOWLEDGE BASE
    retract(blocco(B1, x(X1), y(Y1), z(Z1), larghezza(L1), altezza(H1), profondita(P1), orientamento(O1), contattol(CL1), contattoh(CH1), shape(S1), composto_da(C1))),
    retract(blocco(B2, x(X2), y(Y2), z(Z2), larghezza(L2), altezza(H2), profondita(P2), orientamento(O2), contattol(CL2), contattoh(CH2), shape(S2), composto_da(C2))), 
    assertz(blocco(B1, x(X1), y(Y1), z(Z1), larghezza(L1), altezza(H1), profondita(P1), orientamento(NO1), contattol(CL1), contattoh(B2), shape(S1), composto_da(C1))),
    assertz(blocco(B2, x(NX2), y(NY2), z(NZ2), larghezza(L2), altezza(H2), profondita(P2), orientamento(NO2), contattol(B1), contattoh(CH2), shape(S2), composto_da(C2))),
    string_concat('P', N, PIL),
    retract(count(p, N)),
    N1 is N+1,
    assertz(count(p, N1)),
    assertz(blocco(PIL, x(X1), y(Y1), z(Z1), larghezza(L1), altezza(AltezzaP), profondita(P1), orientamento(1), contattol(CL1), contattoh(CH2), shape('parallelepipedo'), composto_da([B1, B2]))).
    

porta(B1, B2, B3) :-
    % blocchi
    blocco(B1, x(X1), y(Y1), z(Z1), larghezza(L1), altezza(H1), profondita(P1), orientamento(O1), contattol(CL1), contattoh(CH1), shape(S1), composto_da(C1)),
    blocco(B2, x(X2), y(Y2), z(Z2), larghezza(L2), altezza(H2), profondita(P2), orientamento(O2), contattol(CL2), contattoh(CH2), shape(S2), composto_da(C2)),
    blocco(B3, x(X3), y(Y3), z(Z3), larghezza(L3), altezza(H3), profondita(P3), orientamento(O3), contattol(CL3), contattoh(CH3), shape(S3), composto_da(C3)),
    % controllo che i blocchi siano diversi
    all_diff([B1, B2, B3]),
    % controllo compatibilità dimensione blocchi
    L1 = L2,
    P1 = P2,
    H1 = H2,
    L3 >=3,
    % controllo che i blocchi siano impilabili
    ((\+ O1 = 1) -> ruota_blocco(B1, 1, NO1); NO1 = 1),
    ((\+ O2 = 1) -> ruota_blocco(B2, 1, NO2); NO2 = 1),
    ((\+ O3 = 1) -> ruota_blocco(B3, 1, NO3); NO3 = 1),
    CH1 = 'aria',
    CH2 = 'aria',
    CL3 = 'tavolo',
    % controllo che i blocchi siano in posizione corretta
    ((X2 \= (X1+L3-1); Y2 \= Y1 ) -> muovi_blocco(B2, X1+L3-1, Y1, Z1, NX2, NY2, NZ2)),
    ((X3 \= (X1+(L3/2)-1); Y3\=Y1) -> muovi_blocco(B3, X1+(L3/2)-1, Y1, H1+1, NX3, NY3, NZ3)),
    writeln('-----Creazione porta...-----'),
    format('Il blocco ~w si trova in x:~2f y:~2f z:~2f ~n', [B1, X1, Y1, Z1]),
    format('Il blocco ~w si trova in x:~2f y:~2f z:~2f ~n', [B2, X1+L3-1, Y1, Z2]),
    format('Il blocco ~w si trova in x:~2f y:~2f z:~2f ~n', [B3, X1+(L3/2)-1, Y1, H1+1]),
    format('La porta si trova in x:~2f y:~2f z:~2f ~n', [X1+(L3/2)-1, Y1, Z1]),
    format('La porta è alta ~2f ~n', [H1]),
    % AGGIORNAMENTO KNOWLEDGE BASE
    retract(blocco(B1, x(X1), y(Y1), z(Z1), larghezza(L1), altezza(H1), profondita(P1), orientamento(O1), contattol(CL1), contattoh(CH1), shape(S1), composto_da(C1))),
    retract(blocco(B2, x(X2), y(Y2), z(Z2), larghezza(L2), altezza(H2), profondita(P2), orientamento(O2), contattol(CL2), contattoh(CH2), shape(S2), composto_da(C2))),
    retract(blocco(B3, x(X3), y(Y3), z(Z3), larghezza(L3), altezza(H3), profondita(P3), orientamento(O3), contattol(CL3), contattoh(CH3), shape(S3), composto_da(C3))),
    assertz(blocco(B1, x(X1), y(Y1), z(Z1), larghezza(L1), altezza(H1), profondita(P1), orientamento(NO1), contattol(CL1), contattoh(B3), shape(S1), composto_da(C1))),
    assertz(blocco(B2, x(NX2), y(NY2), z(NZ2), larghezza(L2), altezza(H2), profondita(P2), orientamento(NO2), contattol(CL1), contattoh(B3), shape(S2), composto_da(C2))),
    assertz(blocco(B3, x(NX3), y(NY3), z(NZ3), larghezza(L3), altezza(H3), profondita(P3), orientamento(NO3), contattol([B1,B2]), contattoh(CH3), shape(S3), composto_da(C3))).


tavolo(B1, B2, B3, B4, B5) :-
    % blocchi
    blocco(B1, x(X1), y(Y1), z(Z1), larghezza(L1), altezza(H1), profondita(P1), orientamento(O1), contattol(CL1), contattoh(CH1), shape(S1), composto_da(C1)),
    blocco(B2, x(X2), y(Y2), z(Z2), larghezza(L2), altezza(H2), profondita(P2), orientamento(O2), contattol(CL2), contattoh(CH2), shape(S2), composto_da(C2)),
    blocco(B3, x(X3), y(Y3), z(Z3), larghezza(L3), altezza(H3), profondita(P3), orientamento(O3), contattol(CL3), contattoh(CH3), shape(S3), composto_da(C3)),
    blocco(B4, x(X4), y(Y4), z(Z4), larghezza(L4), altezza(H4), profondita(P4), orientamento(O4), contattol(CL4), contattoh(CH4), shape(S4), composto_da(C4)),
    blocco(B5, x(X5), y(Y5), z(Z5), larghezza(L5), altezza(H5), profondita(P5), orientamento(O5), contattol(CL5), contattoh(CH5), shape(S5), composto_da(C5)),
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
    ((\+ O1 = 1) -> ruota_blocco(B1, 1, NO1); NO1 = 1),
    ((\+ O2 = 1) -> ruota_blocco(B2, 1, NO2); NO2 = 1),
    ((\+ O3 = 1) -> ruota_blocco(B3, 1, NO3); NO3 = 1),
    ((\+ O4 = 1) -> ruota_blocco(B4, 1, NO4); NO4 = 1),
    ((\+ O5 = 1) -> ruota_blocco(B5, 1, NO5); NO5 = 1),
    CH1 = 'aria',
    CH2 = 'aria',
    CH3 = 'aria',
    CH4 = 'aria',
    CL5 = 'tavolo',
    % controllo che i blocchi siano in posizione corretta
    ((X2 \= (X1+L5-1); Y2 \= Y1) -> muovi_blocco(B2, X1+L5-1, Y1, Z1, NX2, NY2, NZ2)),
    ((X3 \= X1; Y3 \= (Y1+P5-1)) -> muovi_blocco(B3, X1, Y1+P5-1, Z1, NX3, NY3, NZ3)),
    ((X4 \= (X1+L5-1); Y4 \= (Y1+P5-1)) -> muovi_blocco(B4, X1+L5-1, Y1+P5-1, Z1, NX4, NY4, NZ4)),
    ((X5 \= (X1+(L5/2)-1); Y5 \= (Y1+(P5/2)-1)) -> muovi_blocco(B5, X1+(L5/2)-1, Y1+(P5/2)-1, H1+1, NX5, NY5, NZ5)),
    writeln('-----Creazione tavolo...-----'),
    format('Il blocco ~w si trova in x:~2f y:~2f z:~2f ~n', [B1, X1, Y1, Z1]),
    format('Il blocco ~w si trova in x:~2f y:~2f z:~2f ~n', [B2, NX2, NY2, NZ2]),
    format('Il blocco ~w si trova in x:~2f y:~2f z:~2f ~n', [B3, NX3, NY3, NZ3]),
    format('Il blocco ~w si trova in x:~2f y:~2f z:~2f ~n', [B4, NX4, NY4, NZ4]),
    format('Il blocco ~w si trova in x:~2f y:~2f z:~2f ~n', [B5, NX5, NY5, NZ5]),
    format('Il tavolo si trova in x:~2f y:~2f z:~2f ~n', [X1+(L5/2)-1, Y1+(P5/2)-1, Z1]),
    format('Il tavolo è alto ~2f ~n', [H1+H5]),
    % AGGIORNAMENTO KNOWLEDGE BASE
    retract(blocco(B5, x(X5), y(Y5), z(Z5), larghezza(L5), altezza(H5), profondita(P5), orientamento(O5), contattol(CL5), contattoh(CH5), shape(S5), composto_da(C1))),
    retract(blocco(B4, x(X4), y(Y4), z(Z4), larghezza(L4), altezza(H4), profondita(P4), orientamento(O4), contattol(CL4), contattoh(CH4), shape(S4), composto_da(C2))),
    retract(blocco(B3, x(X3), y(Y3), z(Z3), larghezza(L3), altezza(H3), profondita(P3), orientamento(O3), contattol(CL3), contattoh(CH3), shape(S3), composto_da(C3))),
    retract(blocco(B2, x(X2), y(Y2), z(Z2), larghezza(L2), altezza(H2), profondita(P2), orientamento(O2), contattol(CL2), contattoh(CH2), shape(S2), composto_da(C4))),
    retract(blocco(B1, x(X1), y(Y1), z(Z1), larghezza(L1), altezza(H1), profondita(P1), orientamento(O1), contattol(CL1), contattoh(CH1), shape(S1), composto_da(C5))),
    assert(blocco(B5, x(NX5), y(NY5), z(NZ5), larghezza(L5), altezza(H5), profondita(P5), orientamento(NO5), contattol([B1,B2,B3,B4]), contattoh(CH5), shape(S5), composto_da(C1))),
    assert(blocco(B4, x(NX4), y(NY4), z(NZ4), larghezza(L4), altezza(H4), profondita(P4), orientamento(NO4), contattol(CL4), contattoh(B5), shape(S4), composto_da(C2))),
    assert(blocco(B3, x(NX3), y(NY3), z(NZ3), larghezza(L3), altezza(H3), profondita(P3), orientamento(NO3), contattol(CL3), contattoh(B5), shape(S3), composto_da(C3))),
    assert(blocco(B2, x(NX2), y(NY2), z(NZ2), larghezza(L2), altezza(H2), profondita(P2), orientamento(NO2), contattol(CL2), contattoh(B5), shape(S2), composto_da(C4))),
    assert(blocco(B1, x(X1), y(Y1), z(Z1), larghezza(L1), altezza(H1), profondita(P1), orientamento(NO1), contattol(CL1), contattoh(B5), shape(S1), composto_da(C5))).

