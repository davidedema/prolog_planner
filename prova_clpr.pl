:- use_module(library(clpfd)).
/* risolvi :-
{
    Ta>=0,
    Ta+2=<Tb,
    Ta+2=<Tc,
    Tb+3=<Td,
    Tc+3=<Te,
    Tb+3=<Td,
    Tc+5=<Tf,
    Td+4=<Tf
},
minimize(Tf). */
/* 
task(T, Dur, Start, End) :-
    Dur #> 0,
    Start #>= 0,
    End #>= 0,
    End #= Start + Dur.

non_sovrapposte(T1, T2) :-
    disjoint2([Start1-Dur1, Start2-Dur2], [Start1, Dur1, Start2, Dur2]),
    % cosa fa disjoint2?
    % 1) crea una variabile Start1-Dur1 che rappresenta l'intervallo di tempo in cui si svolge il task T1
    % 2) crea una variabile Start2-Dur2 che rappresenta l'intervallo di tempo in cui si svolge il task T2
    % 3) crea una variabile Start1 che rappresenta l'inizio del task T1
    % 4) crea una variabile Dur1 che rappresenta la durata del task T1
    % 5) crea una variabile Start2 che rappresenta l'inizio del task T2
    % 6) crea una variabile Dur2 che rappresenta la durata del task T2
    % 7) impone che Start1-Dur1 e Start2-Dur2 siano disgiunti
    task(T1, Dur1, Start1, _),
    task(T2, Dur2, Start2, _).

task(a, 5, AStart, AEnd).
task(b, 3, BStart, BEnd).
task(c, 2, CStart, CEnd).

non_sovrapposte(a, b).
non_sovrapposte(a, c). */

solve(X,Y) :-
    X in 0..10,
    Y in 0..10,
    X+Y #= 5, 
    X#>Y,
    label([X,Y]).