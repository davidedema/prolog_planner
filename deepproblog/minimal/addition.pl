nn(mnist_net,[X],Y,[0,1,2,3,4,5,6,7,8,9]) :: digit(X,Y).

addition(X,Y,Z) :- digit(X,X2), digit(Y,Y2), Z is X2+Y2.

% riga 1 -> Neural annotated disjunction
% nn è il funtore (collega la rete neurale alla logica), mnist_net identifica il modello di rete neurale
% [X] è la lista di input, Y è l'output, [0,1,2,3,4,5,6,7,8,9] è il dominio di Y (classi di output)
% digit(X,Y) è il predicato che associa ad ogni cifra il suo valore numerico

% addition(X,Y,Z) è la regola che calcola la somma di due cifre