# Simple task planner for pillar creation

## Install
1) Per provare il progetto assicurarsi di avere [SWI Prolog](https://www.swi-prolog.org/build/PPA.html). L'installazione per ubuntu è la seguente :
``` BASH
sudo apt-add-repository ppa:swi-prolog/stable
sudo apt update
sudo apt install swi-prolog
```
2) Clonare il progetto nel posto desiderato
```BASH
git clone https://github.com/davidedema/progetto_tesi.git
```
3) Caricare il file con l'interprete swipl
```BASH
cd ~/progetto_tesi
swipl block_world.pl
```

## Run

Per generare un pilastro usare la regola: `pillar/6`, questa terrà prenderà in 'input' 6 parametri
- x: cordinata x in cui si vuole creare il pilastro
- y: cordinata x in cui si vuole creare il pilastro
- z: cordinata x in cui si vuole creare il pilastro
- High: Altezza del pilastro
- Width: Larghezza del pilastro
- Depth: Profondità del pilastro

Ad esempio per creare il pilastro di altezza **5** alle coordinate (1,1,0).
```
pillar(1,1,0,5,1,1).
```
**N.B.:** In prolog ogni istruzione termina con il punto '.'. Lanciata questa l'istruzione pilastro verrà mostrata la sequenza di azioni che dovranno essere fatte per creare il pilastro seguite da un **true**, premere invio per continuare.


Il pilastro appena creato sarà visibile tramite il comando `listing(block/13)` il quale mostra tutti i predicati block/13.

Un possibile output di questo comando sarà il seguente:
```SWIPL
?- listing(block/13).
:- dynamic block/13.

block(b3, 2, 0, 0, 1, 2, 1, 1, table, air, block, [b3], 0).
block(b4, 0, 3, 0, 1, 2, 1, 1, table, air, block, [b4], 0).
block(b6, 1, 1, 0, 1, 2, 1, 1, table, air, block, [b6], 0).
block(b7, 1, 3, 1, 1, 2, 1, 3, table, air, block, [b7], 0).
block(b8, 2, 2, 0, 1, 2, 1, 1, table, air, block, [b8], 0).
block(b9, 1, 3, 0, 1, 2, 1, 1, table, air, block, [b9], 0).
block(b10, 1, 4, 0, 1, 2, 1, 1, table, air, block, [b10], 0).
block(b1, 1, 1, 2, 1, 2, 1, 1, b2, air, block, [b1], 1).
block(b2, 1, 1, 0, 1, 2, 1, 1, table, b1, block, [b2], 1).
block(b5, 1, 1, 4, 1, 1, 1, 1, s1, air, block, [b5], 1).
block(s1, 1, 1, 0, 1, 4, 1, 1, table, b5, block, [b1, b2], 1).
block(s2, 1, 1, 0, 1, 5, 1, 1, table, air, block, [b5, s1], 0).
```

