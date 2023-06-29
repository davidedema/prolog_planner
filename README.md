<p align='center'>
    <h1 align="center">Prolog planner for UR5 pick and place</h1>
    <p align="center">
    Project for my bachelor degree at the University of Trento A.Y.2022/2023
    </p>
    <p align='center'>
    Developed by:<br>
    De Martini Davide <br>
    </p>   
</p>

## Project Description
The goal of this project was to develop a Prolog Task planner for performing pick and place tasks. The main task is to build a tower with a "user defined" height. At first a prolog program `block_world.pl` was created. Then it was wrapped inside a ROS node for communicating to the robot. 

## Project Structure
![](img/p_structure.png)

The main folder is:
- `prolog_project` it contains the ROS node (motion node and **planner** node)
    - scripts: Contains the two node plus the utilities
    - msg: Contains the `.msg` file for ROS communication

`block_world.pl` is the prolog file, the core of this project.

`python_node_poc.py` is a simple proof of concept for the pyswip wrapper for prolog

## Requirements

### PROLOG ONLY

### PROLOG + ROS SIMULATION

## Installation

I reccomend to use Ubuntu 20.04 (I used it for developing the project) 

### PROLOG ONLY
1) For testing the *prolog only* version at first install the prolog interpreter [SWI Prolog](https://www.swi-prolog.org/build/PPA.html). Installation is the following:
    ```BASH
    sudo apt-add-repository ppa:swi-prolog/stable
    sudo apt update
    sudo apt install swi-prolog
    ```
2) Clone the project wherever you want
    ```BASH
    git clone https://github.com/davidedema/prolog_planner.git
    ```
3) Load the file with the swipl interpeter
    ```BASH
    cd ~/prolog_planner
    swipl block_world.pl
    ```
### PROLOG + ROS SIMULATION
1) Follow the prolog installation (only step 1 and 2)
2) Install [**Pyswip**](https://github.com/yuce/pyswip)
    ```BASH
    pip3 install git+https://github.com/yuce/pyswip@master#egg=pyswip
    ```
3) Follow the [locosim](https://github.com/mfocchi/locosim) repository for installing ROS and the UR5 simulation

## Running

### PROLOG ONLY
In order to create a pillar use the `pillar/7` rule. This needs 7 parameters in input:
- x: x coord for the pillar generation
- y: y coord for the pillar generation
- z: z coord for the pillar generation
- High: Pillar High
- Width: Pillar width
- Depth: Pillar depth
- Actions: Our "output" variable
It will return in the output variable the plan

### PROLOG + ROS SIMULATION

## Known issues and future works

## Credits

## Run

### PROLOG VERSION
Per generare un pilastro usare la regola: `pillar/7`, questa terrà prenderà in 'input' 6 parametri
- x: cordinata x in cui si vuole creare il pilastro
- y: cordinata x in cui si vuole creare il pilastro
- z: cordinata x in cui si vuole creare il pilastro
- High: Altezza del pilastro
- Width: Larghezza del pilastro
- Depth: Profondità del pilastro

E restituirà in output la lista di azioni actions che conterrà in piano da seguire per creare il pilastro.

Ad esempio per creare il pilastro di altezza **5** alle coordinate (1,1,0).
```
pillar(1,1,0,5,1,1,Actions).
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

