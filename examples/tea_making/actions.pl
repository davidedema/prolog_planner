%  /# of parameters for given predicate
:- dynamic at/2.
:- dynamic empty/1.
:- dynamic full/1.
:- dynamic boiled/1.
:- dynamic holding/1.
:- dynamic available/1.
:- dynamic tea/1.
:- discontiguous action/6.

%===========================================================%
%                           Actions                         %
%===========================================================%
% Move the robot to the destionation room  
action(goto_place(Destination), 
    [at(robot, Place)], 
    [at(robot, Destination)], 
    [], 
    [],
    [del(at(robot, Place)), add(at(robot, Destination))]).


% The robot picks up an object  
action(pickup(Object),
    [available(robot), at(robot, Place), at(Object, Place)], 
    [holding(_)], 
    [], 
    [],
    [del(available(robot)), del(at(Object, Place)), add(holding(Object))]).


% The robot puts down an object  
action(putdown(Object), 
    [holding(Object)], 
    [], 
    [], 
    [],
    [del(holding(Object)), add(available(robot)), add(at(Object, kitchen))]).


% The robot puts water in a kettle
action(fill_kettle, 
    [at(robot, kitchen), holding(kettle), empty(kettle)], 
    [], 
    [], 
    [],
    [del(empty(kettle)), add(full(kettle))]).


% Boil water in kettle
action(boil_water, 
    [at(robot, kitchen), holding(kettle), full(kettle)], 
    [], 
    [],
    [], 
    [add(boiled(water))]).


% The robot puts water in a cup from a kettle
action(pour_water, 
    [at(robot, kitchen), holding(kettle), full(kettle), boiled(water), empty(cup), at(cup, kitchen)], 
    [],
    [], 
    [],
    [del(empty(cup)), del(full(kettle)), add(full(cup))]).


% The robot puts a teabag in a cup
action(add_teabag, 
    [at(robot, kitchen), holding(teabag), at(cup, kitchen), boiled(water), full(cup), tea(notready)], 
    [], 
    [], 
    [],
    [del(tea(notready)), add(tea(prepared)), add(available(robot)), del(holding(teabag)), del(boiled(water))]).

