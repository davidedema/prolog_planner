:- discontiguous tool/1.
:- discontiguous loc/1.
:- discontiguous openable/1.
:- discontiguous reachable/1.
:- discontiguous hallow/1.

tool(pot).
tool(pan).
tool(cup).
tool(knife).
tool(spoon).
tool(fork).
tool(spatula).

hallow(pot).
hallow(pan).
hallow(cup).

loc(stove).
loc(drawer).
loc(drawerBig).
loc(counter).
loc(fridge).

openable(drawer).
openable(drawerBig).
openable(fridge).

reachable(stove).
reachable(counter).