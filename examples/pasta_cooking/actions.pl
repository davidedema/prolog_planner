% This action allows the agent to open a compartment. The preconditions check:
% - The arm is free;
% - The compartment exists;
% - The compartment is not opened;
% - The compartment is openable;
% The effects are:
% - The compartment is opened.
action(
    openDrawer(Arm, Cmprtmnt),
    [handempty(Arm)],
    [opened(Cmprtmnt)],
    [],
    [loc(Cmprtmnt), openable(Cmprtmnt)],
    [
        add(opened(Cmprtmnt))
    ]
)

% This pickup action is for tools that are positioned inside compartments. The
% preconditions check that:
% - The tool exists;
% - The arm is free;
% - The tool is in a compartment and that the compartment is opened. 
% Also, it checks that the object is not already been held nor that the arm is 
% already holding something.
% The effects are:
% - The arm is not free anymore;
% - The tool is not in the compartment anymore;
% - The compartment is not opened anymore;
% - The arm is holding the tool.
action(
    pickup(Arm, tool(Object)),
    [handempty(Arm), at(tool(Object), Cmprtmnt), opened(Cmprtmnt)],
    [holding(_, Object), holding(Arm, _)],
    [],
    [tool(Object)],
    [
        del(handempty(Arm)), del(at(tool(Object), Cmprtmnt)), del(opened(Cmprtmnt)),
        add(holding(Arm, Object))
    ]
).

% This pickup action is for tools that are positioned in reachable places. The
% preconditions check that:
% - The tool exists;
% - The arm is free;
% - The compartment is reachable.
% Also, it checks that the object is not already been held nor that the arm is 
% already holding something.
% The effects are:
% - The arm is not free anymore;
% - The tool is not in the compartment anymore;
% - The arm is holding the tool.
action(
    pickup(Arm, tool(Object)),
    [handempty(Arm), at(tool(Object), Cmprtmnt)],
    [holding(_, Object), holding(Arm, _)],
    [],
    [tool(Object), reachable(Cmprtmnt)],
    [
        del(handempty(Arm)), del(at(tool(Object), Cmprtmnt)),
        add(holding(Arm, Object))
    ]
).


% Fill the pot with water. The preconditions check that:
% - The arm is holding the pot;
% - The pot is not filled;
% As effects, the pot is filled.
action(
    fill_pot(Arm),
    [holding(Arm, tool(pot))],
    [filled(tool(pot))],
    [],
    [],
    [
        add(filled(tool(pot)))
    ]
).

% This action allows the agent to release an object. The preconditions check:
% - The arm is holding the object;
% The effects are:
% - The arm is free;
% - The object is not being held anymore;
action(
    release(Arm, tool(Tool), loc(Position)),
    [holding(Arm, tool(Tool))],
    [],
    [],
    [tool(Tool), loc(Position)],
    [
        del(holding(Arm, tool(Tool))),
        add(handempty(Arm)), add(tool(Tool), at(Position))
    ]
)

% This action allows the agent to place some ingredients in a pot, pan or cup. 
% The preconditions check:
% - The arm is holding the ingredient;
% - The pot or the pan is on the stove;
% The effects are:
% - The arm is free;
% - The ingredient is placed in the tool;
action(
    place(Arm, ing(Ing), tool(Tool)),
    [holding(Arm, ing(Ing)), at(tool(Tool), loc(stove))],
    [],
    [],
    [tool(Tool), hallow(Tool)],
    [
        del(holding(Arm, ing(Ing))),
        add(placed(ing(Ing), tool(Tool)))
    ]
)

% This action allows the agent to place a pot or a pan on the stove. The
action(
    
)

% action(
%     pour(Arm, Object),
%     [holding(Arm, Object)],
%     []
%     []
%     []
%     [
%         del(holding(Arm, Object)), 
%         add(poured(Object))
%     ]
% )

% action(
%     pour(Arm, Object),
%     [holding(Arm, Object)],
%     []
%     []
%     []
%     [
%         del(holding(Arm, Object))
%     ]
% )