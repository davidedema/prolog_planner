testCase(Actions, Times) :- 
    go (
        [   
            handempty(a1), handempty(a2),
            ing(pasta), ing(egg), ing(salt), ing(guanciale), ing(pecorino), 
            at(ing(pasta), loc(counter)), at(ing(salt), loc(counter)),
            at(ing(egg), loc(fridge)), at(ing(guanciale), loc(fridge)),
            at(tool(pot), loc(drawerBig)), at(tool(pan), loc(drawerBig)),
            at(tool(knife), loc(drawer)), at(tool(fork), loc(drawer)),
            at(tool(spoon), loc(drawer)), at(tool(spatula), loc(drawer)),
            at(tool(cup), loc(drawer)) 
        ],
        [
            handempty(a1), handempty(a2),
            cooked(pasta), poured(egg), poured(salt), cooked(guanciale), 
            poured(pecorino), boiled(water)
        ],
        Actions, 
        Times
    ).