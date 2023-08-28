grip -> F -> [on]
grip -> S -> gripped(a1, a), available(a2), available(a3), ontable(b), clear(b)
grip -> F -> [on]
grip -> S -> gripped(a2, b), gripped(a1, a), available(a3)
grip -> F -> [clear, on]
grip -> F -> [clear, ontable]
move_block -> S -> moving(a2, b), gripped(a1, a), available(a3)
grip -> F -> [clear, on]
grip -> F -> [clear, ontable]
move_block -> S -> moving(a1, a), moving(a2, b), available(a3)
grip -> F -> [clear, on]
grip -> F -> [clear, ontable]
move_block -> F -> [gripped]
Traceback (most recent call last):
  File "/home/enrico/Projects/prolog_planner/parser.py", line 85, in <module>
    main()
  File "/home/enrico/Projects/prolog_planner/parser.py", line 81, in main
    parseChunk(chunk)
  File "/home/enrico/Projects/prolog_planner/parser.py", line 31, in parseChunk
    raise Exception("Could not find newState:\n", chunk)
Exception: ('Could not find newState:\n', '   Exit: (11) move(stack(_52100, _52102, _52104), [moving(_52100, _52102), clear(_52104), inposition(_52104)], [notavalidpredicate(_52100)], [del(moving(_52100, _52102)), add(available(_52100)), del(clear(_52104)), add(on(_52102, _52104)), add(clear(_52102)), add(inposition(_52102)), del(inposition(...))])\n   Call: (11) conditions_met([moving(_52100, _52102), clear(_52104), inposition(_52104)], [moving(a1, a), moving(a2, b), available(a3)])\n   Fail: (11) conditions_met([moving(_662, _664), clear(_666), inposition(_666)], [moving(a1, a), moving(a2, b), available(a3)])\n   Redo: (11) move(_652, _654, _656, _658)\n')
