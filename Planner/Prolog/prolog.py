from pyswip import Prolog, Functor, Variable, Query
import os

def execTest(test):
    print("Executing prolog")
    
    prolog = Prolog()
    prolog.consult(os.path.join(os.getcwd(), "tests.pl"))

    test = Functor(test, 2)
    a_var = Variable()
    t_var = Variable()

    sol = Query(test(a_var, t_var))

    sol.nextSolution()
    
    print("Executed prolog")
    
    actions = list(reversed(list(a_var.get_value())))
    times = list(reversed(list(t_var.get_value())))

    return actions, times
