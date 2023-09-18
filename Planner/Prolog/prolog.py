from pyswip import Prolog, Functor, Variable, Query

prolog = Prolog()
prolog.consult("tests.pl")

def execTest(test):
    test = Functor(test, 2)
    a_var = Variable()
    t_var = Variable()

    sol = Query(test(a_var, t_var))

    sol.nextSolution()
    actions = list(reversed(list(a_var.get_value())))
    times = list(reversed(list(t_var.get_value())))

    return actions, times
