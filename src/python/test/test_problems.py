from scoop import Scoop
import cvxopt

n = 5
TOL = 1e-6

problems = [
    (["variable x vector", "minimize sum(x)", "abs(x) <= 3"], -15, {'x': n}),
    (["variable x vector", "parameter c vector", "parameter A matrix", "parameter b vector",
      "minimize c'*x", "A*x <= b", "x >= 0"], -0.166666666666666666, 
      {'x': n, 'c': cvxopt.matrix([1,-1,1,-1,1],(n,1),'d'), 
       'A': cvxopt.matrix([1,2,3,-4,5,6,7,8,9,10,3.5,12,13,14,15], (3,n), 'd'),
       'b': cvxopt.matrix([1,1,1], (3,1), 'd')})
    
]

def check_solved(prob, exp, data):
    p = Scoop()
    map(p.run, prob)
    print p
    # assert False
    f = p.generate()
    x = f(**data)
    print x['status']
    print x['primal objective']
    print x['x']
    assert(x['status'] == 'optimal')
    assert(abs(x['primal objective'] - exp) <= TOL)

def test_problems():
    for p, exp, data in problems:
        yield check_solved, p, exp, data
    