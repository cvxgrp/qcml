from scoop import Scoop
import cvxopt

n = 5
TOL = 1e-6

problems = [
    (["variable x vector", "minimize sum(x)", "abs(x) <= 3"], -15, {'x': n}),
    (["variable x vector", "parameter c vector", "parameter A matrix", "parameter b vector",
      "minimize c'*x", "A*x <= b", "x >= 0"], -0.166666666666666666, 
      {'x': 5, 'c': cvxopt.matrix([1,-1,1,-1,1],(n,1),'d'), 
       'A': cvxopt.matrix([1,2,3,-4,5,6,7,8,9,10,3.5,12,13,14,15], (3,n), 'd'),
       'b': cvxopt.matrix([1,1,1], (3,1), 'd')}),
    (["variable x vector", "parameter mu vector", "parameter gamma positive", "parameter F matrix", "parameter D matrix",
      "maximize mu'*x - gamma*(square(norm(F'*x)) + square(norm(D*x)))",
      "sum(x) == 1", "x >= 0"],4.07012330846,
      {'x': 10, 'mu': cvxopt.matrix([0.3,0.1,0.5,0.1,0.2,0.7,0.8,0.9,0.25,0.05],(10,1),'d'),
       'gamma': 0.1, 'F': cvxopt.matrix([1,9,5,2,3,1,0,2,8,7,7,4,5,10,8,4,10,2,5,5,5,4,5,8,2,4,0,3,8,7,9,1,5,4,3,9,2,4,10,1,3,1,1,2,1,5,8,4,8,8], (10,5),'d'),
       'D': cvxopt.spdiag([0.1,0.2,0.3,0.4,0.5,0.2,0.8,0.9,0.7,1])})
    
]

#TODO: make sure maximization problem returns "- obj val"
def checker(func, *args):

    def check_solved(prob, exp, data):
        p = Scoop()
        map(p.run, prob)
        print p
        # assert False
        f = getattr(p, func)(*args)
        x = f(**data)
        print x['status']
        print x['primal objective']
        print x['x']
        assert(x['status'] == 'optimal')
        assert(abs(x['primal objective'] - exp) <= TOL)
    return check_solved

def test_problems():
    for p, exp, data in problems:
        yield checker("generate"), p, exp, data
    
    for p, exp, data in problems:
        yield checker("generate_fixed_soc", 3), p, exp, data
    