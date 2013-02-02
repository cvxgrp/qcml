###################################################################
# Disciplined Convex Programming                                  #
# Neal Parikh <npparikh@cs.stanford.edu>                          #
###################################################################

###################################################################
###################################################################

NUMBER = (int, float)

###################################################################
###################################################################

class Expression(object):

    def __init__(self, node, arguments):
        self.node = node
        self.arguments = arguments

    # DCP

    def is_convex(self):
        if isinstance(self.node, NUMBER) or isinstance(self, Variable):
            return True
        elif self.node == '+':
            return all(arg.is_convex() for arg in self.arguments)
        elif self.node == '*':
            left, right  = self.arguments
            case1 = left.is_nonneg_const() and right.is_convex()
            case2 = left.is_nonpos_const() and right.is_concave()
            case3 = left.is_constant() and right.is_affine()
            return (case1 or case2 or case3)
        elif isinstance(self.node, Program):
            return self.node.is_dcp(self.arguments)
        else:
            return False

    def is_concave(self):
        if isinstance(self.node, NUMBER) or isinstance(self, Variable):
            return True
        elif self.node == '+':
            return all(arg.is_concave() for arg in self.arguments)
        elif self.node == '*':
            left, right  = self.arguments
            case1 = left.is_nonneg_const() and right.is_concave()
            case2 = left.is_nonpos_const() and right.is_convex()
            case3 = left.is_constant() and right.is_affine()
            return (case1 or case2 or case3)
        elif isinstance(self.node, Program):
            # Currently, all programs are minimization programs, so
            # can never be concave in this context.
            return False
        else:
            return False

    def is_dcp(self):
        return self.is_convex() or self.is_concave()

    def is_affine(self):
        return self.is_convex() and self.is_concave()

    def is_constant(self):
        return isinstance(self.node, NUMBER)

    def is_nonneg_const(self):
        return isinstance(self.node, NUMBER) and self.node >= 0

    def is_nonpos_const(self):
        return isinstance(self.node, NUMBER) and self.node <= 0

    # Should this evaluate or simply substitute?
    def substitute(self, mapping):
        if self.is_constant():
            return self
        elif isinstance(self, Variable):
            return mapping.get(self, self)
        elif isinstance(self.node, Program):
            return Expression(self.node, 
                              [a.substitute(mapping) for a in self.arguments])
        else:
            return Expression(self.node, 
                              [a.substitute(mapping) for a in self.arguments])

    def variables(self):
        if isinstance(self, Variable):
            return [self]
        else:
            variables = []
            for argument in self.arguments:
                variables.extend(argument.variables())
            return list(set(variables))

    # Math operations (return larger Expressions)

    def __add__(self, other):
        return Expression('+', [self, _promote(other)])

    def __radd__(self, other):
        return Expression('+', [_promote(other), self])

    def __sub__(self, other):
        return Expression('+', [self, _promote(-other)])

    def __rsub__(self, other):
        return Expression('+', [_promote(other), -self])

    def __mul__(self, other):
        return Expression('*', [self, _promote(other)])

    def __rmul__(self, other):
        return Expression('*', [_promote(other), self])

    def __neg__(self):
        return (-1.0)*self

    # Comparisons (return Constraints)

    def __le__(self, other):
        return Constraint(self, '<=', _promote(other))

    def __lt__(self, other):
        self.__le__(self, other)

    def __ge__(self, other):
        return _promote(other).__le__(self)

    def __gt__(self, other):
        self.__ge__(self, other)

    def __eq__(self, other):
        return Constraint(self, '==', _promote(other))

    # Miscellaneous

    def __repr__(self):
        s = "Expression(%s, [%s])" % (self.node, ', '.join(str(c) for c in self.arguments))
        return s

    def __str__(self):
        s = "%s(%s)" % (self.node, ', '.join(str(c) for c in self.arguments))
        return s.replace('()', '')

def _promote(x):
    return x if isinstance(x, Expression) else Expression(float(x), []) 

###################################################################
###################################################################

class Variable(Expression):

    counter = 0

    def __init__(self):
        Variable.counter += 1
        self.name  = "s%d" % Variable.counter
        self.value = None
        Expression.__init__(self, self.value, [])

    def __repr__(self):
        return self.name

    def __str__(self):
        return self.name

    def __len__(self):
        return 1

###################################################################
###################################################################

class Matrix(Expression):
    pass

###################################################################
###################################################################

class Vector(Matrix):
    pass

###################################################################
###################################################################

class Constraint(object):

    def __init__(self, lhs, op, rhs):
        self.lhs = lhs
        self.op  = op
        self.rhs = rhs

    def is_dcp(self):
        if self.op == '<=':
            return self.lhs.is_convex() and self.rhs.is_concave()
        elif self.op == '>=':
            return self.lhs.is_concave() and self.rhs.is_convex()
        elif self.op == '==':
            return self.lhs.is_affine() and self.rhs.is_affine()
        elif self.op == 'in':
            return self.lhs.is_affine() and isinstance(self.rhs, ConvexSet)

    def variables(self):
        if self.op == 'in':
            return list(set(self.lhs.variables()))
        else:
            return list(set(self.lhs.variables() + self.rhs.variables()))

    def substitute(self, mapping):
        return Constraint(self.lhs.substitute(mapping), 
                          self.op, 
                          self.rhs.substitute(mapping))

    def __str__(self):
        return "%s %s %s" % (self.lhs, self.op, self.rhs)

###################################################################
###################################################################

class Program(object):

    def __init__(self, objective, constraints=[], arguments=[], name=None):
        self.name = name
        self.arguments = arguments
        self.status = 'unsolved'
        self.objective = objective
        self.constraints = constraints

    def solve(self):
        if not self.is_dcp():
            raise ValueError("Optimization problem is not DCP-compliant.")

        unrolled = unroll(self)
        result = solver(unrolled)
        return result

    def __call__(self, *args):
        if all(isinstance(arg, NUMBER) for arg in args):
            mapping = dict(zip(self.arguments, args))
            p = self.substitute(mapping)
            return p.solve()
        else:
            return Expression(self, args)

    def is_dcp(self, args=None):
        if args is None:
            return (self.objective.is_convex() and 
                    all(c.is_dcp() for c in self.constraints))
        else:
            mapping = dict(zip(self.arguments, args))
            p = self.substitute(mapping)
            return p.is_dcp()

    def substitute(self, mapping):
        objective = self.objective.substitute(mapping)
        constraints = [ c.substitute(mapping) for c in self.constraints ]
        return Program(objective, constraints)

    def epigraph(self):
        objective, constraints = unroll(self.objective)
        for constraint in self.constraints:
            if constraint.op == 'in':
                constraints.append(constraint)
            else:
                constraints.extend(unroll(constraint))
        return Program(objective, constraints, self.arguments, self.name)

    def __str__(self):
        if self.name is not None:
            return self.name
        s = "\nminimize (%s)\n" % self.objective
        for constraint in self.constraints:
            s += "\t%s\n" % constraint
        return s

def solver(problem):

    import cvxopt
    import cvxopt.solvers

    # Formulate and solve the following problem:
    #
    #   minimize    c^T x
    #   subject to  Gx + s = h, Ax = b, s >= 0

    # Should be able to just unroll an entire problem in one shot and
    # call variables(), equalities(), etc on the problem object.
    # But make sure this works first.
    constraints = problem.constraints # unroll(problem.constraints)
    variables = [ c.variables() for c in constraints ]
    variables = list(set(v for vs in variables for v in vs))
    n = len(variables)

    index = dict((v,i) for i, v in enumerate(variables))

    # Objective
    cc = cvxopt.matrix(0.0, (n,1))
    for v in problem.objective.variables():
        cc[index[v]] = 1.0

    # Equality constraints
    equalities = [ c for c in constraints if c.op == '==' ]
    m = len(equalities)
    A = cvxopt.spmatrix(0.0, [], [], (m,n))
    b = cvxopt.matrix(0.0, (m,1))

    for i, constraint in enumerate(equalities):
        lhs, rhs = constraint.lhs, constraint.rhs
        for side, coefficient in ((lhs, 1.0), (rhs, -1.0)):
            if isinstance(side, Variable):
                A[i,index[side]] += coefficient
            elif isinstance(side, Expression):
                if side.is_constant():
                    b[i,0] += coefficient*side.node
                elif side.node == '*':
                    side_variable = side.arguments[1]
                    A[i,index[side_variable]] += coefficient*side.arguments[0].node
                elif side.node == '+':
                    pass

    # Inequality constraints
    inequalities = [ c for c in constraints if c.op in ('<=', '>=') ]
    m = len(inequalities)
    G = cvxopt.spmatrix(0.0, [], [], (m,n))
    h = cvxopt.matrix(0.0, (m,1))

    for i, constraint in enumerate(inequalities):
        lhs, op, rhs = constraint.lhs, constraint.op, constraint.rhs
        for side, direction in ((lhs, '<='), (rhs, '>=')):
            if side.is_constant():
                if op == direction:
                    h[i,0] -= side.node # make this side.value?
                else:
                    h[i,0] += side.node
            else:
                if op == direction:
                    G[i,index[side]] += 1.0
                else:
                    G[i,index[side]] -= 1.0

    print '='*72
    print 'Solving with CVXOPT...'
    print '-'*72
    dims = { 'l': G.size[0], 'q': [], 's': [] }
    result = cvxopt.solvers.conelp(cc, G, h, dims, A, b)

    if result['status'] != 'primal infeasible':
        for v in variables:
            v.value = result['x'][index[v]]

    print ', '.join("%s:%f" % (v.name, v.value) for v in variables)
    print '='*72

    return result

def unroll(obj):

    # Unroll an expression tree with new variables and constraints. Leave
    # individual variables and constants alone; expressions like u+v are
    # unrolled to have a new variable t and constraint t == u+v. Expressions
    # like f(x), where f has a graph implementation, are replaced with an
    # epigraph expansion; a new objective variable t is introduced, then we
    # constrain f <= t, where f is the unrolled graph implementation of f(x).

    if isinstance(obj, Expression):

        if obj.is_constant() or isinstance(obj, Variable):
            return (obj, [])

        elif obj.node == '*':
            # XXX this is inefficient. if this sees a*x <= b, this will introduce
            # a new variable z and give constraints [z == a*x, z <= b]. obviously,
            # can do without the extra variable and constraint. need to update
            # inequality processor in the solver to handle *(a,x) on lhs/rhs
            # correctly and then can simplify this.
            var, constraints = unroll(obj.arguments[1])
            t = Variable()
            f = Expression(obj.node, [obj.arguments[0], var])
            constraints.append(f == t)
            return (t, constraints)

        arguments = []
        constraints = []
        for arg in obj.arguments:
            var, new_constraints = unroll(arg)
            arguments.append(var)
            constraints.extend(new_constraints)
        
        t = Variable()
        f = Expression(obj.node, arguments)
        if obj.node == '+':
            constraints.append(f == t)
        elif isinstance(obj.node, Program):
            constraints.append(f <= t)
        return (t, constraints)

    elif isinstance(obj, Program):
        constraints = []
        for constraint in obj.constraints:
            lhs = constraint.lhs.node
            op  = constraint.op
            rhs = constraint.rhs.node
            if isinstance(lhs, Program) and op == '<=':
                constraints.append(lhs.objective <= constraint.rhs)
                constraints.extend(lhs.constraints)
            elif isinstance(rhs, Program) and op == '>=':
                constraints.append(constraint.lhs >= rhs.objective)
                constraints.extend(rhs.constraints)
            else:
                constraints.append(constraint)
        return Program(obj.objective, constraints)

    elif isinstance(obj, Constraint):
        lo, lc = unroll(obj.lhs)
        ro, rc = unroll(obj.rhs)
        return lc + rc + [Constraint(lo, obj.op, ro)]

    elif isinstance(obj, ConvexSet):
        return obj, []

###################################################################
###################################################################

class ConvexSet(object):

    def __init__(self):
        raise NotImplementedError

    def __str__(self):
        return self.__class__.__name__

class SecondOrderCone(ConvexSet):

    def __init__(self):
        pass

second_order_cone = SecondOrderCone()

###################################################################
###################################################################

# Library functions

def abs(x):
    t = Variable()
    z = Variable()
    f = minimize(t, [z <= t, -z <= t], [z], 'abs')
    return f(x)

def max(x):
    n = len(x)
    if n == 1:
        return x
    else:
        t = Variable()
        z = Variable(n)
        f = minimize(t, [z <= t], [z], 'max')
        return f(x)

# Helper methods

def variable(m=1, n=1):
    if m == 1 and n == 1:
        return Variable()
    elif m == 1 or n == 1:
        return Vector(m if m > n else n)
    else:
        return Matrix(m,n)

def minimize(objective, constraints, arguments=[], name=None):
    return Program(objective, constraints, arguments, name)

###################################################################
###################################################################

def main():
    x = variable()
    p = minimize(abs(x), [x >= 1, x <= 4])

    print p
    q = p.epigraph()
    print q
    r = unroll(q)
    print r

    print p.is_dcp()

    #result = r.solve()
    #for k, v in result.iteritems():
    #    print "%s: %s" % (k, v)

if __name__ == '__main__':
    main()
