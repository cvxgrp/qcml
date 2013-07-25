import qcml
from qcml.qc_ast import Variable

def _create_varname():
    """Creates a new, temporary variable name; beings with underscore."""
    name = '_t' + str(qcml.QCRewriter.varcount)
    qcml.QCRewriter.varcount += 1

    return name

def create_variable(shape):
    v = Variable(_create_varname(), shape)
    qcml.QCRewriter.new_variables[v.value] = v
    return v

def annotate(fn_name):
    def decorate(fn, *args):
        def atom(*args):
            """Decorates an atom to perform some housekeeping. It checks to
            see if an expression using this atom has been called before. If
            so, it just returns the previous Variable object. If not, it
            executes the code and puts the new result into the MacroExpander
            lookup table.
            """
            node = args[0]
            other_args = args[1:]

            expr = "%s(%s)" % (fn_name, ','.join(map(str, other_args)))

            # lookup to see if the expression has been created before
            v = qcml.QCRewriter.lookup.get(expr, None)

            if v:
                return (v,[])
            else:
                (v,constraints) = fn(node, *other_args)
                # # TODO: this should be in "create_varname"
                # if isinstance(v, qcml.qc_ast.Variable):
                #     qcml.QCRewriter.new_variables[v.value] = v
                #comment = ["# '%s' canonicalizes '%s'" % (v.name, expr_string)]
                qcml.QCRewriter.lookup[expr] = v
                return (v,constraints)
        return atom
    return decorate
