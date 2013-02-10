import scoop as s
import scoop.macro as m
import re

first_underscore = re.compile("^_")

def create_varname():
    """Creates a new, temporary variable name"""
    name = 't' + str(s.Scoop.varcount)
    s.Scoop.varcount += 1
    
    return name

def comment(fn, *args):
    def atom(*args):
        """Decorates an atom to perform some housekeeping. It checks to 
        see if an expression using this atom has been called before. If 
        so, it just returns the previous Expression object. If not, it 
        executes the code and puts the new result into the MacroExpander
        lookup table.
        """
        arglist = ', '.join( map(lambda e: re.sub(first_underscore, "",e.name), args) )
    
        # get the name of the function and its arguments
        func_name = fn.__name__.rstrip('_') # remove trailing underscores
        expr_string = '%s(%s)' % (func_name, arglist)
        v = m.MacroExpander.lookup.get(expr_string, None)
        if v:
            return ([], v)
        else:
            lines, v = fn(*args)
            comment = ["# '%s' canonicalizes '%s'" % (v.name, expr_string)]
            if lines:
                lines = comment + lines
                m.MacroExpander.lookup[expr_string] = v
            return (lines, v)
    return atom