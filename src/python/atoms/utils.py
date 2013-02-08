import scoop as s

def create_varname():
    """Creates a new, temporary variable name"""
    name = 't' + str(s.Scoop.varcount)
    s.Scoop.varcount += 1
    
    return name

    
    