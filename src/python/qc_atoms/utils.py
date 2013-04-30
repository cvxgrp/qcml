import scoop

def create_varname():
    """Creates a new, temporary variable name"""
    name = 't' + str(scoop.QCRewriter.varcount)
    scoop.QCRewriter.varcount += 1
    
    return name