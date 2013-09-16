""" ProgramData contains the dictionaries for the dimensions, parameters, and 
    variables of a problem. It also has the ability to iterate over them.
"""
class ProgramData(object):
    def __init__(self, dimensions = None, parameters = None, variables = None):
        """ Creates ProgramData.
        """
        self.dimensions = dimensions if dimensions else set()
        # parameters declared by the user
        self.parameters = parameters if parameters else {}
        # variables declared by the user
        self.variables = variables if variables else {}
    
    def __iter__(self):
        for dim in self.dimensions:
            yield dim
        for param in self.parameters:
            yield param
        for variable in self.variables:
            yield variable


