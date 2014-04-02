""" ProgramData contains the dictionaries for the dimensions, parameters, and
    variables of a problem. It also has the ability to iterate over them.
"""
class ProgramData(object):
    def __init__(self, dimensions = None, parameters = None, variables = None):
        """ Creates ProgramData.
        """
        # dimensions declared by the user
        self.__dimensions = dimensions if dimensions else set()
        # keep track of the original, abstract dims
        self.__original_dims = self.__dimensions

        # parameters declared by the user
        self.parameters = parameters if parameters else {}
        # variables declared by the user
        self.variables = variables if variables else {}

    @property
    def abstract_dims(self):
        return [dim for dim in self.__dimensions if isinstance(dim, str)]

    @property
    def dimensions(self):
        return self.__dimensions

    @dimensions.setter
    def dimensions(self, dims):
        # whenever you set dims, you reset the abstract dimensions
        self.__dimensions = self.__original_dims
        for elem in self.parameters.values():
            elem.shape.eval(dims)
        for elem in self.variables.values():
            elem.shape.eval(dims)
        self.__dimensions = set([dims.get(k, k) for k in self.dimensions])
