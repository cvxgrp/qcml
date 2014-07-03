def create_encoder(lookup):
    # creates a function which uses the lookup table to determine what string
    # to print. uses the class name of the object
    def encode(x):
        try:
            #encode_func = lookup.get(x.__class__, lambda x: x.__str__())
            return lookup[x.__class__](x)
        except TypeError:
            print x
            # print x.arg
            # print x.arg.left
            # print x.arg.left.is_matrix_param
            # print x.arg.right
            # print x.isknown
            # print x.isscalar
            # print x.is_matrix_param
            # print x.__class__
            # print lookup
            raise
    return encode
