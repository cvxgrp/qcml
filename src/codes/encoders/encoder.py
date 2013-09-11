def create_encoder(lookup):
    # creates a function which uses the lookup table to determine what string
    # to print. uses the class name of the object
    def encode(x):
        #encode_func = lookup.get(x.__class__, lambda x: x.__str__())
        return lookup[x.__class__](x)
    return encode
