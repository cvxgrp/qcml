# TODO: (ECHU) presumably, this will work, but in actuality, i'm not sure...
# Need to test this.
#
# This code doesn't work yet. Requires expression slices.
#
from .. ast.constraints import SOC, SOCProd
from .. codes import SliceCoeff
from variable_creation_mixin import VariableCreatorMixin

class FixedConeMixin(VariableCreatorMixin):
    """ This implements the fixed cone size behavior.
    """
    def __init__(self, cone_size = None, *args, **kwargs):
        super(FixedConeMixin, self).__init__(*args, **kwargs)

        if cone_size is not None: self.cone_size = max(3,cone_size)
        else: self.cone_size = 3

    def visit_SOC(self, node):
        if self.cone_size is not None:
            # look at the size of the SOC
            cone_length = 1
            for e in node.left:
                dim = e.shape.size(abstractdim_rewriter=self.abstractdim_rewriter)
                cone_length += dim

            while cone_length > self.cone_size:
                # maximum number of elements on the lhs
                max_lhs = self.cone_size - 1

                # collect the new arguments
                new_args = []
                old_args = []
                cum = 0
                create_new = True
                for e in node.left:
                    if create_new:
                        dim = e.shape.size(abstractdim_rewriter=self.abstractdim_rewriter)
                        # if the dimension of the current expression doesn't
                        # exceed the max allowable, just push onto argument stack
                        if cum + dim <= max_lhs:
                            new_args.append(e)
                        else:
                            # if it exceeds, only push the slice up to max_lhs
                            new_args.append(SliceCoeff(e, 0, max_lhs - cum))
                            # save the rest of the expression for another cone
                            old_args.append(SliceCoeff(e, max_lhs - cum, dim))

                        if cum + dim >= max_lhs:
                            create_new = False
                    else:
                        # just push into the old args
                        old_args.append(e)
                    cum += dim

                # create a new variable
                new_var = self.create_variable(1)

                # process the new cone, which has the right size
                super(FixedConeMixin,self).visit_SOC(SOC(new_var, new_args))

                # process the old cone
                old_args.append(new_var)

                node.left = old_args
                cone_length -= (max_lhs - 1) # the extra "1" is the rhs

            if cone_length < self.cone_size:
                # create a new variable and append to the node
                new_length = self.cone_size - cone_length
                new_var = self.create_variable(new_length)
                node.left.append(new_var)

        super(FixedConeMixin,self).visit_SOC(node)

    def visit_SOCProd(self, node):
        if self.cone_size is not None:
            # look at the size of the SOC
            n = node.shape.size(abstractdim_rewriter=self.abstractdim_rewriter)

            cone_length = 1 + node.nargs
            #print cone_length

            while cone_length > self.cone_size:
                # maximum number of elements on the lhs
                max_lhs = self.cone_size - 1

                # collect the new arguments
                new_args = []
                old_args = []
                count = 0
                for e in node.arglist:
                    if count < max_lhs: new_args.append(e)
                    else: old_args.append(e)
                    count += 1

                new_var = self.create_variable(n)

                # process the new cone, which has the right size
                super(FixedConeMixin,self).visit_SOCProd(SOCProd(new_var, new_args))

                # process the old cone
                old_args.append(new_var)

                node.arglist = old_args
                cone_length -= (max_lhs - 1) # the extra "1" is the rhs

            if cone_length < self.cone_size:
                # create a new variable and append to the node
                new_length = self.cone_size - cone_length
                for i in range(new_length):
                    new_var = self.create_variable(n)
                    node.arglist.append(new_var)

        super(FixedConeMixin,self).visit_SOCProd(node)
