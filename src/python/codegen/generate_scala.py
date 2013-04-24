"""
Copyright (c) 2012-2013, Eric Chu (eytchu@gmail.com)
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met: 

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer. 
2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution. 

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.

The views and conclusions contained in the software and documentation are
those of the authors and should not be interpreted as representing official
policies, either expressed or implied, of the FreeBSD Project.
"""

import re

import cvxopt as o
from cvxopt import solvers
from scoop.expression import Parameter
from codegen import mangle, ismultiply, istranspose, height_of, recover_variables
# for embedded in Delite

def replace_with_opticvx(s):
    # replace ones^T*(varname) with (varname)._sum
    slist = s.split('+')
    slist = map(lambda x: re.sub(r'\s*ones\^T\*([a-zA-Z_]+)\s*',r'\1.sum', x), slist)
    # replace x' with x.T
    retlist = re.sub(r'\'', r'.T',' + '.join(slist))
    
    return retlist

def valid_args(e):
    return isinstance(e,int) or isinstance(e,o.matrix) or isinstance(e,o.spmatrix) or isinstance(e,float)

def generate(self, **kwargs):
    """This function will make sure to check that all *vector* variables have
    their dimension defined. If dimensions are defined for SCALAR variables, 
    they are ignored."""
    
    codegen = self.codegen    
    used = set(self.used_syms)
    if all(valid_args(e) for e in kwargs.values()):
        # args contains *actual* dimensions (for variables) and parameter values
        args = mangle(kwargs)
        sizes = codegen.get_variable_sizes(args)
    
        def variables():
            for k in codegen.variables:
                yield "val %s = cvxexpr" % k
    
        def parameters():
            for k in codegen.parameters:
                yield "val %s = cvxparam" %k
            
        def params():
            for k in codegen.parameters:
                yield "params(%s)," % k
            
        def given():
            for k in codegen.parameters:
                yield "given(%s)," % k
    
        def over():
            for k in codegen.variables:
                yield "vector(%d) -> %s" % (sizes[k], k)
        
        def constraints():
            for k in codegen.cones:
                if k.size == 0:
                    s = str(k.t)
                    yield "%s == 0" % replace_with_opticvx(s)
                elif k.size == 1:
                    s = str(k.t)
                    yield "%s >= 0" % replace_with_opticvx(s)
                else:
                    if len(k.arglist) == 1:
                        s1 = ', '.join(map(lambda x: replace_with_opticvx(str(x)),k.arglist[0]))
                        s2 = replace_with_opticvx(str(k.t))
                        yield "in_secondorder_cone(cat(%s),%s)" % (s1, s2)
                    else:                        
                        yield "cfor(...) ( (i) => in_second_order_cone(cat(...), ...) )"

                
            
        print """
def main(args: Array[String]) {
  /* parameter declarations */
  %s
    
  /* variable declarations */
  %s
  
  /* define the SOCP */
  val prob = problem(
    /* list params? */
    %s
    
    /* list inputs */
    %s
    
    over(%s),
    where(
      %s
    ),
    minimize(%s)
  )
  
}""" % (    '\n  '.join(variables()), 
            '\n  '.join(parameters()), 
            '\n    '.join(params()), 
            '\n    '.join(given()),
            ', '.join(over()),
            ',\n      '.join(constraints()),
            replace_with_opticvx(str(codegen.obj))
             )

    else:
        raise Exception("Expected integer arguments for variables and matrix or float arguments for params.") 
"""
    codegen = self.codegen
    used = set(self.used_syms)
    
    # get the used params and the used variables
    params = dict( (k,v) for k,v in self.symtable.iteritems() if k in used and isinstance(v,Parameter) )
    variable_set = codegen.needed_variable_dims(used)
    
    # for the objective 
    c = codegen.obj.linfunc.linear_dict
    
    # these are the data matrices
    A, b, b_height = [], [], []
    Gl, hl, hl_height = [], [], []
    Gq, hq, hq_height = [], [], []
    # this is a block matrix for product cones
    Gblk, hblk, hblk_blocks = [], [], [] # these are for doing things like abs(x) <= t
    
    for k in codegen.cones:
        if k.size == 0:
            # print "goes in A,b"
            row, coeff, sizes = k.get_all_rows()
            A += row
            b += coeff
            b_height += sizes
        elif k.size == 1:
            # print "goes in Gl, hl"
            row, coeff, sizes = k.get_all_rows()
            Gl += row
            hl += coeff
            hl_height += sizes
        elif isinstance(k.size, int):
            # print "row-wise goes in Gblk,hblk"
            row, coeff, sizes = k.get_all_rows()
            Gblk.append(row)
            hblk.append(coeff)
            hblk_blocks.append(sizes)
        else:
            # print "concat in Gq,hq"
            row, coeff, sizes = k.get_all_rows()
            Gq.append(row)
            hq.append(coeff)
            hq_height.append(sizes)
        if all(valid_args(e) for e in kwargs.values()):
            # args contains *actual* dimensions (for variables) and parameter values
            args = mangle(kwargs)
            
            # only care about the ones that are used
            # args = dict( (k,v) for k,v in mangled.iteritems() if k in set(used) )
            
            # make sure all keys are subset of needed variable list
            if variable_set.issubset(args) and set(params).issubset(args):
                # first, make sure all parameter arguments are sparse matrices
                for k in set(params):
                    if isinstance(args[k], float) or isinstance(args[k], int):
                        args[k] = o.spmatrix(args[k],[0],[0])
                    elif isinstance(args[k], o.matrix):
                        args[k] = o.sparse(args[k])
                                
                # get the size lookup table using actual dimensions
                sizes = codegen.get_variable_sizes(args)
                # build the location of the start indices from the size table
                start_idxs, cum = {}, 0
                for k,v in sizes.iteritems():
                    start_idxs[k] = cum
                    cum += v
                                
                # add parameter sizes to the dictionary (!!!hack)
                for k in codegen.parameters:
                    if k in set(args):
                        sizes[k] = args[k].size
                
                # get objective vector
                c_obj = o.matrix(0, (cum,1), 'd')
                for k,v in c.iteritems():
                    # we ignore constant coefficients
                    if k != '1':
                        idx = start_idxs[k]
                        row_height = sizes[k]
                        c_obj[idx:idx+row_height] = eval_matrix_coeff(v, args, row_height, 1, transpose_output=True)
                
                # get matrices
                A_mat, b_mat = build_matrix(A, b, b_height, args, sizes, start_idxs, cum)
                Gl_mat, hl_vec = build_matrix(Gl, hl, hl_height, args, sizes, start_idxs, cum)
                Gq_mats, hq_vecs = [], []
                
                # matrices in SOC
                for G, h, height in zip(Gq, hq, hq_height):
                    mat, vec = build_matrix(G, h, height, args, sizes, start_idxs, cum)
                    # ensure that sizes agree
                    oldsize = mat.size
                    mat.size = (oldsize[0], cum)
                    Gq_mats.append(mat)
                    hq_vecs.append(vec)
                
                for G, h, height in zip(Gblk, hblk, hblk_blocks):
                    mats, vecs = build_block_matrices(G, h, height, args, sizes, start_idxs, cum)
                    # ensure that sizes agree
                    for m in mats:
                        oldsize = m.size
                        m.size = (oldsize[0], cum)
                    
                    Gq_mats += mats
                    hq_vecs += vecs

                sol = solvers.socp(c_obj, Gl_mat, hl_vec, Gq_mats, hq_vecs, A_mat, b_mat)
                # print sol
                # # Gl_mat, hl_vec
                # 
                # print sizes
                # print c_obj
                # print A_mat
                # print b_mat
                # print Gl_mat
                # print hl_vec
                # 
                #     
                # print c_obj

                solution = recover_variables(sol['x'], start_idxs, sizes, codegen.variables)
                solution['primal objective'] = sol['primal objective']
                solution['status'] = sol['status']
                return solution

            else:
                raise Exception("Not all variable dimensions or parameters have been specified.")
        else:
            raise Exception("Expected integer arguments for variables and matrix or float arguments for params.") 
    """
    