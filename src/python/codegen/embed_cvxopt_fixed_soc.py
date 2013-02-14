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

import math
import cvxopt as o
from cvxopt import solvers
from scoop.expression import Parameter, Variable, Scalar, Vector, Constant, Cone
from codegen import mangle, ismultiply, istranspose, height_of
from scoop.atoms.utils import create_varname
# for embedded in python


# these functions should probably go into codegen
def eye(v,m):
    return o.spmatrix(v, range(m), range(m))

def ones(v,m):
    return o.spmatrix(v, range(m), [0]*m)
    
def onesT(v,m):
    return o.spmatrix(v, [0]*m, range(m))
    
# define how to evaluate a coefficient in a matrix
# a failure case sum(a*x), where x is scalar, but a is vector
# another failure case is A*B*C, where B has diff dimensions than C.cols
def eval_matrix_coeff(coeff, params, rows, cols, transpose_output=False):
    params['ones^T'] = onesT(1,cols)
    v = coeff.constant_value()
    if v:
        if cols == rows:
            return eye(v, rows)
        elif cols == 1:
            return ones(v, rows)
        else:
            raise Exception("Unknown matrix coeff...")
    else:
        value = 0
        for k,v in coeff.coeff_dict.iteritems():
            if k == '1':
                if cols == rows:
                    value += eye(v, rows)
                elif cols == 1:
                    value += ones(v, rows)
                else:
                    raise Exception("Unknown matrix coeff...")
            elif not ismultiply(k):
                p = params[ k.rstrip('\'') ]
                if istranspose(k): value += v*p.T
                else: value += v*p
            else:
                keys = k.split('*')
                keys.reverse()
                mult = 1
                prev_param = None
                for k1 in keys:
                    
                    # in the special case where we we have something like
                    # sum(a*x) = 1^T*a*x and 'a' is a vector while x is scalar
                    # so the initial size of ones^T was not correct. we resize
                    # ones^T so that it "matches"
                    if k1 == 'ones^T':
                        params[k1] = onesT(1,prev_param.size[0])

                    p = params[ k.rstrip('\'') ]
                    if istranspose(k): 
                        mult = p.T*mult
                        prev_param = p.T
                    else: 
                        mult = p*mult
                        prev_param = p
                value += v*mult
        if transpose_output: return value.T
        else: return value

# define how to evaluate a coefficient in a vector
def eval_coeff(coeff, params, rows):
    v = coeff.constant_value()
    if v:
        return o.matrix(v, (rows,1))
    else:
        value = 0
        for k,v in coeff.coeff_dict.iteritems():
            if k == '1':
                value += o.matrix(v, (rows,1))
            elif not ismultiply(k):
                p = params[ k.rstrip('\'') ]
                if istranspose(k): value += v*p.T
                else: value += v*p
            else:
                keys = k.split('*')
                keys.reverse()
                mult = 1
                for k1 in keys:
                    p = params[ k.rstrip('\'') ]
                    if istranspose(k): mult = p.T*mult
                    else: mult = p*mult
                value += v*mult
        return value
        
def build_matrix(A,b,b_height, params,vec_sizes,start_idxs,total_width):
    h_cum = height_of(b_height, vec_sizes)
    h_vec = o.matrix(0, (h_cum,1), 'd')
    G_vals = []
    G_I, G_J = [], []
    idx = 0  
        
    for row, coeff, size in zip(A,b,b_height):
        row_height = size.row_value(vec_sizes)
        
        h_vec[idx:idx+row_height] = eval_coeff(coeff, params, row_height)
        for k,v in row.iteritems():   # repeated function
            # we ignore constant coefficients
            if k != '1':
                col_width = vec_sizes[k]
                result_mat = o.sparse(eval_matrix_coeff(v, params, row_height, col_width))
                # set the row
                G_I += (result_mat.I + idx)
                # set the column
                G_J += (result_mat.J + start_idxs[k])
                # set the values
                G_vals += result_mat.V
                            
        idx += row_height

    Gl_mat = o.spmatrix(G_vals, G_I, G_J, (h_cum, total_width))
    hl_vec = h_vec
    return (Gl_mat, hl_vec)

def build_block_matrices(A_blk,b_blk,b_blk_height, params,vec_sizes,start_idxs,total_width):
    heights = map(lambda e:e.row_value(vec_sizes), b_blk_height)
    print b_blk_height
    if any(e != heights[0] for e in heights):
        raise Exception("Expected blocks to be the same size!")
        
    blk_sz = len(A_blk)
    blk_height = heights[0]
    total_height = sum(heights)
    
    G_vals = []
    G_I, G_J = [], []
    idx = 0  
    h_vec = o.matrix(0, (total_height,1), 'd')
    
    for row, coeff, size in zip(A_blk,b_blk,b_blk_height):
        row_height = size.row_value(vec_sizes) # == blk_height
        h_vec[idx::blk_sz] = eval_coeff(coeff, params, row_height)
        for k,v in row.iteritems():   # repeated function
            # we ignore constant coefficients
            if k != '1':
                col_width = vec_sizes[k]
                result_mat = o.sparse(eval_matrix_coeff(v, params, row_height, col_width))
                # set the row
                G_I += (blk_sz*result_mat.I + idx)
                # set the column
                G_J += (result_mat.J + start_idxs[k])
                # set the values
                G_vals += result_mat.V
                            
        idx += row_height+1
        idx %= blk_height
    
    Gl_mat = o.spmatrix(G_vals, G_I, G_J, (total_height, total_width))
    hl_vec = h_vec
    
    # now, chop them up into 'blk_height' matrices, each with 'blk_sz' rows
    G_mats = []
    h_vecs = []
    for i in range(blk_height):
        G_mats.append(Gl_mat[blk_sz*i:blk_sz*i+blk_sz, :])
        h_vecs.append(hl_vec[blk_sz*i:blk_sz*i+blk_sz])
    
    return (G_mats, h_vecs)
    
def pad_matrix(G, h, start_idxs, total_width, target_size):
    init_size = G.size[0]
    counter = 0
    Ai, Aj, Aval = [], [], []
    Gi, Gj, Gval = list(G.I), list(G.J), list(G.V)
    hval = list(h)
    b = []
    while init_size < target_size:
        new_var = create_varname()  # this is always scalar
        # create a new var and ensure its index is at the end
        start_idxs[new_var] = total_width

        Ai.append(counter)
        Aj.append(total_width)
        Gi.append(init_size)
        Gj.append(total_width)
        Gval.append(-1.)
        
        # increment the total number of variables
        total_width += 1
        counter += 1
        init_size += 1
        
    Aval = counter*[1.]
    b = counter*[0.]
    hval += counter*[0.]
    
    return [Gi], [Gj], [Gval], [hval], [Ai], [Aj], [Aval], [b], total_width
        
def chop_matrix(G, h, start_idxs, total_width, target_size):
    if h.size[0] <= target_size:
        return pad_matrix(G,h, start_idxs, total_width, target_size)
    else:
        Ai, Aj, Aval = [], [], []
        b = []
        Gis, Gjs, Gvals = [], [], []
        hlist = []
        hval = list(h)
        t, ht = G[0,:], h[0]
        others = G[1:,:]
        #ti,tj,val = G.I[0], G.J[0], G.V[0]          # first component is "t"
        #Gi, Gj, Gval = G.I[1:], G.J[1:], G.V[1:]    # the rest
        
        hval = list(h[1:])
        
        arglist_size = target_size - 1  # recall that target size counts "t" as well
        l = others.size[0]
        # chunk size
        n = int(math.ceil(l/float(arglist_size)))
        G_chunk = [others[i:i+n,:] for i in range(0,l,n)]
        h_chunk = [hval[i:i+n] for i in range(0,l,n)]
        
        
        # introduce a new variable for each arg
        new_variables = []
        for g,hc in zip(G_chunk,h_chunk):
            Gi, Gj, Gval = list(g.I+1), list(g.J), list(g.V)
            new_var = create_varname()  # this is always scalar
            # create a new var and ensure its index is at the end
            start_idxs[new_var] = total_width
            new_variables.append(total_width)
            
            Gi.append(0)
            Gj.append(total_width)
            Gval.append(-1.)
            total_width += 1
            
            h_vec = o.matrix([0] + hc)
            G_mat = o.spmatrix(Gval, Gi, Gj, (g.size[0]+1, total_width))
            
            Gi_list, Gj_list, Gval_list, hval_list, \
            Ai_list, Aj_list, Aval_list, b_list, total_width \
                = chop_matrix(G_mat, h_vec, start_idxs, total_width, target_size)
            #[Gi], [Gj], [Gval], [hval], [Ai], [Aj], [Aval], [b], total_width
            
            Gis += Gi_list
            Gjs += Gj_list
            Gvals += Gval_list
            hlist += hval_list
            Ai += Ai_list
            Aj += Aj_list
            Aval += Aval_list
            b += b_list
            
        ti, tj, tval = list(t.I), list(t.J), list(t.V)
        counter = 1
        for ind in new_variables:
            tval.append(-1.)
            tj.append(ind)
            ti.append(counter)
            counter += 1
            
        hlist.append([ht] + (counter-1)*[0])
        Gis.append(ti)
        Gjs.append(tj)
        Gvals.append(tval)

        return Gis, Gjs, Gvals, hlist, Ai, Aj, Aval, b, total_width
    
def valid_args(e):
    return isinstance(e,int) or isinstance(e,o.matrix) or isinstance(e,o.spmatrix) or isinstance(e,float)


def generate(self, soc_sz):
    """This function will make sure to check that all *vector* variables have
    their dimension defined. If dimensions are defined for SCALAR variables, 
    they are ignored.
    
    This code generator assumes the second-order cone size has a fixed length."""
    
    if soc_sz < 3:
        raise Exception("Minimum SOC size is 3.")
    
    codegen = self.codegen
    used = set(self.used_syms)
    
    def pad_cone(k, target_size):
        row, coeff, sizes = k.get_all_rows()
        init_size = k.size
        cones = []
        others = []
        while init_size < target_size:
            v = Variable(create_varname(), sizes[0])
            codegen.variables[v.name] = v
            cones.append(v == Constant(0))
            others.append(v)
            init_size += 1
        arglist = list(k.arglist) + others
        new_cone = Cone.SOC(k.t, *arglist)
        cones.append(new_cone)
        return cones
        
    def split_cone(k, target_size):
        if k.size <= target_size:
            return pad_cone(k, target_size)
        
        cones = []
        row, coeff, sizes = k.get_all_rows()
        arglist_size = target_size - 1  # recall that target size counts "t" as well
        t = k.t
        arglist = list(k.arglist)
        l = len(arglist)
        # chunk size
        n = int(math.ceil(l/float(arglist_size)))
        args = [arglist[i:i+n] for i in range(0,l,n)]
        
        # introduce a new variable for each arg
        new_variables = []
        for a in args:
            v = Variable(create_varname(), sizes[0])
            codegen.variables[v.name] = v
            new_variables.append(v)
            new_cone = Cone.SOC(v, *a)
            cones += split_cone(new_cone, target_size)
        
        new_cone = Cone.SOC(k.t, *new_variables)
        cones.append(new_cone)
        return cones

    
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
    
    # chop up known blocks
    cones = [] 
    while codegen.cones:
        k = codegen.cones.pop()
        if k.size == 0 or k.size == 1:
            cones.append(k)
        elif isinstance(k.size, int):
            counter = k.size
            row, coeff, sizes = k.get_all_rows()
            others = []
            # i think just a simple "split_cone" call will do
            if counter < soc_sz:
                cones += pad_cone(k, soc_sz)
            elif counter > soc_sz:
                cones += split_cone(k, soc_sz)
            else:
                cones.append(k)
        else:
            cones.append(k)
        
    codegen.cones = cones
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

            
    def solver(**kwargs):
        if all(valid_args(e) for e in kwargs.values()):
            # args contains *actual* dimensions (for variables) and parameter values
            args = mangle(kwargs)
            
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
                        sizes[k] = args[k].size[0]
                
                Gis, Gjs, Gvals, hs = [],[],[],[]
                Ais, Ajs, Avals, bs = [],[],[],[]
                # we do the SOC first, since we will be introducing new variables (new columns)
                for G, h, height in zip(Gq, hq, hq_height):
                    mat, vec = build_matrix(G, h, height, args, sizes, start_idxs, cum)
                    
                    Gi_list, Gj_list, Gvals_list, hlist, Ai_list, Aj_list, Av_list, \
                    blist, cum = chop_matrix(mat, vec, start_idxs, cum, soc_sz)
                    
                    Gis += Gi_list
                    Gjs += Gj_list
                    Gvals += Gvals_list
                    hs += hlist
                    Ais += Ai_list
                    Ajs += Aj_list
                    Avals += Av_list
                    bs += blist
                    
                    # print args
                    # # ensure that sizes agree
                    # oldsize = mat.size
                    # mat.size = (oldsize[0], cum)
                    # print mat
                    # print vec
                    # Gq_mats.append(mat)
                    # hq_vecs.append(vec)
                    
                # cum is now updated
                Gq_mats, hq_vecs = [], []
                for i,j,v,h in zip(Gis, Gjs, Gvals, hs):
                    hq_vecs.append(o.matrix(h,tc='d'))
                    Gq_mats.append(o.spmatrix(v,i,j,(soc_sz, cum)))
                
                
                # get objective vector
                c_obj = o.matrix(0, (cum,1), 'd')
                for k,v in c.iteritems():
                    # we ignore constant coefficients
                    if k != '1':
                        idx = start_idxs[k]
                        row_height = sizes[k]
                        c_obj[idx:idx+row_height] = eval_matrix_coeff(v, args, row_height, 1, transpose_output=True)
                
                # get matrices
                A_mat, b_vec = build_matrix(A, b, b_height, args, sizes, start_idxs, cum)
                height = A_mat.size[0]
                Ai, Aj, Av, b_vals = list(A_mat.I), list(A_mat.J), list(A_mat.V), list(b_vec)
                # now, update A_mat and b_mat
                for i,j,v,h in zip(Ais, Ajs, Avals, bs):
                    b_vals += h
                    Ai += (map(lambda e: e+height, i))
                    Aj += j
                    Av += v
                    height += len(h)
                    
                
                b_vec = o.matrix(b_vals,tc='d')
                A_mat = o.spmatrix(Av, Ai, Aj, (height, cum))
                    
                Gl_mat, hl_vec = build_matrix(Gl, hl, hl_height, args, sizes, start_idxs, cum)
                
                # these have been pre-divided
                for G, h, height in zip(Gblk, hblk, hblk_blocks):
                    mats, vecs = build_block_matrices(G, h, height, args, sizes, start_idxs, cum)
                    # ensure that sizes agree
                    for m in mats:
                        oldsize = m.size
                        m.size = (oldsize[0], cum)
                    
                    Gq_mats += mats
                    hq_vecs += vecs


                sol = solvers.socp(c_obj, Gl_mat, hl_vec, Gq_mats, hq_vecs, A_mat, b_vec)
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
                return sol

            else:
                raise Exception("Not all variable dimensions or parameters have been specified.")
        else:
            raise Exception("Expected integer arguments for variables and matrix or float arguments for params.") 
    return solver
    