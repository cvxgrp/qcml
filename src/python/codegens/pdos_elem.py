from pdos import PDOSCodegen

class PDOSElemCodegen(PDOSCodegen):
    def __init__(self, dims, path="/usr/local/lib"):
        self.path = path
        super(PDOSElemCodegen,self).__init__(dims)

    def function_preamble(self):
        yield "import cvxopt as o"
        yield "import numpy as np"

    def function_prototype(self):
        yield "def stuff_matrices(params):"


    def function_solve(self):
        yield "dims['f'] = %s" % self.num_lineqs
        yield "b = o.matrix([b, h])"
        yield "A = o.matrix(o.sparse([A, G]))"


    def function_recover(self, keys):
        yield "return {'A': np.array(A), 'b': np.array(b).flatten(), 'c': np.array(c).flatten(), 'dims': dims}"


    def _codegen_helper(self):
        import sys
        sys.path.append(self.path)
        import elem
        import numpy as np
        from mpi4py import MPI

        comm = MPI.COMM_WORLD
        rank = comm.Get_rank()

        '''This module contains code for a PDOS solver using Elemental python
        bindings for the linear algebra.
        The key method is solve, which takes in elemental distributed matrices.
        '''

        def proj_q(x):
            '''project onto quadratic cone. first element of the array is t.
            modify x in place.'''
            if len(x) <= 1:
                x = np.maximum(x,0)
                return x

            t = x[0]
            v = x[1:] #v is a view
            normv = np.linalg.norm(v)
            if normv <= -t:
                x[:] = 0
            elif normv > t:
                a = (normv + t)/2.0
                x[0] = a
                v[:] = a*v/normv

            #otherwise, leave vector unchanged

        def proj_K(s,dims):
            '''project local vector s onto the product cone K.
            change s in place.'''

            var_zero,var_lin,var_quad = np.split(s,np.cumsum([dims['f'],dims['l']]))
            var_zero[:] = 0
            var_lin[:] = np.maximum(var_lin,0)
            for vec in np.split(var_quad,np.cumsum(dims['q'])):
                proj_q(vec)

        def proj_cone(s,y,stild,slocal,lamb,dims):
            '''paralell code for doing the cone projection.
            sends distributed vector to a single node, computes the
            cone projection there, and then redistributes vector.
            works on data in place.'''

            elem.Copy(stild,s)
            elem.Axpy(-1.0*lamb,y,s)
            elem.Copy(s,slocal)

            if slocal.Participating:
                proj_K(slocal.Matrix,dims)

            elem.Copy(slocal,s)


        def proj_E(A,factC,p,b,c,lambd,x,s,y,stild):
            '''performs the projection onto Ax+s=b for PDOS, that is,
            x+,stild+ = proj_E(x-lambda*c,s+lambda*y).
            The first step is
            x+ = L-T*L-1( x - lambd*c + A^T*(b - s + lambd*y) ).
            The second step is
            stild+ = b - A*x.
            Modifies x,s,stild in place.'''
            elem.Axpy(-1.0,b,s)
            elem.Axpy(lambd,y,s)
            elem.Axpy(-lambd,c,x)
            elem.Gemv(elem.TRANSPOSE,-1.0,A,s,1.0,x)
            elem.SolveAfter_LU(elem.NORMAL,factC,p,x)
            elem.Copy(b,stild)
            elem.Gemv(elem.NORMAL,-1.0,A,x,1.0,stild)

        def compute_residuals(c,A,b,x,y,s,res_primal,res_dual):
            #form the primal residual in res_primal = Ax + s - b
            elem.Copy(s,res_primal)
            elem.Axpy(-1.0,b,res_primal)
            elem.Gemv(elem.NORMAL,1.0,A,x,1.0,res_primal)
            norm_res_primal = elem.Norm(res_primal)

            #form the dual residual in res_dual = A^Ty + c
            elem.Copy(c,res_dual)
            elem.Gemv(elem.TRANSPOSE,1.0,A,y,1.0,res_dual)
            norm_res_dual = elem.Norm(res_dual)

            return norm_res_primal,norm_res_dual


        _opts = {'MAX_ITERS': 2000,
                 'EPS_ABS': 1e-4,
                 'LAMBDA': 1.0}

        def solve(c,A,b,dims,opts={}):
            '''takes in DistMatrix values c,A,b and solves, using Elemental
            for the linear algebra'''
            #TODO: fix options loading
            m = A.Height
            n = A.Width

            #user need not specify all options
            opts1 = _opts.copy()
            opts1.update(opts)
            opts = opts1

            if rank==0:
                alg_start_time = MPI.Wtime()

            sol_vars = {'status': 'reached MAX_ITER',
                        'm': m,
                        'n': n,
                        'x': None,
                        's': None,
                        'y': None,
                        'factor_time': None,
                        'form_time': None,
                        'total_time': None}

            #we form C = ATA + I in elemental
            #then factor in elemental

            if rank == 0:
                start_time = MPI.Wtime()
            C = elem.DistMatrix_d(n,n)
            p = elem.DistMatrix_i_VC_STAR()
            elem.Identity(C,n,n)
            elem.Gemm(elem.TRANSPOSE,elem.NORMAL,1.0,A,A,1.0,C)
            if rank == 0:
                sol_vars['form_time'] = MPI.Wtime()-start_time
                print 'Forming matrix took ',
                print '%f seconds.'%(sol_vars['form_time'])

            if rank == 0:
                start_time = MPI.Wtime()
            elem.LU(C,p)
            if rank == 0:
                sol_vars['factor_time'] = MPI.Wtime()-start_time
                print 'Computing LU took ',
                print '%f seconds.'%(sol_vars['factor_time'])


            #elem.SolveAfter_LU(elem.NORMAL,C,p,x)

            #set up iteration variables
            x = elem.DistMatrix_d(n,1)
            s = elem.DistMatrix_d(m,1)
            slocal = elem.DistMatrix_d_CIRC_CIRC(m,1)
            stild = elem.DistMatrix_d(m,1)
            y = elem.DistMatrix_d(m,1)

            res_primal = elem.DistMatrix_d(m,1)
            res_dual = elem.DistMatrix_d(n,1)

            elem.Zeros(x,n,1)
            elem.Zeros(s,m,1)
            elem.Zeros(y,m,1)

            ##options
            maxiters = opts['MAX_ITERS']
            lamb = opts['LAMBDA']

            #stopping tolerances
            eps_abs = opts['EPS_ABS']
            eps_pri = eps_abs*(1+elem.InfinityNorm(b))
            eps_dual = eps_abs*(1+elem.InfinityNorm(c))

            #start iteration
            res_pris = []
            res_duals = []
            for i in range(maxiters):

                proj_E(A,C,p,b,c,lamb,x,s,y,stild)

                proj_cone(s,y,stild,slocal,lamb,dims)

                #y update
                elem.Axpy(-1.0,s,stild)
                elem.Axpy(-1.0/lamb,stild,y)

                norm_res_primal,norm_res_dual = compute_residuals(c,A,b,x,y,s,res_primal,res_dual)
                res_pris.append(norm_res_primal)
                res_duals.append(norm_res_dual)

                if rank==0:
                    print '%d:\t%f %f'%(i,norm_res_primal,norm_res_dual)

                if norm_res_primal <= eps_pri and norm_res_dual <= eps_dual:
                    sol_vars['status'] = 'solved'
                    break

            if rank==0:
                sol_vars['total_time'] = MPI.Wtime() - alg_start_time

            sol_vars['x'] = x
            sol_vars['s'] = s
            sol_vars['y'] = y
            #maybe put in input options, lambda, residuals. good for record-
            #keeping
            sol_vars['res_pris'] = res_pris
            sol_vars['res_duals'] = res_duals
            sol_vars['opts'] = opts

            return sol_vars
        return solve

    def codegen(self):
        # execute bytecode to create the "stuff_matrices" function
        exec '\n'.join(self.prog) in locals()
        self.stuff_matrices = stuff_matrices

        import sys
        sys.path.append(self.path)
        import elem
        import numpy as np
        from mpi4py import MPI

        comm = MPI.COMM_WORLD
        rank = comm.Get_rank()

        def disp_sol(sol):
            print '\nExit status: ' + sol['status']
            print 'Matrix formation time was %f seconds.'%sol['form_time']
            print 'Matrix factor time was %f seconds.'%sol['factor_time']
            print 'Total PDOS time was %f seconds.'%sol['total_time']

        def form_dist(params):
            Alocal = elem.DistMatrix_d_CIRC_CIRC()

            if Alocal.Grid.Rank == Alocal.Root:
                pdos_vars = self.stuff_matrices(params)
                A = pdos_vars['A']
                b = pdos_vars['b']
                c = pdos_vars['c']
                dims = pdos_vars['dims']
                m,n = A.shape
            else:
                m,n,dims = None, None, None

            m = comm.bcast(m, root=Alocal.Root)
            n = comm.bcast(n, root=Alocal.Root)
            dims = comm.bcast(dims, root=Alocal.Root)

            Alocal.ResizeTo(m,n)
            blocal = elem.DistMatrix_d_CIRC_CIRC(m,1)
            clocal = elem.DistMatrix_d_CIRC_CIRC(n,1)

            if Alocal.Grid.Rank == Alocal.Root:
                elem.Copy( A, Alocal.Matrix )
                elem.Copy( b, blocal.Matrix )
                elem.Copy( c, clocal.Matrix )

            A = elem.DistMatrix_d()
            b = elem.DistMatrix_d()
            c = elem.DistMatrix_d()
            elem.Copy( Alocal, A )
            elem.Copy( blocal, b )
            elem.Copy( clocal, c )

            if rank == 0:
                print 'A is %d by %d'%(A.Height,A.Width)
                print 'On Rank 0 proc, local A has shape %d by %d'%A.Matrix.shape

            return c,A,b,dims

        elem_pdos_solver = self._codegen_helper()

        def solve(params, opts = {}):
            c,A,b,dims = form_dist(params)
            sol = elem_pdos_solver(c,A,b,dims,opts)

            if rank == 0:
                disp_sol(sol)

            return sol
        return solve