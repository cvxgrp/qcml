#!/usr/bin/env python

# Author: Andrew Best
# abest@ccrma.stanford.edu

# FIR Lowpass Filter with Maximum Stop Band Attenuation
# See FIR Filter Design via Spectral Factorization and Convex Optimization - Wu, Boyd, Vandengerghe
# and fir_mag_design_low.m - Mutapcic at http://cvxr.com/cvx/examples

import sys, os, subprocess, shutil, platform
from qcml import QCML
from numpy import *
from scipy import *
# import matplotlib for plotting if possible
try:
    from matplotlib import pyplot
    plotting = True
except ImportError:
    plotting = False


if __name__ == '__main__':
    
    if len(sys.argv) < 2:
        print "Please provide a path to ECOS solver."
        sys.exit(0)
    else:
        ECOS_PATH = sys.argv[1]

    print "Creating data."

    n = 2

    wpass = 0.12*pi
    wstop = 0.24*pi
    delta = 1.0

    m = 15*n;
    w = linspace(0,pi,m)

    A = zeros((m,n))
    A[0:m, 0] = 1
    for i in range(0, m) :
        for j in range(1, n) :
            A[i,j] = 2*cos(j*w[i])
    print "A ---------------"
    print A

    ind = w <= wpass
    wp = w[ind]
    pb = size(wp)
    Lp = ones(pb)*pow(pow(10.0, -delta/20.0), 2.0)
    print "Lp ---------------"
    print Lp
    Up  = ones(pb)*pow(pow(10.0, +delta/20.0), 2.0)
    print "Up ---------------"
    print Up
    Ap = A[ind, 0:n]
    print "Ap ---------------"
    print Ap

    ind = w >= wstop
    ws = w[ind]
    sb = size(ws)
    As  = A[ind, 0:n]
    print "As ---------------"
    print As

    print "Creating problem."

    # a QCML model is specified by strings
    #   the parser parses each model line by line and builds an internal
    #   representation of an SOCP
    s = """
        dimensions m n pb sb
        variable r(n,1)
        parameters A(m,n) Ap(pb,n) As(sb,n) Lp(pb) Up(pb)
        minimize max(abs(As*r))
            Ap*r >= Lp
            Ap*r <= Up
            A*r >= 0
    """
    print s

    raw_input("press ENTER to parse....")
    p = QCML(debug=True)
    p.parse(s)

    raw_input("press ENTER to canonicalize....")
    p.canonicalize()

    raw_input("press ENTER to generate python code....")
    
    p.dims = {'m': m, 'n': n, 'pb': pb, 'sb': sb}
    p.codegen("python")

    raw_input("press ENTER to solve with ECOS....")
    socp_data = p.prob2socp(params=locals())
    import ecos
    sol = ecos.solve(**socp_data)

    optval = sol.get("info").get("pcost")
    print "The minimum attenuation in the stop band is " + str(10*log10(optval)) + " dB."
    vars = p.socp2prob(sol['x'])
    r = transpose(asmatrix(vars['r']))
    
    # Spectral Factorization
    jay = complex(0,1)
    mult_factor = 100
    m = mult_factor*n
    w = 2*linspace(0,pi-pi/m,m)

    A = zeros((m,n))
    A[0:m, 0] = 1
    for i in range(0, m) :
        for j in range(1, n) :
            A[i,j] = 2*cos(j*w[i])
    R = A*r

    alpha = 0.5*log(R)[0:m,0]
    alphatemp = fft(asarray(alpha))
    alphatemp[floor(m/2):m] = -alphatemp[floor(m/2):m]
    alphatemp[0] = 0;
    alphatemp[floor(m/2)] = 0;
    alphatemp = jay*alphatemp
    phi = real(ifft(alphatemp))

    ind = (linspace(0,m-1,m) % mult_factor) == 0
    alpha1 = alpha[ind]
    phi1 = phi[ind]

    print "h ---------------"
    h = real(ifft(exp(alpha1+jay*phi1),n))
    print h
    
    print 
    print ">> Now we'll generate C code and compile it with C++ compiler."
    print
    raw_input("press ENTER to generate C code....")
    p.dims = {'m': m, 'n': n}   # only specify *some* of the dimensions
    p.codegen("C", name="fir_lowpass")

    raw_input("press ENTER to compile the test C++ program....")
    PATH_TO_EXAMPLES = "../../examples"
    os.chdir("fir_lowpass")
    shutil.copyfile(PATH_TO_EXAMPLES + "/fir_lowpass.cpp", "fir_lowpass.cpp")
    print "Running make...."
    subprocess.call(["make"])
    if platform.system() == 'Linux':
        cmd = ["c++", "-O3", "fir_lowpass.cpp", "-L%s" % ECOS_PATH, 
                "-I%s/include" % ECOS_PATH, 
                "-I%s/external/SuiteSparse_config" % ECOS_PATH,
                "-lecos", "-lm", "-lrt", "fir_lowpass.o", 
                "qcml_utils.o", "-o","fir_lowpass"]
    else:
        cmd = ["c++", "-O3", "fir_lowpass.cpp", "-L%s" % ECOS_PATH, 
                "-I%s/include" % ECOS_PATH, 
                "-I%s/external/SuiteSparse_config" % ECOS_PATH,
                "-lecos", "-lm", "fir_lowpass.o", 
                "qcml_utils.o", "-o","fir_lowpass"]
    print ' '.join(cmd)
    subprocess.call(cmd)

    print
    raw_input("press ENTER to run C++ program....")
    subprocess.call(["./fir_lowpass"])

    print "Verify that the reported objective in C is %f" % optval
    
    # Plot
    if plotting:
        H = fft(h, 2048);
        H = H[0:1024];
        w = linspace(0,pi-pi/1024,1024)

        passbandhi = empty(size(w[w <= wpass]))
        passbandlo = empty(size(w[w <= wpass]))
        passbandhi[:] = +delta
        passbandlo[:] = -delta

        stopband = empty(size(w[w >= wstop]))
        stopband[:] = 10*log10(optval)

        pyplot.plot(w, 20*log10(abs(H)), 'b')
        pyplot.plot(w[w <= wpass], passbandhi, 'r')
        pyplot.plot(w[w <= wpass], passbandlo, 'r')
        pyplot.plot(w[w >= wstop], stopband, 'r')
        pyplot.xlabel('w')
        pyplot.ylabel('|H(w)| dB')
        pyplot.title('FIR Lowpass Filter Magnitude')
        pyplot.axis([0, pi, -100, 10])
        pyplot.grid()
        pyplot.show()
