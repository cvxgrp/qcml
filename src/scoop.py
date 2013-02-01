from scoop_parser import ScoopParser
from profiler import profile, print_prof_data  

# just a driver for the problems

# if the lang were embedded in python, could do some crazy stuff
# problem is getting variable names from the symbol table
if __name__ == '__main__':
    print "hello"
    p = ScoopParser()
    print p.varcount
    p.varcount += 1
    print p.varcount
    
    # i can parse scoop line by line
    # each "file" is just a code block, within the block i just require
    # that you had the thing defined before
    
    # here's an example of how you could embed this inside python with 
    # strings. kind of cool
    map (p.run, 
    ["# this entire line is a comment!",
     "variable x vector # hello, this is way too cool blah",
     "variable _x vector",
     "parameter A matrix positive",
     "parameter b vector",
     "parameter a scalar",
     "variable t scalar",
     "variable y vector",
     "parameter z scalar",
     "minimize -norm(A*x - b,abs(4*x+b))",
     "subject to",
     "  norm(x,y,z) + -5 <= A*x - -3"
     "#norm(x) <= -3",
     "#a >= 0",
     "#abs(x)+3*x <= t -3",
     "#sqrt(3*x-y) >= abs(z)",
     "#geo_mean(3,1) == sqrt(3*2-b)"])

    # f = p.generate()
    # 
    # g = p.deliteGenerate(Adim=(),,)
    # 
    # f(A,b,a,z)
    # g(A,b,a,z)
    
    print p.symtable
    
    print_prof_data()
