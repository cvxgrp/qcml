import numpy as np
import scipy.sparse as sp
from .. codes.csc_matrix import CSCMap, to_C

# create the i,j,v values of a matrix
# if the entry is numeric, v = 1
# if the entry is a parameter, v = 2, 3, 4, ... etc.
#   by our restricted multiplication and addition, we prevent duplicates from
#   appearing (very important!)
#
# then, create a csc matrix to find the ordering. finally, iterate through v
# *once* (via generators) to yield count, v==params.count_map for each param
# (where "count" is the location of the element in Apr and "count_map" tells
# us which number maps to which parameter key)
#
# then, copying is just
#   dest_map = param_map.(%name);
#   src = param.(%name).pr
#   Apr[*dest_map++] = *src++
#
# requires a many-to-one map, count_map[2-2] gives the "parameter" associated
# with the number (just an array)


# this creates a lookup table which has size "nnz"...

#
# nice thing about doing this in numpy is that i can always accelerate the
# slow code via numba or numexpr
#
params = ['A', 'b', 'c', 'G']

#bwd = {v:k for k,v in fwd.iteritems()}
Apat = sp.coo_matrix((np.ones(2),([1,0],[0,1])))
Gpat = np.ones((2,2))
bpat = np.ones(2)
cpat = np.ones(2)
mymap = [CSCMap('A', [(1,0),(0,1)], True),
        CSCMap('G', [(0,0),(0,1),(1,0),(1,1)], True),
        CSCMap('A', [(1,0),(0,1)]),
        CSCMap('b', [(1,0),(0,0)]),
        CSCMap('G', [(0,0),(0,1),(1,0),(1,1)]),
        CSCMap('c', [(0,0),(1,0)])]
count = 6*[0]
# [eye(2) A G;
#  A     diag(b) 0;
#  G     0      diag(c)]

# constructing this is just a matter of concatenating i,j,v's
# the trick is in setting up "mymap" and the "v" counters
# "mymap" should *not* accept numeric constants, only Param types...
i = np.append(np.arange(6),[0,1,2,3, 0,1,0,1,4,5,4,5])
j = np.append(np.arange(6),[3,2,1,0, 4,4,5,5,0,1,1,0])
v = np.array([1, 1, 5, 5, 7, 7, 2, 2, 4, 4, 3, 3, 3, 3, 6, 6, 6, 6])

F = sp.coo_matrix((v,(i,j))).tocsc()
print F.todense()

for k,ind in enumerate(F.data):
    if ind == 1:
        continue
    else:
        param = mymap[ind-2]
        param.append(k)
        print param.name, k

print map(to_C, mymap)


# TODO: remove above code
##############################################################################
m = [5,6,7]
def check_name(mat, name):
    print mat.cname, name
    assert(mat.cname == name)

def check_map(mat, i, val):
    print mat.mapping[i], val
    assert(mat.mapping[i] == val)

def test_CSCMap():
    A = CSCMap('A', [(0,0),(0,1),(1,0)])
    A.append(m[0])
    A.append(m[1])
    A.append(m[2])

    yield check_name, A, "params.A"
    for i in xrange(3):
        yield check_map, A, i, m[i]

    Atrans = CSCMap('B', [(0,0),(0,1),(1,0)], True)
    Atrans.append(m[0])
    Atrans.append(m[2])
    Atrans.append(m[1])

    yield check_name, Atrans, "params.B"
    for i in xrange(3):
        yield check_map, Atrans, i, m[i]
