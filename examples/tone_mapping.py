#!/usr/bin/env python
""" This code is from an iPython notebook by AJ Friend
"""
# -*- coding: utf-8 -*-
# <nbformat>3.0</nbformat>

# <markdowncell>

# # Tone mapping with gradient domain compression and convex optimization

# <codecell>

import numpy as np

# <codecell>

#form test image x

n = 20
x = np.zeros((n,n))
bg1 = 1e0
bg2 = 1e1

x[:int(n/2),:] = bg1
x[int(n/2):,:] = bg2
x[int(n/6):int(n/3),round(n/3):int(2*n/3)] = bg1+1
x[int(4*n/6):int(5*n/6),int(n/3):int(2*n/3)] = bg2+1
x.shape = n*n

# <codecell>

#form matrices to extract a subset of pixels from the image
m = n
bottom = np.delete(np.eye(m*n),np.arange(n),0)
top = np.delete(np.eye(m*n),np.arange(m*n-n,m*n),0)
right = np.delete(np.eye(m*n),np.s_[::n],0)
left = np.delete(np.eye(m*n),np.s_[n-1::n],0)

# <codecell>

#form the differences between neighboring pixels in the image
diff_down = top.dot(x) - bottom.dot(x)
diff_right = left.dot(x) - right.dot(x)

s_down = np.sign(diff_down)
diff_down = np.abs(diff_down)
diff_down = s_down*np.log(diff_down+1)
s_down = np.abs(diff_down)
s_down = 1.0/(s_down+1)

s_right = np.sign(diff_right)
diff_right = np.abs(diff_right)
diff_right = s_right*np.log(diff_right+1)
s_right = np.abs(diff_right)
s_right = 1.0/(s_right+1)

#s_down is the scaling we want to associate with each difference. Large differences
#in the original image are given small weight in the objective
s_down = np.diag(s_down)

# <codecell>

from qcml import QCML
import cvxopt

p = QCML()

rows = (m-1)*n
cols = m*n

s = '''
    dimension cols
    dimension rows
    variable y(cols)
    parameter top(rows,cols)
    parameter bottom(rows,cols)
    parameter left(rows,cols)
    parameter right(rows,cols)

    parameter diff_down(rows)
    parameter diff_right(rows)

    parameter s_down(rows,rows)

    y >= 0

    minimize ( sum(square(s_down*(top*y - bottom*y - diff_down))) + sum(square( left*y - bottom*y - diff_right )) )
    '''
p.parse(s)

# <codecell>

sol = p.solve()
