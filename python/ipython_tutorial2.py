# -*- coding: utf-8 -*-
"""
Created on Thu Oct  9 13:20:53 2014

@author: ivan
"""

import random
import numpy as np
%precision 3

n = 1000000
x = [random.random() for _ in range(n)]
y = [random.random() for _ in range(n)]

x[:3], y[:3]

z = [x[i] + y[i] for i in range(n)]
z[:3]

%timeit [x[i] + y[i] for i in range(n)]

xa = np.array(x)
ya = np.array(y)

xa[:3]

za = xa + ya
za[:3]

%timeit xa + ya

%timeit sum(x)  # pure Python
%timeit np.sum(xa)  # NumPy

d = [abs(x[i] - y[j]) 
     for i in range(1000) for j in range(1000)]:
d[:3]

da = np.abs(xa[:1000,None] - ya[:1000])
da

%timeit [abs(x[i] - y[j]) for i in range(1000) for j in range(1000)]
%timeit np.abs(xa[:1000, None] - ya[:1000])