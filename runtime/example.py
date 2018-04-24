# -*- coding: utf-8 -*-
"""
Éditeur de Spyder

Ceci est un script temporaire.
"""


import os
os.chdir('/users/michaud/cpp/runtime')
import numpy as np


from cpp import cpp

cpp.loadcpp();

ages = [60, 61, 62, 63, 64, 65, 66, 67, 68];
ben = np.zeros((9,1));
i = 0
for a in ages:
   ben[i] =  cpp.ben(50e3,a,1950);
   i+=1
   
cpp.nra[:] = 67;
benp = np.zeros((9,1));
i = 0
for a in ages:
    benp[i] = cpp.ben(50e3,a,1950);
    i+=1;

# reset
cpp.loadcpp();   

# change replacement rate
cpp.reprate[:] = 0.5;
benr = np.zeros((9,1));
i = 0
for a in ages:
    benr[i] = cpp.ben(50e3,a,1950);
    i+=1;
 
    
t = cpp.tax(2005,50e3,0.0,50,False,False)

cpp.loadcpp();
   