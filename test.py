import cpp
import importlib
import pandas as pd
import numpy as np
yrspars = cpp.load_rules(qpp=False)
r=cpp.rules(yrspars,qpp=False)
case = cpp.account(byear=1970,rules=r) 
#case.SetHistory_fam(claimage=65,age_birth=[])
#case.SetHistory_ratio(retage=65,a={"r":1.5,'n':47})
list_contrib = np.array([1.4 for x in range(65-18)])
case.RunCase(60,65,np.array([1.4 for x in range(65-18)]))  
print(case.gPRB(2040))
print(case.gPRB_s1(2040))
print(case.gPRB_s2(2040))