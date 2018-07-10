import numpy as np 
import operator
from enum import Enum
import numpy as np
import pandas as pd
   
class record:
    def __init__(self,year,earn=0.0,kids=False):
        self.year = year
        self.earn = earn
        self.contrib = 0.0
        self.ownben = 0.0
        self.kids = False
        self.claim = False

class rules:
    def __init__(self):
        bnames = ['year','era','nra','lra']
        ynames = ['year','ympe','exempt','worker','employer','selfemp','arf','drc','nympe','reprate',
            'droprate','worker_qpp','employer_qpp','selfemp_qpp','droprate_qpp','u1','u3','u4',
            'maxcombined60', 'maxcombined65', 'survagecutoff1', 'survagecutoff2', 
			 'survrate1', 'survrate2']
        self.cppyrs = pd.read_csv('../params/cppyear.csv',names=ynames)
        self.cppbyr = pd.read_csv('../params/cppbyear.csv',names=bnames)
    def worktax(self,year):
        return self.cppyrs.loc[year,'worker']
    def empltax(self,year):
        return self.cppyrs.loc[year,'employer']
    def tax(self,year)
        return self.worktax(year)+self.empltax(year)
    def exempt(self,year):
        return self.cppyrs.loc[year,'ssexempt']
    def ypme(self,year):
        return self.cppyrs.loc[year,'ypme']
    def era(self,byear):
        return self.cppbyr.loc[byear,'era']
    def nra(self,byear):
        return self.cppbyr.loc[byear,'nra']
    def lra(self,byear):
        return self.cppbyr.loc[byear,'lra']
    def arf(self,year):
        return self.cppyrs.loc[year,'arf']
    def drc(self,year):
        return self.cppyrs.loc[year,'drc']
    def drop(self,year):
        return self.cppyrs.loc[year,'droprate']

""" 			read(1,*)buffer, ympe(i), base(i),workcrate(i),empcrate(i),selfcrate(i),arf(i),drc(i), &
			 nympe(i), reprate(i),gendroprate(i),workcrate_qpp(i),empcrate_qpp(i),selfcrate_qpp(i),gendroprate_qpp(i), &
			 U1(i),U3(i),U4(i), maxcombined60(i), maxcombined65(i), survagecutoff1(i), survagecutoff2(i), &
			 survrate1(i), survrate2(i) """

class account:
    def __init__(self,byear=None,rules=None):
        self.byear = byear
        self.claimage = None
        self.history = []   
        self.ncontrib = 0    
        self.ape = None
        self.receiving = False
        self.rules = rules
    def MakeContrib(self,year,earn,kids=False):
        years = [self.history[p].year for p in range(self.ncontrib)] 
        if year in years:
            ix = [i for i in self.history if i.year == year] 
            self.history[ix[0]] = record(year,earn,kids)
        else :
            self.history.append(record(year,earn,kids))      
            self.ncontrib +=1
    def ClaimCPP(self,year):
        currage = self.gAge(year)
        if self.claimage!=None:
            print('already claimed at ',self.claimage,' ...')
        else :
            if currage >= self.rules.era(self.byear):
                self.claimage = currage
                self.receiving = True
                self.CalcAPE()
            else :
                print('not yet eligible...')
    def gAge(self,year):
        return year - self.byear
    def gYear(self,age):
        return self.byear + age
    def CalcAPE(self):
        this = 0    



