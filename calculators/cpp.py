import numpy as np 
import operator
from enum import Enum
import numpy as np
import pandas as pd
   
class record:
    def __init__(self,year,claim=False,earn=0.0,contrib=0.0,ownben=0.0,kids=False,disab=False):
        self.year = year
        self.earn = earn
        self.contrib = contrib
        self.ownben = ownben
        self.kids = kids
        self.claim = claim
        self.disab = disab

class rules:
    def __init__(self,qpp=False):
        bnames = ['byear','era','nra','lra']
        ynames = ['year','ympe','exempt','worker','employer','selfemp','arf','drc','nympe','reprate',
            'droprate','ape1','ape3','ape4','survmax60', 'survmax65', 'survage1', 'survage2', 
			 'survrate1', 'survrate2']
        self.qpp = qpp
        if (self.qpp==True):     
            self.yrspars = pd.read_csv('params/qppyear.csv',names=ynames,delimiter=';')
            self.byrpars = pd.read_csv('params/qppbyear.csv',names=bnames,delimiter=';')
        else :
            self.yrspars = pd.read_csv('params/cppyear.csv',names=ynames,delimiter=';')
            self.byrpars = pd.read_csv('params/cppbyear.csv',names=bnames,delimiter=';')
        self.yrspars = self.yrspars.set_index('year')
        self.byrpars = self.byrpars.set_index('byear')
    def ympe(self,year):
        return self.yrspars.loc[year,'ympe']
    def exempt(self,year):
        return self.yrspars.loc[year,'exempt']
    def worktax(self,year):
        return self.yrspars.loc[year,'worker']
    def empltax(self,year):
        return self.yrspars.loc[year,'employer']
    def tax(self,year):
        return self.worktax(year)+self.empltax(year)
    def selftax(self,year):
        return self.yrspars.loc[year,'selfemp']
    def arf(self,year):
        return self.yrspars.loc[year,'arf']
    def drc(self,year):
        return self.yrspars.loc[year,'drc']
    def nympe(self,year):
        return self.yrspars.loc[year,'nympe']
    def reprate(self,year):
        return self.yrspars.loc[year,'reprate']
    def droprate(self,year):
        return self.yrspars.loc[year,'droprate']
    def ape1(self,year):
        return self.yrspars.loc[year,'ape1']
    def ape3(self,year):
        return self.yrspars.loc[year,'ape3']
    def ape4(self,year):
        return self.yrspars.loc[year,'ape4']
    def survmax60(self,year):
        return self.yrspars.loc[year,'survmax60']
    def survmax65(self,year):
        return self.yrspars.loc[year,'survmax65']
    def survage1(self,year):
        return self.yrspars.loc[year,'survage1']
    def survage2(self,year):
        return self.yrspars.loc[year,'survage2']
    def survrate1(self,year):
        return self.yrspars.loc[year,'survrate1']
    def survrate2(self,year):
        return self.yrspars.loc[year,'survrate2']
    def era(self,byear):
        return self.byrpars.loc[byear,'era']
    def nra(self,byear):
        return self.byrpars.loc[byear,'nra']
    def lra(self,byear):
        return self.byrpars.loc[byear,'lra']

class account:
    def __init__(self,byear=None,rules=None):
        self.byear = byear
        self.claimage = None
        self.history = []   
        self.nrecord = 0  
        self.ncontrib = 0  
        self.ape = None
        self.receiving = False
        self.rules = rules
        self.benefit = 0.0
    def MakeContrib(self,year,earn,kids=False):
        taxable = np.min([earn,self.rules.ympe(year)])
        contrib = self.rules.worktax(year) * taxable 
        years = [self.history[p].year for p in range(self.nrecord)] 
        if year in years:
            ix = [i for i in self.history if (i.year == year) and (i.claim == False)] 
            self.history[ix[0]] = record(year,earn=earn,contrib = contrib,kids=kids)
        else :
            self.history.append(record(year,earn=earn,contrib = contrib,kids=kids))      
            self.nrecord +=1
            self.ncontrib +=1
    def ClaimCPP(self,year):
        currage = self.gAge(year)
        if self.claimage!=None:
            print('already claimed at ',self.claimage,' ...')
        else :
            if currage >= self.rules.era(self.byear):
                self.claimage = currage
                self.receiving = True
                self.CalcAMPE(year)
                self.CalcBenefit(year)
    
            else :
                print('not yet eligible...')
    def gAge(self,year):
        return year - self.byear
    def gYear(self,age):
        return self.byear + age
    def CalcAMPE(self,year):
        # parameters
        yr18 = np.max([self.gYear(18),1966])
        yr70 = np.min([self.gYear(70),year])
        nyrs = yr70-yr18+1
        yrs = [self.history[p].year for p in range(self.ncontrib)]
        ympe = [self.rules.ympe(i) for i in yrs]
        exempt = [self.rules.exempt(i) for i in yrs]
        kids = [self.history[p].kids for p in range(self.ncontrib)]
        earn = [self.history[p].earn for p in range(self.ncontrib)]
        nympe = self.rules.nympe(year)
        # unadjusted pensionable earnings
        upe = [np.min([earn[i],ympe[i]]) for i in range(self.ncontrib)]
        upe = [np.where(upe[i]<exempt[i],0.0,upe[i]) for i in range(self.ncontrib)]
        # average ympe last 5 years
        avgympe = np.mean([self.rules.ympe(i) for i in range(year-nympe+1,year+1)])
        # compute ape
        ape = [upe[i]/ympe[i]*avgympe for i in range(self.ncontrib)]
        # dropout years for childrearing (CRD01)
        ndrop = 0
        dropped = np.full(self.ncontrib, False)
        for i in range(self.ncontrib):
            if (upe[i]==0.0 and kids[i]==True):
                dropped[i] = True
                ndrop +=1
        # compute average ape
        avgape = np.sum(ape)/(nyrs - ndrop)
        # Child rearing provision (CRD02)
        for i in range(self.ncontrib):
            if (ape[i]<avgape and kids[i]==True):
                ape[i] = 0.0
                dropped[i] = True
                ndrop +=1
        # General dropout
        gdrop = np.ceil(self.rules.droprate(year)*(self.ncontrib - ndrop))
        ape.sort()
        for i in range(self.ncontrib):
            if (dropped[i]==False and gdrop>0):
                ape[i] = 0.0
                gdrop -=1
                ndrop +=1
        self.ampe = (1/12)*np.sum(ape)/(nyrs - ndrop)


    def CalcBenefit(self,year):
        if self.receiving==True:
            if (self.gAge(year)==self.claimage):
                nra = self.rules.nra(self.byear)
                arf = self.rules.arf(year)
                drc = self.rules.drc(year)
                age = self.gAge(year)
                self.benefit = self.rules.reprate(year) * self.ampe 
                if (age<nra):
                    self.benefit *= 1.0+arf*(age-nra)
                else :
                    self.benefit *= 1.0+drc*(age-nra)                       
        else:
            self.benefit = 0.0
    def PayBenefit(self,year):
        if self.receiving==True:
            self.CalcBenefit(year)
            self.history.append(record(year,claim=True,ownben=self.benefit)) 
            self.nrecord +=1           



