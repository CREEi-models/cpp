import numpy as np
import operator
from enum import Enum
import pandas as pd
from os import path

class record:
    def __init__(self,year,earn=0.0,contrib=0.0, contrib_s1=0.0, contrib_s2=0.0, kids=False,disab=False):
        self.year = year
        self.earn = earn
        self.contrib = contrib
        self.contrib_s1 = contrib_s1
        self.contrib_s2 = contrib_s2
        self.kids = kids
        self.disab = disab

class rules:
    def __init__(self,qpp=False):
        bnames = ['byear','era','nra','lra']
        ynames = ['year','ympe','exempt','worker','employer','selfemp','arf','drc','nympe','reprate',
            'droprate','pu1','pu2','pu3','pu4','survmax60', 'survmax65', 'survage1', 'survage2',
			 'survrate1', 'survrate2','era','nra','lra','test','supp','disab_rate','disab_base','cola',
                 'ympe_s2','worker_s1','employer_s1','worker_s2','employer_s2','selfemp_s1','selfemp_s2',
                 'reprate_s1', 'reprate_s2']
        self.qpp = qpp
        self.start = 1966
        self.start_s1= 2019
        self.start_s2= 2024
        params = path.join(path.dirname(__file__), 'params')
        if (self.qpp==True):
            self.yrspars = pd.read_excel(params+'/qpp_history.xlsx',names=ynames)
        else :
            self.yrspars = pd.read_excel(params+'/cpp_history.xlsx',names=ynames)
        self.stop  = np.max(self.yrspars['year'].values)
        self.yrspars = self.yrspars.set_index('year')
        self.cpi = 0.02
        self.wgr = 0.03
    def ympe(self,year):
        if (year>self.stop):
            value = self.yrspars.loc[self.stop,'ympe']
            value *= (1.0+self.wgr)**(year-self.stop)
        else:
            value = self.yrspars.loc[year,'ympe']
        return value
    def ympe_s2(self,year):
        value = self.ympe(year)
        value *= self.yrspars.loc[min(year,self.stop),'ympe_s2']
        return value
    def exempt(self,year):
        if (year > self.stop):
            value = self.yrspars.loc[self.stop,'exempt']
        else :
           value = self.yrspars.loc[year,'exempt']
        return value
    def worktax(self,year):
        if (year > self.stop):
            value = self.yrspars.loc[self.stop,'worker']
        else :
           value = self.yrspars.loc[year,'worker']
        return value
    def worktax_s1(self,year):
        if (year > self.stop):
            value = self.yrspars.loc[self.stop,'worker_s1']
        else :
            value = self.yrspars.loc[year,'worker_s1']
        return value
    def worktax_s2(self,year):
        if (year > self.stop):
            value = self.yrspars.loc[self.stop,'worker_s2']
        else :
            value = self.yrspars.loc[year,'worker_s2']
        return value
    def empltax(self,year):
        if (year > self.stop):
            value = self.yrspars.loc[self.stop,'employer']
        else :
           value = self.yrspars.loc[year,'employer']
        return value
    def empltax_s1(self,year):
        if (year > self.stop):
            value = self.yrspars.loc[self.stop,'employer_s1']
        else :
            value = self.yrspars.loc[year,'employer_s1']
        return value
    def empltax_s2(self,year):
        if (year > self.stop):
            value = self.yrspars.loc[self.stop,'employer_s2']
        else :
            value = self.yrspars.loc[year,'employer_s2']
        return value
    def tax(self,year):
        return self.worktax(year)+self.empltax(year)
    def tax_s1(self,year):
        return self.worktax_s1(year)+self.empltax_s1(year)
    def tax_s2(self,year):
        return self.worktax_s2(year)+self.empltax_s2(year)
    def selftax(self,year):
        if (year > self.stop):
            value = self.yrspars.loc[self.stop,'selfemp']
        else :
            value = self.yrspars.loc[year,'selfemp']
        return value
    def selftax_s1(self,year):
        if (year > self.stop):
            value = self.yrspars.loc[self.stop,'selfemp_s1']
        else :
            value = self.yrspars.loc[year,'selfemp_s1']
        return value
    def selftax_s2(self,year):
        if (year > self.stop):
            value = self.yrspars.loc[self.stop,'selfemp_s2']
        else :
            value = self.yrspars.loc[year,'selfemp_s2']
        return value
    def arf(self,year):
        if (year > self.stop):
            value = self.yrspars.loc[self.stop,'arf']
        else :
            value = self.yrspars.loc[year,'arf']
        return value
    def drc(self,year):
        if (year > self.stop):
            value = self.yrspars.loc[self.stop,'drc']
        else :
            value = self.yrspars.loc[year,'drc']
        return value
    def nympe(self,year):
        if (year > self.stop):
            value = self.yrspars.loc[self.stop,'nympe']
        else :
            value = self.yrspars.loc[year,'nympe']
        return value
    def reprate(self,year):
        if (year > self.stop):
            value = self.yrspars.loc[self.stop,'reprate']
        else :
            value = self.yrspars.loc[year,'reprate']
        return value
    def reprate_s1(self,year):
        if (year > self.stop):
            value = self.yrspars.loc[self.stop,'reprate_s1']
        else :
            value = self.yrspars.loc[year,'reprate_s1']
        return value
    def reprate_s2(self,year):
        if (year > self.stop):
            value = self.yrspars.loc[self.stop,'reprate_s2']
        else :
            value = self.yrspars.loc[year,'reprate_s2']
        return value
    def droprate(self,year):
        if (year > self.stop):
            value = self.yrspars.loc[self.stop,'droprate']
        else :
            value = self.yrspars.loc[year,'droprate']
        return value
    def pu1(self,year):
        if year > self.stop :
            yr = self.stop
        else:
            yr = year
        return self.yrspars.loc[yr,'pu1']
    def pu2(self,year):
        if year > self.stop :
            yr = self.stop
        else:
            yr = year
        return self.yrspars.loc[yr,'pu2']
    def pu3(self,year):
        if year > self.stop :
            yr = self.stop
        else:
            yr = year
        return self.yrspars.loc[yr,'pu3']
    def pu4(self,year):
        if year > self.stop :
            yr = self.stop
        else:
            yr = year
        return self.yrspars.loc[yr,'pu4']
    def survmax60(self,year):
        if year > self.stop :
            yr = self.stop
        else:
            yr = year
        return self.yrspars.loc[yr,'survmax60']
    def survmax65(self,year):
        if year > self.stop :
            yr = self.stop
        else:
            yr = year

        return self.yrspars.loc[yr,'survmax65']
    def survage1(self,year):
        if year > self.stop :
            yr = self.stop
        else:
            yr = year
        return self.yrspars.loc[yr,'survage1']
    def survage2(self,year):
        if year > self.stop :
            yr = self.stop
        else:
            yr = year
        return self.yrspars.loc[yr,'survage2']
    def survrate1(self,year):
        if year > self.stop :
            yr = self.stop
        else:
            yr = year
        return self.yrspars.loc[yr,'survrate1']
    def survrate2(self,year):
        if year > self.stop :
            yr = self.stop
        else:
            yr = year
        return self.yrspars.loc[yr,'survrate2']
    def era(self,year):
        if year > self.stop :
            yr = self.stop
        else:
            yr = year
        return self.yrspars.loc[yr,'era']
    def nra(self,year):
        if year > self.stop :
            yr = self.stop
        else:
            yr = year
        return self.yrspars.loc[yr,'nra']
    def test(self,year):
        if year > self.stop :
            yr = self.stop
        else:
            yr = year
        return self.yrspars.loc[yr,'test']
    def supp(self,year):
        if year > self.stop :
            yr = self.stop
        else:
            yr = year
        return self.yrspars.loc[yr,'supp']
    def disab_rate(self,year):
        if year > self.stop :
            yr = self.stop
        else:
            yr = year
        return self.yrspars.loc[yr,'disab_rate']
    def disab_base(self,year):
        if year > self.stop :
            yr = self.stop
        else:
            yr = year
        return self.yrspars.loc[yr,'disab_base']
    def cola(self,year):
        if year > self.stop :
            yr = self.stop
        else:
            yr = year
        return self.yrspars.loc[yr,'cola']
    def chgpar(self,name,y0,y1,value):
        if (name in self.yrspars.columns):
            for i in range(y0,y1+1):
                self.yrspars.loc[i,name] = value
        else :
            for i in range(y0,y1+1):
                self.byrpars.loc[i,name] = value

class account:
    def __init__(self,byear,rules=None):
        self.byear = byear
        self.claimage = None
        self.history = [record(yr) for yr in range(self.byear+18,self.byear+70,1)]
        self.ncontrib = 0
        self.ncontrib_s1 = 0
        self.ncontrib_s2 = 0
        self.ampe = 0.0
        self.ampe_s1 = 0.0
        self.ampe_s2 =0.0
        self.receiving = False
        self.rules = rules
        self.benefit = 0.0
        self.benefit_s1 = 0.0
        self.benefit_s2 = 0.0
        self.prb = [0 for x in range(10)]

    def MakeContrib(self,year,earn,kids=False):
        if year>=self.rules.start:
            taxable = np.min([earn,self.rules.ympe(year)])
            if taxable > self.rules.exempt(year):
                taxable -= self.rules.exempt(year)
            else :
                taxable = 0.0
            contrib = self.rules.worktax(year) * taxable
            years = [self.history[p].year for p in range(self.ncontrib)]

            taxable_s2 = np.min([np.max([earn-self.rules.ympe(year),0.0]),(self.rules.ympe_s2(year)-1)*self.rules.ympe(year)])
            contrib_s2 = self.rules.worktax_s2(year) * taxable_s2
            index = self.gAge(year)-18
            self.history[index]= record(year,earn=earn,contrib = contrib,contrib_s2=contrib_s2,kids=kids)
            if self.claimage!=None:
                self.CalcPRB(year,taxable,taxable_s2,earn)
            
    def ClaimCPP(self,year):
        currage = self.gAge(year)
        if self.claimage!=None:
           print('already claimed at ',self.claimage,' ...')
        else :
            if currage >= self.rules.era(year):
                self.ncontrib = np.min([currage - 18,year-1966])
                self.ncontrib_s1 = np.max([np.min([currage - 18,year-self.rules.start_s1]),0])
                self.ncontrib_s2 = np.max([np.min([currage - 18,year-self.rules.start_s2]),0])
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
        yr18 = np.max([self.gYear(18),self.rules.start])
        yr70 = np.min([self.gYear(70),year])
        nyrs = yr70-yr18
        yr18_s2 = np.max([self.gYear(18),self.rules.start_s2])
        nyrs_s2 = [np.max([(yr70-yr18_s2),0])]
        index = np.max([self.gAge(1966)-18,0])
        yrs = [self.history[p].year for p in range(index,index+self.ncontrib)]
        yrs_s2 = [self.history[p].year for p in range(index,index+self.ncontrib_s2)]
        ympe = [self.rules.ympe(i) for i in yrs]
        ympe_s2 = [self.rules.ympe_s2(i) for i in yrs]
        worktax_s1 = [self.rules.worktax_s1(i) for i in yrs]
        exempt = [self.rules.exempt(i) for i in yrs]
        kids = [self.history[p].kids for p in range(index,index+self.ncontrib)]
        disab = [self.history[p].disab for p in range(index,index+self.ncontrib)]
        earn = [self.history[p].earn for p in range(index,index+self.ncontrib)]
        nympe = self.rules.nympe(year)
        # unadjusted pensionable earnings
        self.upe = [np.min([earn[i],ympe[i]]) for i in range(self.ncontrib)]
        self.upe = [np.where(self.upe[i]<exempt[i],0.0,self.upe[i]) for i in range(self.ncontrib)]
        #upe_s2 Need to start only in 2024
        self.upe_s2 = [np.max([np.min([earn[i]-ympe[i],ympe_s2[i]-ympe[i]]),0.0]) for i in range(self.ncontrib)]

        # average ympe last 5 years
        avgympe = np.mean([self.rules.ympe(i) for i in range(year-nympe+1,year+1)])
        # compute ape
        ape = [self.upe[i]/ympe[i]*avgympe for i in range(self.ncontrib)]
        ape_s1 = [self.upe[i]/ympe[i] * avgympe * worktax_s1[i]*100 for i in range(self.ncontrib)]
        ape_s2 = [self.upe_s2[i]/ympe[i]*avgympe for i in range(self.ncontrib)]
        # need provision for disability
        ndrop = 0
        dropped = np.full(self.ncontrib, False)
        for i in range(self.ncontrib):
            if (self.upe[i]==0.0 and disab[i]==True):
                dropped[i] = True
                ndrop +=1

        ndrop_s1 = 0
        dropped_s1 = np.full(self.ncontrib, False)
        for i in range(self.ncontrib):
            if year>=self.rules.start_s1 and (self.upe[i]==0.0 and disab[i]==True):
                dropped_s1[i] = True
                ndrop_s1 +=1

        ndrop_s2 = 0
        dropped_s2 = np.full(self.ncontrib, False)
        for i in range(self.ncontrib):
            if year>=self.rules.start_s2 and (self.upe_s2[i]==0.0 and disab[i]==True):
                dropped_s2[i] = True
                ndrop_s2 +=1


        # dropout years for childrearing (CRD01)
        ndrop = 0
        dropped = np.full(self.ncontrib, False)
        for i in range(self.ncontrib):
            if (self.upe[i]==0.0 and kids[i]==True):
                dropped[i] = True
                ndrop +=1
        # compute average ape
        avgape = np.sum(ape)/(nyrs - ndrop)
        avgape_s2 = np.sum(ape_s2)/40
        # Child rearing provision (CRD02)
        for i in range(self.ncontrib):
            if (ape[i]<avgape and kids[i]==True):
                ape[i] = 0.0
                dropped[i] = True
                ndrop +=1
        # need add provision working past 65

        # General dropout
        gdrop = int(np.ceil(self.rules.droprate(year)*(self.ncontrib - ndrop)))
        apef = [ape[i] for i in range(self.ncontrib) if dropped[i]==False]
        ixf = np.asarray(apef).argsort()[0:gdrop]
        yrsf  = [yrs[i] for i in range(self.ncontrib) if dropped[i]==False]
        yrstodrop = [yrsf[i] for i in ixf]
        for i in range(self.ncontrib):
            if (yrs[i] in yrstodrop and gdrop!=0):
                ape[i] = 0
                dropped[i] = True
                ndrop +=1
                gdrop -=1
        self.ampe = (1/12)*np.sum(ape)/(nyrs - ndrop)

        # Revoir indices car le code ne fait pas encore qu'il devrait faire
        # par grave pour le moment car par défaut
        # dgrop_s1 = 0 avant 2059
        # dgrop_s2 = 0 avant 2064

        gdrop_s1 = int(np.max([self.ncontrib_s1-40,0.0]))
        apef_s1 = [ape_s1[i] for i in range(self.ncontrib) if dropped_s1[i]==False]
        ixf_s1 = np.asarray(apef_s1).argsort()[0:gdrop_s1]
        yrsf_s1  = [yrs[i] for i in range(self.ncontrib) if dropped_s1[i]==False]
        yrstodrop_s1 = [yrsf_s1[i] for i in ixf_s1]
        for i in range(self.ncontrib):
            if (yrs[i] in yrstodrop_s1 and gdrop_s1!=0):
                ape_s1[i] = 0
                dropped_s1[i] = True
                ndrop_s1 +=1
                gdrop_s1 -=1
        self.ampe_s1 = (1/12)*np.sum(ape_s1)/40

        gdrop_s2 = int(np.max([self.ncontrib_s2-40,0.0]))
        apef_s2 = [ape_s2[i] for i in range(self.ncontrib) if dropped_s2[i]==False]
        ixf_s2 = np.asarray(apef_s2).argsort()[0:gdrop_s2]
        yrsf_s2  = [yrs[i] for i in range(self.ncontrib) if dropped_s2[i]==False]
        yrstodrop_s2 = [yrsf_s2[i] for i in ixf_s2]

        for i in range(self.ncontrib):
            if (yrs[i] in yrstodrop_s2 and gdrop_s2!=0):
                ape_s2[i] = 0
                dropped_s2[i] = True
                ndrop_s2 +=1
                gdrop_s2 -=1
        self.ampe_s2 = (1/12)*np.sum(ape_s2)/40

    def CalcBenefit(self,year):
        if self.receiving==True:
            if (self.gAge(year)==self.claimage):
                nra = self.rules.nra(year)
                arf = self.rules.arf(year)
                drc = self.rules.drc(year)
                age = self.gAge(year)
                self.benefit = self.rules.reprate(year) * self.ampe
                self.benefit_s1 = self.rules.reprate_s1(year) * self.ampe_s1
                self.benefit_s2 = self.rules.reprate_s2(year) * self.ampe_s2
                if (age<nra):
                    self.benefit *= 1.0+arf*(age-nra)
                    self.benefit_s1 *= 1.0+arf*(age-nra)
                    self.benefit_s2 *= 1.0+arf*(age-nra)
                else :
                    self.benefit *= 1.0+drc*(age-nra)
                    self.benefit_s1 *= 1.0+drc*(age-nra)
                    self.benefit_s2 *= 1.0+drc*(age-nra)
        else:
            self.benefit = 0.0
            self.benefit_s1 = 0.0
            self.benefit_s2 = 0.0
    def CalcPRB(self,year,taxable,taxable_s2,earn):
        if self.rules.qpp:
            if year>=2014:
                prb = self.prb[self.gAge(year)-60]+taxable*(0.005+self.rules.worktax_s1(year)*100*0.0016)
                self.prb[self.gAge(year)-60+1] = prb + taxable_s2*0.0066
        else:
            if year>=2013 & self.gAge(year)<70:
                nra = self.rules.nra(year)
                arf = self.rules.arf(year)
                drc = self.rules.drc(year)
                age = self.gAge(year)
                upe = np.min([earn,self.rules.ympe(year)]) 
                if upe<self.rules.exempt(year) : upe = 0
                #upe_s2 Need to start only in 2024
                upe_s2 = np.max([np.min([earn-self.rules.ympe(year),self.rules.ympe_s2(year)-self.rules.ympe(year)]),0.0])
                #PRB base + PRB S1
                prb = upe/self.rules.ympe(year) * self.rules.ympe(year+1)*(0.00625+self.rules.worktax_s1(year)*100*0.00208)
                #PRB S2
                prb = prb + upe_s2/(self.rules.ympe_s2(year)-self.rules.ympe(year))*(self.rules.ympe_s2(year+1)-self.rules.ympe(year+1)) * 0.00833
                #Ajustment factor
                if (age<nra):
                    self.prb[self.gAge(year)-60+1] = self.prb[self.gAge(year)-60] + (1.0+arf*(age-nra)) * prb
                else :
                    self.prb[self.gAge(year)-60+1] = self.prb[self.gAge(year)-60] + (1.0+drc*(age-nra)) * prb
    def RunCase(self,claimage=65):
        yr18 = np.max([self.gYear(18),self.rules.start])
        start_age = self.gAge(yr18)
        for a in range(start_age,self.retage):
            if a == claimage:
                self.ClaimCPP(self.gYear(claimage))
            yr = self.gYear(a)
            self.MakeContrib(yr,earn=self.rules.ympe(yr)*self.ratio_list[a-start_age], kids = self.kids_list[a-start_age])
        if self.retage < claimage :
            for a in range(self.retage,claimage):
                yr = self.gYear(a)
                self.MakeContrib(yr,earn=0)
        if self.claimage==None: self.ClaimCPP(self.gYear(claimage))
        return

    def SetHistory_ratio(self,retage=60, **kwargs):
        self.retage = retage
        yr18 = np.max([self.gYear(18),self.rules.start])
        start_age = self.gAge(yr18)
        nyears = self.retage - start_age
        self.ratio_list = [1]*nyears
        nargs = len(kwargs)
        niter=0
        for key,val in kwargs.items():
            temp_list= [x for x in val.values()]
            for i in np.arange(niter,niter+temp_list[1]):
                self.ratio_list[i] = temp_list[0]
                niter += 1
        return
    
    def SetHistory_fam(self, claimage = 65, age_birth=[]):
        self.age_birth = age_birth       
        yr18 = np.max([self.gYear(18),self.rules.start])
        start_age = self.gAge(yr18)
        nyears = claimage - start_age
        self.kids_list = [False]*nyears
        for x in range(len(age_birth)):
            indice = age_birth[x] - start_age
            if age_birth[x]>=start_age and age_birth[x]<= 50:
                for yr in range(7):
                    self.kids_list[indice+yr] = True
            if age_birth[x]>=start_age and age_birth[x]>50:
                print("Please check age at birth")
            else :
                years_deduc = 7 - (start_age - age_birth[x])
                for yr in range(years_deduc):
                    self.kids_list[yr] = True


    def ResetCase(self):
        self.claimage = None
        self.history = [record(yr) for yr in range(self.byear+18,self.byear+70,1)]
        self.ncontrib = 0
        self.ampe = 0.0
        self.receiving = False
        self.benefit = 0.0
