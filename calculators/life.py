
import pandas as pd
import numpy as np
import requests
class table: 
    def __init__(self,prov='qc',scenario='M',gender='males'):
        quotes = pd.read_excel("params/quotients.xlsx",sheet_name='DTH_Tx_Scenario_'+scenario,skiprows=3) 
        provdict = {    
            'tn':10,
            'pe':11,
            'ne':12,
            'nb':13,
            'qc':24,
            'on':35,
            'mb':46,
            'sk':47,
            'ab':48,
            'bc':59,
            'yu':60,
            'to':61,
            'nu':62}
        sexdict = {'males':1,'females':2}    
        quotes = quotes[(quotes.CGT==provdict[prov]) & (quotes.Sexe==sexdict[gender])]
        quotes = quotes.drop(columns=[-1,'110+','CGT'])
        names = list(quotes.columns.values)
        names[0] = 'year'
        names[1] = 'gender'
        quotes.columns = names
        quotes['year'], quotes['temp'] = quotes['year'].str.split('-', 1).str
        quotes = quotes.drop(columns=['temp','gender'])
        quotes.set_index('year')
        # transform quotients into rates
        for a in range(0,110):
            quotes[a] = 2.0*quotes[a]/(2.0-quotes[a])
            quotes[a] = np.where(quotes[a]>1.0,1.0,quotes[a])  
        quotes = quotes.set_index('year')  
        quotes[110] = quotes[109]
        quotes.columns = [str(i) for i in range(0,111)]
        self.prospect = quotes
        self.pull_history()
        self.splice()
        return
    def pull_history(self):    
        # get historical (only quebec for now)
        file = 'http://www.prdh.umontreal.ca/BDLC/data/que/Mx_1x1.txt'
        r = requests.get(file)
        list_r = r.content.decode().split("\n")
        names = list_r[2].split()
        list_r = list_r[3:]
        r = 0
        for line in list_r:
            list_r[r] = line.split()   
            r +=1
        history = pd.DataFrame(data=list_r,index=None,columns=names)
        history.loc[history.Age=='110+','Age']= '110'
        history.dropna(axis=0,inplace=True)        
        history['Year'] = history['Year'].astype('int64')
        history['Age'] = history['Age'].astype('int64')
        names = names[2:]
        for n in names:
            history[n] = np.where(history[n]=='.','1.0',history[n])
            history[n] = history[n].astype('float64',inplace=True)
        gender = 'males'
        if (gender=='males'):
            history = history[['Year','Age','Male']]
        else :
            history = history[['Year','Age','Female']]
        history.columns=['year','age','mx']
        history = pd.pivot_table(history,index=['year'],columns=['age'],values=['mx'])
        history = history['mx']
        history.columns = [str(a) for a in range(0,111)]
        for a in range(0,111):
            history[str(a)] = np.where(history[str(a)]>1.0,1.0,history[str(a)]) 
        self.history = history
        self.history.index = [str(i) for i in range(1921,2012)]
        return
    def splice(self):
        self.history.loc['2012'] = self.history.loc['2011']
        self.tab = self.history.append(self.prospect)
        return
    def get(self,stat,year,age):
        return self.tab.loc[str(year),str(age)]
    def sx(self,byear,agestart,agestop):
        yrstart = byear + agestart
        nyrs = agestop - agestart
        prob = 1.0
        for i in range(0,nyrs):
            if (yrstart+i<2063):
                prob *= (1.0-self.tab.loc[str(yrstart+i),str(agestart+i)])
            else:
                prob *= (1.0-self.tab.loc[str(2062),str(agestart+i)])
        return prob
    def ex(self,byear,agestart):
        n = 1.0
        for i in range(agestart+1,111):
            n += self.sx(byear,agestart,i)  
        return n  
    def ann(self,byear,agestart,rate):
        n = 1.0
        for i in range(agestart+1,111):
            n += self.sx(byear,agestart,i)/((1+rate)**(i - agestart))
        return n 