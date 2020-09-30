import numpy as np
import operator
from enum import Enum
import pandas as pd
from os import path

class record:
    """
    Cette classe permet d'enregistrer une année d'information.

    Les instances de cette classe sont regroupées dans la liste *account.history*. Habituellement l'utilisateur utilisera la fonction de méthode *MakeContrib* de la classe *account* (voir ci-dessous) pour créer des entrées.

    Parameters
    __________
    year : int
        année de l'entrée
    earn : float
        gains admissibles
    contrib : float
        cotisation au régime de base
    contrib_s1 : float
        cotisation au régime supplémentaire 1 (en lien avec la hausse du taux de remplacement)
    contrib_s2 : float
        cotisation au régime supplémentaire 2 (en lien avec la hausse du salaire admissible à 114% du MGA pré-réforme)
    kids : boolean
        présence d'enfants de moins de 7 ans (True seulement pour la personne admissible à l'exemption pour soins aux enfants en bas âge); défaut=False
    disab : boolean
        rente d'invalidité (non modélisée actuellement) si True, rente de retraite si False; défaut=False
    """
    def __init__(self,year,earn=0.0,contrib=0.0, contrib_s1=0.0, contrib_s2=0.0,
                      contrib_aut=0,contrib_aut_s1=0,contrib_aut_s2=0,kids=False,disab=False):
        self.year = year
        self.earn = earn
        self.contrib = contrib
        self.contrib_s1 = contrib_s1
        self.contrib_s2 = contrib_s2
        self.contrib_aut = contrib_aut
        self.contrib_aut_s1 = contrib_aut_s1
        self.contrib_aut_s2 = contrib_aut_s2
        self.kids = kids
        self.disab = disab

class rules:
    """
    Classe contenant les règles du RPC et du RRQ.

    Une instance de cette classe charge soit les pamamètres pour le RRQ (qpp=True) ou pour le RPC (qpp=False). Les paramètres jusqu'en 2025 sont dans params/qpp_history.xlsx et params/cpp_history.xlsx. Passé 2025 les différentes fonctions de méthodes renvoient soit la valeur de 2025 ou la valeur actualisée, selon le paramètre. En général, pour accéder à un paramètre pour une année donnée il suffit d'appeler la fonction de méthode *rules.<param>(year)*.

    On note que tous les taux de cotisation sont modélisés séparément, car les différentes composantes des cotisations sont traitées de façon différente sur le plan fiscal.

    Parameters
    __________
    qpp : boolean
        True pour charger les paramètres du RRQ, False pour charger ceux du RPC; défaut=True

    Attributes
    __________
    cpi : float
        taux annuel d'indexation des prestations
    wgr : float
        taux annuel d'augmentation du MGA
    lastyear : int
        lastyear + 1: année à partir de laquelle *rules.cpi* et *rules.wgr* sont utilisés à la place des valeurs du fichier de paramètres
    self.indexation : float(NxN)
        tableau pré-calculé d'indexation des prestations de 1966 à 2100
    """
    def __init__(self,qpp=False):
        bnames = ['byear','era','nra','lra']
        ynames = ['year','ympe','exempt','worker','employer','selfemp','ca','arf','drc','nympe','reprate',
            'droprate','pu1','pu2','pu3','pu4','survmax60', 'survmax65', 'survage1', 'survage2',
			 'survrate1', 'survrate2','era','nra','lra','supp','disab_rate','disab_base','cola',
                 'ympe_s2','worker_s1','employer_s1','worker_s2','employer_s2','selfemp_s1','selfemp_s2',
                 'reprate_s1', 'reprate_s2','supp_s1','supp_s2']
        self.qpp = qpp
        self.start = 1966
        self.start_s1= 2019
        self.start_s2= 2024
        params = path.join(path.dirname(__file__), 'params')
        if (self.qpp==True):
            self.yrspars = pd.read_excel(params+'/qpp_history.xlsx',names=ynames)
        else :
            #No ca column in cpp
            for i,name in enumerate(ynames):
                if name == "ca":
                    ynames.pop(i)
            self.yrspars = pd.read_excel(params+'/cpp_history.xlsx',names=ynames)
        self.stop  = np.max(self.yrspars['year'].values)
        self.yrspars = self.yrspars.set_index('year')
        self.cpi = 0.02
        self.wgr = 0.03
        self.lastyear = 2019
        self.indexation = np.ones((2100-1966,2100-1966))
        ones_lower = np.tril(self.indexation)
        for y in range(2100-1966):
            self.indexation[:,y] =  self.indexation[:,y] + self.cola(1966+y)
        self.indexation = np.cumprod((np.triu(self.indexation)-np.diag(np.diag(self.indexation))+ones_lower), axis=1)

    def ympe(self,year):
        """
        MGA de base pour une année donnée.

        Par défaut, indexé à *wgr* pour les années après 2025.

        Parameters
        __________
        year : int
            année du paramètre
        Return
        ______
        float
            Montant du MGA de base.
        """
        if (year>self.lastyear):
            value = self.yrspars.loc[self.lastyear,'ympe']
            value *= (1.0+self.wgr)**(year-self.lastyear)
        else:
            value = self.yrspars.loc[year,'ympe']
        return value
    def ympe_s2(self,year):
        """
        Montant du MGA pour le régime supplémentaire pour une année donnée (0 avant 2024, indexé à *wgr* après 2025).

        Parameters
        __________
        year : int
            année du paramètre
        Return
        ______
        float
             Montant du MGA pour le régime supplémentaire.
        """
        value = self.ympe(year)
        value *= self.yrspars.loc[min(year,self.stop),'ympe_s2']
        return value
    def exempt(self,year):
        """
        Montant de l'exemption sur les gains cotisables.

        Après 2025, maintenue fixe à sa valeur nominale de 2025.

        Parameters
        __________
        year : int
            année du paramètre
        Return
        ______
        float
            Montant de l'exemption.

        """
        if (year > self.stop):
            value = self.yrspars.loc[self.stop,'exempt']
        else :
           value = self.yrspars.loc[year,'exempt']
        return value
    def worktax(self,year):
        """
        Taux de cotisation de l'employé au régime de base. Fixe après 2025.

        Parameters
        __________
        year : int
            année du paramètre
        Return
        ______
        float
            Taux de cotisation.

        """

        if (year > self.stop):
            value = self.yrspars.loc[self.stop,'worker']
        else :
           value = self.yrspars.loc[year,'worker']
        return value
    def worktax_s1(self,year):
        """
        Taux de cotisation de l'employé au régime supplémentaire 1.

        Fixe après 2025.

        Parameters
        __________
        year : int
            année du paramètre
        Return
        ______
        float
            Taux de cotisation.

        """
        if (year > self.stop):
            value = self.yrspars.loc[self.stop,'worker_s1']
        else :
            value = self.yrspars.loc[year,'worker_s1']
        return value
    def worktax_s2(self,year):
        """
        Taux de cotisation de l'employé au régime supplémentaire 1.

        Fixe après 2025.

        Parameters
        __________
        year : int
            année du paramètre
        Return
        ______
        float
            Taux de cotisation.

        """
        if (year > self.stop):
            value = self.yrspars.loc[self.stop,'worker_s2']
        else :
            value = self.yrspars.loc[year,'worker_s2']
        return value
    def empltax(self,year):
        """
        Taux de cotisation de l'employeur au régime de base.

        Fixe après 2025.

        Parameters
        __________
        year : int
            année du paramètre
        Return
        ______
        float
            Taux de cotisation.

        """
        if (year > self.stop):
            value = self.yrspars.loc[self.stop,'employer']
        else :
           value = self.yrspars.loc[year,'employer']
        return value
    def empltax_s1(self,year):
        """
        Taux de cotisation de l'employeur au régime supplémentaire 1.

        Fixe après 2025.

        Parameters
        __________
        year : int
            année du paramètre
        Return
        ______
        float
            Taux de cotisation.

        """
        if (year > self.stop):
            value = self.yrspars.loc[self.stop,'employer_s1']
        else :
            value = self.yrspars.loc[year,'employer_s1']
        return value
    def empltax_s2(self,year):
        """
        Taux de cotisation de l'employeur au régime supplémentaire 2.

        Fixe après 2025.

        Parameters
        __________
        year : int
            année du paramètre
        Return
        ______
        float
            Taux de cotisation.
        """
        if (year > self.stop):
            value = self.yrspars.loc[self.stop,'employer_s2']
        else :
            value = self.yrspars.loc[year,'employer_s2']
        return value
    # def tax(self,year):
    #     """
    #     Taux de cotisation combiné de l'employeur et de l'employé au régime de base.
    #
    #     Parameters
    #     __________
    #     year : int
    #         année du paramètre
    #     Return
    #     ______
    #     float
    #         Taux de cotisation.
    #     """
    #     return self.worktax(year)+self.empltax(year)
    # def tax_s1(self,year):
    #     """
    #     Taux de cotisation combiné de l'employeur et de l'employé au régime supplémentaire 1.
    #
    #     Parameters
    #     __________
    #     year : int
    #         année du paramètre
    #     Return
    #     ______
    #     float
    #         Taux de cotisation.
    #     """
    #     return self.worktax_s1(year)+self.empltax_s1(year)
    # def tax_s2(self,year):
    #     """
    #     Taux de cotisation combiné de l'employeur et de l'employé au régime supplémentaire 2.
    #
    #     Parameters
    #     __________
    #     year : int
    #         année du paramètre
    #     Return
    #     ______
    #     float
    #         Taux de cotisation.
    #     """
    #     return self.worktax_s2(year)+self.empltax_s2(year)
    def selftax(self,year):
        """
        Taux de cotisation sur revenun de travail autonome au régime de base.

        Parameters
        __________
        year : int
            année du paramètre
        Return
        ______
        float
            Taux de cotisation.
        """
        if (year > self.stop):
            value = self.yrspars.loc[self.stop,'selfemp']
        else :
            value = self.yrspars.loc[year,'selfemp']
        return value
    def selftax_s1(self,year):
        """
        Taux de cotisation sur revenun de travail autonome au régime supplémentaire 1.

        Parameters
        __________
        year : int
            année du paramètre
        Return
        ______
        float
            Taux de cotisation.
        """
        if (year > self.stop):
            value = self.yrspars.loc[self.stop,'selfemp_s1']
        else :
            value = self.yrspars.loc[year,'selfemp_s1']
        return value
    def selftax_s2(self,year):
        """
        Taux de cotisation sur revenun de travail autonome au régime supplémentaire 2.

        Parameters
        __________
        year : int
            année du paramètre
        Return
        ______
        float
            Taux de cotisation.
        """
        if (year > self.stop):
            value = self.yrspars.loc[self.stop,'selfemp_s2']
        else :
            value = self.yrspars.loc[year,'selfemp_s2']
        return value
    def ca(self,year):
        """
        Dans le RRQ, facteur d'ajustement en fonction du revenu de la réduction pour rente hâtive (débutant avant 65 ans).

        Parameters
        __________
        year : int
            année du paramètre
        Return
        ______
        float
            Facteur d'ajustement.
        """
        if self.qpp:
            if (year > self.stop):
                value = self.yrspars.loc[self.stop,'ca']
            else :
                value = self.yrspars.loc[year,'ca']
        else:
            value = 0.0
        return value
    def arf(self,year):
        """
        Taux de réduction des prestations pour un début avant 65 ans.

        Parameters
        __________
        year : int
            année du paramètre
        Return
        ______
        float
            Taux de réduction.
        """
        if (year > self.stop):
            value = self.yrspars.loc[self.stop,'arf']
        else :
            value = self.yrspars.loc[year,'arf']
        return value
    def drc(self,year):
        """
        Taux de bonification des prestations pour un début après 65 ans.

        Parameters
        __________
        year : int
            année du paramètre
        Return
        ______
        float
            Taux de bonification.
        """
        if (year > self.stop):
            value = self.yrspars.loc[self.stop,'drc']
        else :
            value = self.yrspars.loc[year,'drc']
        return value
    def nympe(self,year):
        """
        Parameters
        __________
        year : int
            année du paramètre
        Return
        ______
        float
            nombre d'année pour calculé ympe moyen
        """
        if (year > self.stop):
            value = self.yrspars.loc[self.stop,'nympe']
        else :
            value = self.yrspars.loc[year,'nympe']
        return value
    def reprate(self,year):
        """

        Parameters
        __________
        year : int
            année du paramètre

        Return
        ______
        float
            taux de remplacement régime de base
        """
        if (year > self.stop):
            value = self.yrspars.loc[self.stop,'reprate']
        else :
            value = self.yrspars.loc[year,'reprate']
        return value
    def reprate_s1(self,year):
        """
        Parameters
        __________
        year : int
            année du paramètre
        Return
        ______
        float
            taux de remplacement régime supplémentaire 1
        """
        if (year > self.stop):
            value = self.yrspars.loc[self.stop,'reprate_s1']
        else :
            value = self.yrspars.loc[year,'reprate_s1']
        return value
    def reprate_s2(self,year):
        """
        Parameters
        __________
        year : int
            année du paramètre
        Return
        ______
        float
            taux de remplacement régime supplémentaire 1
        """
        if (year > self.stop):
            value = self.yrspars.loc[self.stop,'reprate_s2']
        else :
            value = self.yrspars.loc[year,'reprate_s2']
        return value
    def droprate(self,year):
        """
        Parameters
        __________
        year : int
            année du paramètre
        Return
        ______
        float
            taux d'année droppé
        """
        if (year > self.stop):
            value = self.yrspars.loc[self.stop,'droprate']
        else :
            value = self.yrspars.loc[year,'droprate']
        return value
    # def pu1(self,year):
    #     """
    #     Parameters
    #     __________
    #     year : int
    #         année du paramètre
    #     Return
    #     ______
    #     float
    #         NE PAS UTILISER
    #     """
    #     if year > self.stop :
    #         yr = self.stop
    #     else:
    #         yr = year
    #     return self.yrspars.loc[yr,'pu1']
    # def pu2(self,year):
    #     """
    #     Return
    #     ______
    #     float
    #         NE PAS UTILISER
    #     """
    #     if year > self.stop :
    #         yr = self.stop
    #     else:
    #         yr = year
    #     return self.yrspars.loc[yr,'pu2']
    # def pu3(self,year):
    #     """
    #     Return
    #     ______
    #     float
    #         NE PAS UTILISER
    #     """
    #     if year > self.stop :
    #         yr = self.stop
    #     else:
    #         yr = year
    #     return self.yrspars.loc[yr,'pu3']
    # def pu4(self,year):
    #     """
    #     Return
    #     ______
    #     float
    #         NE PAS UTILISER
    #     """
    #     if year > self.stop :
    #         yr = self.stop
    #     else:
    #         yr = year
    #     return self.yrspars.loc[yr,'pu4']
    def supp(self,year):
        """
        Parameters
        __________
        year : int
            année du paramètre
        Return
        ______
        float
            taux de bonification pour PRB sur régime de base
        """
        if year > self.stop :
            yr = self.stop
        else:
            yr = year
        return self.yrspars.loc[yr,'supp']
    def supp_s1(self,year):
        """
        Parameters
        __________
        year : int
            année du paramètre
        Return
        ______
        float
            taux de bonification pour PRB sur régime supplémentaire 1
        """
        if year > self.stop :
            yr = self.stop
        else:
            yr = year
        return self.yrspars.loc[yr,'supp_s1']
    def supp_s2(self,year):
        """
        Parameters
        __________
        year : int
            année du paramètre
        Return
        ______
        float
            taux de bonification pour PRB sur régime supplémentaire 2
        """
        if year > self.stop :
            yr = self.stop
        else:
            yr = year
        return self.yrspars.loc[yr,'supp_s2']
    # def survmax60(self,year):

    #     if year > self.stop :
    #         yr = self.stop
    #     else:
    #         yr = year
    #     return self.yrspars.loc[yr,'survmax60']
    # def survmax65(self,year):
    #     if year > self.stop :
    #         yr = self.stop
    #     else:
    #         yr = year

    #     return self.yrspars.loc[yr,'survmax65']
    # def survage1(self,year):
    #     if year > self.stop :
    #         yr = self.stop
    #     else:
    #         yr = year
    #     return self.yrspars.loc[yr,'survage1']
    # def survage2(self,year):
    #     if year > self.stop :
    #         yr = self.stop
    #     else:
    #         yr = year
    #     return self.yrspars.loc[yr,'survage2']
    # def survrate1(self,year):
    #     if year > self.stop :
    #         yr = self.stop
    #     else:
    #         yr = year
    #     return self.yrspars.loc[yr,'survrate1']
    # def survrate2(self,year):
    #     if year > self.stop :
    #         yr = self.stop
    #     else:
    #         yr = year
    #     return self.yrspars.loc[yr,'survrate2']
    def era(self,year):
        """
        Parameters
        __________
        year : int
            année du paramètre
        Return
        ______
        float
            age minimum pour claimer
        """
        if year > self.stop :
            yr = self.stop
        else:
            yr = year
        return self.yrspars.loc[yr,'era']
    def nra(self,year):
        """
        Parameters
        __________
        year : int
            année du paramètre
        Return
        ______
        float
            age "standard"
        """
        if year > self.stop :
            yr = self.stop
        else:
            yr = year
        return self.yrspars.loc[yr,'nra']
    def disab_rate(self,year):
        """
        Parameters
        __________
        year : int
            année du paramètre
        Return
        ______
        float
            taux pour disability
        """
        if year > self.stop :
            yr = self.stop
        else:
            yr = year
        return self.yrspars.loc[yr,'disab_rate']
    def disab_base(self,year):
        """
        Parameters
        __________
        year : int
            année du paramètre
        Return
        ______
        float
            Montant pour disability
        """
        if year > self.stop :
            yr = self.stop
        else:
            yr = year
        return self.yrspars.loc[yr,'disab_base']
    def cola(self,year):
        """
        Parameters
        __________
        year : int
            année du paramètre
        Return
        ______
        float
            taux d'indexation du YMPE
        """
        if year > self.stop :
            value  = self.cpi
        else:
            value = self.yrspars.loc[year,'cola']
        return value
    def gIndexation(self,start,stop):
        """
        calcule l'indexation pour une période définie par (start,stop)

        Parameters
        __________
        start : int
            première année d'indexation
        stop : int
            dernière année d'indexation

        Return
        ______
        float
            indexation
        """
        return self.indexation[start-self.start][stop-self.start]
    def max_benefit(self,year):
        """
        Parameters
        __________
        year : int
            année du paramètre
        calcule le benefice maximum éligible à une année (moyenne du YMPE des 5 dernière année )
        Return
        ______
        float

        """
        return np.mean([self.ympe(x) for x in [max(year-x,1966) for x in range(5)]])*self.reprate(year)/12
    # def chgpar(self,name,y0,y1,value):
    #     """
    #     Ne pas utiliser
    #     """
    #     if (name in self.yrspars.columns):
    #         for i in range(y0,y1+1):
    #             self.yrspars.loc[i,name] = value
    #     else :
    #         for i in range(y0,y1+1):
    #             pass
    #             #self.byrpars.loc[i,name] = value

class account:
    """
    Classe principal regroupant l'information sur un individu et comprenant les
    fonctions de calcul.

    Principal interface avec le simulateur. Chaque individu simulé doit avoir une
    instance de classe.

    Parameters
    __________
    byear : int
        année de naissance
    rules : rules
        une instance de la classe rules.

    """
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
        self.prb = [0 for x in range(11)]
        self.prb_s1 = [0 for x in range(11)]
        self.prb_s2 = [0 for x in range(11)]
        self.cqppcontrib = 0.0

    def MakeContrib(self,year,earn,earn_aut=0,kids=False):
        """
        Ajoute une année de cotisation (une instance de record dans la list des années de cotisation)

        Parameters
        __________
        year : int
            année de cotisation
        earn : float
            revenue d'emploi cotisable
        earn_aut : float
            revenue autonome cotisable
        kids : boolean
        enfants de moins de 7  True seulemet pour la personne admissible à l'exemption pour enfants


        """
        if year>=self.rules.start:
            if earn>=self.rules.ympe(year):
                taxable = self.rules.ympe(year)-self.rules.exempt(year)
                taxable_aut = 0
            elif (earn+earn_aut) >=self.rules.ympe(year):
                if earn >= self.rules.exempt(year):
                    taxable = earn-self.rules.exempt(year)
                    taxable_aut = self.rules.ympe(year)-earn
                else :
                    taxable = 0
                    taxable_aut = self.rules.ympe(year) - self.rules.exempt(year)
            else:
                taxable = earn
                taxable_aut = earn_aut
                if taxable >=self.rules.exempt(year):
                    taxable -= self.rules.exempt(year)
                elif taxable + taxable_aut >=self.rules.exempt(year):
                    taxable_aut -= (self.rules.exempt(year)-taxable)
                    taxable = 0.0
                else:
                    taxable = 0.0
                    taxable_aut = 0.0


            contrib = self.rules.worktax(year) * taxable
            contrib_aut = self.rules.worktax(year) * taxable_aut * 2
            contrib_s1 = self.rules.worktax_s1(year) * taxable
            contrib_aut_s1 = self.rules.worktax_s1(year) * taxable_aut * 2

            taxable_s2 = np.min( [np.max([earn-self.rules.ympe(year),0.0]) , (self.rules.ympe_s2(year)-self.rules.ympe(year))])
            if taxable_s2>0.0:
                if taxable_s2<(self.rules.ympe_s2(year)-self.rules.ympe(year)):
                   taxable_aut_s2 =  np.min([taxable_aut,(self.rules.ympe_s2(year)-self.rules.ympe(year))-taxable_s2])
                else :
                    taxable_aut_s2 = 0.0
            else:
                taxable_aut_s2 = 0.0
            contrib_s2 = self.rules.worktax_s2(year) * taxable_s2
            contrib_aut_s2 = self.rules.worktax_s2(year) * taxable_aut_s2 * 2
            self.cqppcontrib = contrib +contrib_s2
            index = self.gAge(year)-18
            self.history[index]= record(year,earn=earn+earn_aut,contrib = contrib,contrib_s1 = contrib_s1,contrib_s2=contrib_s2,
                                        contrib_aut = contrib_aut,contrib_aut_s1 = contrib_aut_s1,contrib_aut_s2=contrib_aut_s2,kids=kids)
            if self.claimage!=None:
                self.CalcPRB(year,taxable,taxable_s2,earn)

    def ClaimCPP(self,year):
        """
        Effectue le claiming, appelle CalcAMPE() et CalcBenefit()

        Parameters
        __________
        year : int
            année de cotisation
        """
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
        """
        Retourne l'âge de l'individu à une année donné.

        Parameters
        __________
        year : int
            année

        Return
        ______
        int

        """
        return year - self.byear
    def gYear(self,age):
        """
        Retourne l'année pour un âge donné.

        Parameters
        __________
        year : int
            Âge

        Return
        ______
        int

        """
        return self.byear + age
    def CalcAMPE(self,year):
        """
        Calcule le AMPE

        Parameters
        __________
        year : int
            année de claiming
        """
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
        """
        Calcule le benefice (base,supp 1 et supp 2). Appellé après CalcAMPE()

        Parameters
        __________
        year : int
            année de claiming
        """
        if self.receiving==True:
            if (self.gAge(year)==self.claimage):
                nra = self.rules.nra(year)
                ca = self.rules.ca(year)
                arf = self.rules.arf(year)
                drc = self.rules.drc(year)
                age = self.gAge(year)

                self.benefit = self.rules.reprate(year) * self.ampe
                self.benefit_s1 = self.rules.reprate_s1(year) * self.ampe_s1
                self.benefit_s2 = self.rules.reprate_s2(year) * self.ampe_s2
                if (age<nra):
                    self.benefit *= 1.0+(arf+int(self.rules.qpp)*ca*self.benefit/self.rules.max_benefit(year))*(age-nra)
                    self.benefit_s1 *= 1.0+(arf+int(self.rules.qpp)*ca*self.benefit/self.rules.max_benefit(year))*(age-nra)
                    self.benefit_s2 *= 1.0+(arf+int(self.rules.qpp)*ca*self.benefit/self.rules.max_benefit(year))*(age-nra)
                else :
                    self.benefit *= 1.0+drc*(age-nra)
                    self.benefit_s1 *= 1.0+drc*(age-nra)
                    self.benefit_s2 *= 1.0+drc*(age-nra)
        else:
            self.benefit = 0.0
            self.benefit_s1 = 0.0
            self.benefit_s2 = 0.0
    def CalcPRB(self,year,taxable,taxable_s2,earn):
        """
        Calcule le Post retirement benefice quand une cotisation est faite après avoir claimer.
        Appelé par MakeContrib() qui calcule les paramêtres

        Parameters
        __________
        year : int
            année de la cotisation
        taxable : float
            montant cotisable pour régime de base et supplémentaire 1
        taxable_s2 : float
            montant cotisable pour régime supplémentaire 2
        """
        if self.rules.qpp:
            if year>=2014:
                self.prb[self.gAge(year)-60+1] = (self.prb[self.gAge(year)-60]*(1+self.rules.cola(year))
                                                  + taxable*self.rules.supp(year)/12)
                self.prb_s1[self.gAge(year)-60+1] = (self.prb_s1[self.gAge(year)-60]*(1+self.rules.cola(year))+
                                                    taxable*self.rules.worktax_s1(year)*100*self.rules.supp_s1(year)/12)
                self.prb_s2[self.gAge(year)-60+1] = (self.prb_s2[self.gAge(year)-60+1] +
                                                     taxable_s2*self.rules.worktax_s2(year)*100*self.rules.supp_s2(year)/12)
                if self.gAge(year)<69:
                    for index in range(self.gAge(year)-60+2,11):
                        self.prb[index] = self.prb[index-1]*(1+self.rules.cola(year+index))
                        self.prb_s1[index] = self.prb_s1[index-1]*(1+self.rules.cola(year+index))
                        self.prb_s2[index] = self.prb_s2[index-1]*(1+self.rules.cola(year+index))

        else:
            if year>=2014 & self.gAge(year)<70:
                nra = self.rules.nra(year)
                arf = self.rules.arf(year)
                drc = self.rules.drc(year)
                age = self.gAge(year)
                upe = np.min([earn,self.rules.ympe(year)])
                if upe<self.rules.exempt(year) : upe = 0
                #upe_s2 Need to start only in 2024
                upe_s2 = np.max([np.min([earn-self.rules.ympe(year),self.rules.ympe_s2(year)-self.rules.ympe(year)]),0.0])
                #PRB base
                prb = upe/self.rules.ympe(year) * self.rules.ympe(year+1)*self.rules.supp(year)
                # PRB S1
                prb_s1 = upe/self.rules.ympe(year) * self.rules.ympe(year+1)*self.rules.worktax_s1(year)*100*self.rules.supp_s1(year)
                #PRB S2
                if upe_s2>0:
                    prb_s2 = upe_s2/(self.rules.ympe_s2(year)-self.rules.ympe(year))*(self.rules.ympe_s2(year+1)-self.rules.ympe(year+1)) * self.rules.supp_s1(year)
                #Ajustment factor
                if (age<nra):
                    self.prb[self.gAge(year)-60+1] = self.prb[self.gAge(year)-60]*(1+self.rules.cola(year)) + (1.0+arf*(age-nra)) * prb/12
                    self.prb_s1[self.gAge(year)-60+1] = self.prb_s1[self.gAge(year)-60]*(1+self.rules.cola(year)) + (1.0+arf*(age-nra)) * prb_s1/12
                    self.prb_s2[self.gAge(year)-60+1] = self.prb_s2[self.gAge(year)-60]*(1+self.rules.cola(year)) + (1.0+arf*(age-nra)) * prb_s2/12
                else :
                    self.prb[self.gAge(year)-60+1] = self.prb[self.gAge(year)-60]*(1+self.rules.cola(year)) + (1.0+drc*(age-nra)) * prb/12
                    self.prb_s1[self.gAge(year)-60+1] = self.prb_s1[self.gAge(year)-60]*(1+self.rules.cola(year)) + (1.0+drc*(age-nra)) * prb_s1/12
                    self.prb_s2[self.gAge(year)-60+1] = self.prb_s2[self.gAge(year)-60]*(1+self.rules.cola(year)) + (1.0+drc*(age-nra)) * prb_s2/12
                if self.gAge(year)<69:
                    for index in range(self.gAge(year)-60+2,11):
                        self.prb[index] = self.prb[index-1]*(1+self.rules.cola(year+index))
                        self.prb_s1[index] = self.prb_s1[index-1]*(1+self.rules.cola(year+index))
                        self.prb_s2[index] = self.prb_s2[index-1]*(1+self.rules.cola(year+index))
    def gBenefit(self,year):
        """
        Retourne le montant de la prestation du régime de base à une année donnée

        Parameters
        __________
        year : int
            année de la prestation
        Return
        ______
        float
            bénéfice
        """
        if self.claimage :
            claimyear = self.gYear(self.claimage)
            return self.benefit * self.rules.gIndexation(claimyear,year)
        else :
            return self.benefit
    def gBenefit_s1(self,year):
        """
        Retourne le montant de la prestation du régime supplémentaire 1à une année donnée

        Parameters
        __________
        year : int
            année de la prestation
        Return
        ______
        float
            bénéfice
        """
        if self.claimage :
            claimyear = self.gYear(self.claimage)
            return self.benefit_s1 * self.rules.gIndexation(claimyear,year)
        else :
            return self.benefit_s1
    def gBenefit_s2(self,year):
        """
        Retourne le montant de la prestation du régime supplémentaire 2 à une année donnée

        Parameters
        __________
        year : int
            année de la prestation
        Return
        ______
        float
            bénéfice
        """
        if self.claimage :
            claimyear = self.gYear(self.claimage)
            return self.benefit_s2 * self.rules.gIndexation(claimyear,year)
        else :
            return self.benefit_s2
    def gPRB(self,year):
        """
        Retourne le montant du PRB du régime de base à une année donnée

        Parameters
        __________
        year : int
            année du PRB
        Return
        ______
        float
            montant PRB
        """
        if self.gAge(year)<60 :
            return 0.0
        elif  self.gAge(year)<self.gAge(year)<=70:
            return self.prb[self.gAge(year)-60]
        else :
            return self.prb[10]*self.rules.gIndexation(self.gYear(70),year)
    def gPRB_s1(self,year):
        """
        Retourne le montant du PRB du régime supplémentaire 1 à une année donnée

        Parameters
        __________
        year : int
            année du PRB
        Return
        ______
        float
            montant PRB
        """
        if self.gAge(year)<60 :
            return 0.0
        elif  self.gAge(year)<self.gAge(year)<=70:
            return self.prb_s1[self.gAge(year)-60]
        else :
            return self.prb_s1[10]*self.rules.gIndexation(self.gYear(70),year)
    def gPRB_s2(self,year):
        """
        Retourne le montant du PRB du régime supplémentaire 2 à une année donnée

        Parameters
        __________
        year : int
            année du PRB
        Return
        ______
        float
            montant PRB
        """
        if self.gAge(year)<60 :
            return 0.0
        elif  self.gAge(year)<self.gAge(year)<=70:
            return self.prb_s2[self.gAge(year)-60]
        else :
            return self.prb_s2[10]*self.rules.gIndexation(self.gYear(70),year)

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
        """
        Reset un account
        """
        self.claimage = None
        self.history = [record(yr) for yr in range(self.byear+18,self.byear+70,1)]
        self.ncontrib = 0
        self.ampe = 0.0
        self.receiving = False
        self.benefit = 0.0
