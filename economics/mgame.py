import sympy
import ipywidgets
import numpy as np
import matplotlib.pyplot as plt
import nashpy


class eq():
    def __init__(self):
        points = 101
        self.x,step = np.linspace(0,1,points,retstep=True)        
        
        #--- Sliders----
        # n: number of times mediator replicates computation
        self.wn = ipywidgets.IntSlider(min=0,max=10,step=1,value=1, description='n',continuous_update=False)
        # PR: The penalty rate for being at fault
        self.wPR = ipywidgets.IntSlider(min=0,max=10,step=1,value=2, description='PR', continuous_update=False) 
        # C_r: the cost of requesting mediation
        self.wC_r = ipywidgets.IntSlider(min=0,max=100,step=1,value=0, description='C_r', continuous_update=False)

        # p_a: The probability that a non-determinstic job will return result a, which the JC will accept
        self.wp_a = ipywidgets.FloatSlider(min=0, max=1, step=.1, value=1, description='p_a', continuous_update=False)
        # B: The benefit the JC recieves for having the job executed
        self.wB = ipywidgets.IntSlider(min=0,max=100,step=1,value=2, description='B', continuous_update=False)
        # pi: The reward the JC offers for a job result 
        self.wpi = ipywidgets.IntSlider(min=0,max=100,step=1,value=1, description='pi', continuous_update=False)
        # C_v: The JCs cost of verifying a result
        self.wC_v = ipywidgets.IntSlider(min=0,max=100,step=1,value=1, description='C_v',continuous_update=False) # cost for JC to verify
        # p_v: the probability the JC will verify a result. Same as sigma_v, but a slider. 
        self.wp_v = ipywidgets.FloatSlider(min=0, max=1, step=.1, value=1, description='p_v', continuous_update=False)

        
        # C_e: The RPs cost to execute a job
        self.wC_e = ipywidgets.IntSlider(min=0,max=10,step=1,value=1, description='C_e',continuous_update=False) # cost for RP to execute
        # C_d: The RPs cost to generate a deceitful result
        self.wC_d = ipywidgets.IntSlider(min=0,max=10,step=1,value=0, description='C_d',continuous_update=False) # cost for RP to deceive
        # p_e: the probability the RP will execute a job. Same as sigma_e, but a slider
        self.wp_e = ipywidgets.FloatSlider(min=0, max=1, step=.1, value=1, description='p_e', continuous_update=False)

        self.ui1 = ipywidgets.HBox([self.wn,self.wPR, self.wC_r])
#         self.ui2 = ipywidgets.HBox([self.wp_a, self.wB,self.wpi,self.wC_v, self.wp_v])
#         self.ui3 = ipywidgets.HBox([self.wC_e,self.wC_d, self.wp_e])
        self.ui2 = ipywidgets.HBox([self.wB,self.wpi,self.wC_v])
        self.ui3 = ipywidgets.HBox([self.wC_e,self.wC_d])
        
        
        #--- Symbolic symbols used in expected utility functions
        # n, PR,C_r, D, p_a, B, pi, C_v, p_v, C_e, C_d, p_e
        self.n, self.PR, self.C_r, self.D = sympy.symbols('n PR C_r D', real=True)
        self.p_a, self.B, self.pi, self.C_v,  = sympy.symbols('p_a B pi C_v', real=True)
        self.C_e, self.C_d = sympy.symbols('C_e C_d', real=True)
        
        
    def getEq(self, n,PR,C_r,     B,pi,C_v,     C_e,C_d,    points, verbose):
        
        D = pi*(PR + n)
        
        JC_max = {"JU":-1e6, "RU":-1e6, "p_a":-1}
        
        x_axis = []
        y_axis_jutil = []
        y_axis_pv = []
        y_axis_rutil = []
        y_axis_pe = []
        
        for p_a in np.linspace(0,1,points):
            
            
            
            R1,R2,R3,R4 =self.RPU(n, D, p_a, pi, C_e, C_d)
            RUM = np.array([[R1, R2], [R3, R4]]) #Row player
            
            J1,J2,J3,J4 = self.JCU(n, D, p_a, B, pi, C_v, C_r)
            JUM = np.array([[J1, J2],[J3, J4]]) #Col player  
            
            g = nashpy.Game(RUM, JUM)
            
            if verbose:
                print("---p_a={}---".format(p_a))
                display(g)
                
            eq = g.support_enumeration()
            for s1, s2 in eq:
                RP_util = np.dot(np.dot(s1, RUM), s2)
                JC_util = np.dot(np.dot(s1, JUM), s2)
                
                x_axis.append(p_a)
                
                y_axis_rutil.append(RP_util)
                y_axis_pe.append(s1[0])
                
                y_axis_jutil.append(JC_util)
                y_axis_pv.append(s2[0])
                
                # Show probability of each outcome
                
                s1_2DT = np.array([s1]).T
                s2_2D = np.array([s2])     
                outcomes = np.dot(s1_2DT,s2_2D)  
                
                if verbose:
                    print("RP strategy: {}".format(s1))
                    print("JC strategy: {}".format(s2))
                    display(outcomes)
             
                
                if JC_util > JC_max["JU"]:
                    JC_max["RU"] = RP_util
                    JC_max["JU"] = JC_util            
                    JC_max["p_a"] = p_a
                    JC_max["g"] = g 
                    JC_max["outcomes"] = outcomes
                    JC_max["auto"] = B - C_e
        
        return JC_max, x_axis, y_axis_rutil, y_axis_pe, y_axis_jutil, y_axis_pv
        
        
        
    def findNashEq(self, n, PR, p_a, B, pi, C_v, C_r, C_e, C_d):
        '''This function solves for the mixed strategy of the JC and RP. 
        sigma_e : the probability that the RP will execute the job
        1-sigma_e: the probability that the RP will not execute the job
        sigma_v: the probability that the JC will verify the result
        1-sigma_v: the probability that the JC will not verify the result'''
#         n, PR,C_r, D, p_a, B, pi, C_v, p_v, C_e, C_d, p_e
        
        sigma_e = sympy.symbols('sigma_e', real=True)
        sigma_v = sympy.symbols('sigma_v', real=True)
        
        
        D = pi*(PR + n)
        
        # J1 and J3, JC verifies
        # J2 and J4, JC does not verify
        J1,J2,J3,J4 = self.JCU(n, D, p_a, B, pi, C_v, C_r)
        
        # Ev: JC's expected value for verifying the result
        Ev = sigma_e*J1 + (1-sigma_e)*J3
        print("E[v]")
        display(Ev)
        print("E[v] simplified")
        display(Ev.expand().simplify())
        # Ep: JC's expected value for not verifying the result
        Ep = sigma_e*J2 + (1-sigma_e)*J4 
        # RP solves for sigma_e such that the JC is ambivalent between verifying and not verifying by setting the two expected values equal to each other
        nse = sympy.solve([Ev - Ep], [sigma_e])[sigma_e]
        
        # Substitute nse(nash equlibrium sigma_e) back in to JC's expected utility function
        print("E[v] with nse simplified")
        Evne = nse*J1 + (1-nse)*J3
        display(Evne.expand().simplify())
        
     
        # R1 and R2, RP executes 
        # R3 and R4, RP does not execute
        R1,R2,R3,R4 = self.RPU(n, D, p_a, pi, C_e, C_d)
        # Ee: RP's expected value for executing the job
        Ee = sigma_v*R1 + (1-sigma_v)*R2
        print("E[e]")
        display(Ee)
        print("E[e] simplified")
        display(Ee.expand().simplify())
        # Ed: RP's expected value for sending a deceiving result
        Ed = sigma_v*R3 + (1-sigma_v)*R4
        print("E[d] simplified")
        display(Ed.expand().simplify())
        # JC solve sor sigma_v such that the RP is ambivalent between executing and deceiving 
        nsv = sympy.solve([Ee - Ed], [sigma_v])[sigma_v]
        
        
        print("E[e] with nsv simplified")
        Eene = nsv*R1 + (1-nsv)*R2
        display(Eene.expand().simplify())
        
#         print("E[d] with nsv simplified")
#         Edne = nsv*R3 + (1-nsv)*R4
#         display(Edne.expand().simplify())
        
        
        # Verify that when substituting nse back in the JC expected values are equal.
        check_e = nse*J1 + (1-nse)*J3 - ( nse*J2 + (1-nse)*J4 )
        print("e is equilibrium: {}, check is {}".format(check_e.simplify()==sympy.numbers.Zero, check_e.simplify()))
        
        
        return nse, nsv
        
    def JCU(self, n, D, p_a, B, pi, C_v, C_r):
        '''The JC's utility functions'''
        J1 = p_a*(B-pi-C_v) + (1-p_a)*( (p_a**n)*(B+pi-C_v) + (1-p_a**n)*(B-D-C_r-C_v))
        J2 = B - pi
        J3 = (p_a**n)*(-C_v+pi) + (1-p_a**n)*(-C_r-C_v-D)
        J4 = -pi
        
        return J1, J2, J3, J4
    
#     def RPU(self, p_a, pi, C_e, C_d, n, D):
    def RPU(self, n, D, p_a, pi, C_e, C_d):
        '''The RP's utility functions'''
        R1 = p_a*(pi-C_e) + (1-p_a)*( (p_a**n)*(-C_e-D) + (1-p_a**n)*(pi-C_e))
        R2 = pi - C_e
        R3 = (p_a**n)*(-C_d-D) + (1-p_a**n)*(-C_d+pi)
        R4 = pi - C_d
        
        return R1, R2, R3, R4
    
    def getE(self, x, sigma, A, B):
        '''returns the expected value, replacing the symbol p_a with a value. 
        p_a: probabaility of non-derterministic job returning result a, which is accepted'''
        
        
        symE = sigma*A + (1-sigma)*B
        E = symE.subs({self.p_a:x})
        try:
            Ef = float(E)
        except TypeError as err:
            if E == sympy.zoo:
                Ef = np.Infinity
            print(err)
            print(symE.expand().simplify())
            print(E)
            print(Ef)
            
        
        return Ef
    
    
    def getE2(self, x, sigma, A, B):
        '''returns the expected value, replacing the symbol p_a with a value. 
        p_a: probabaility of non-derterministic job returning result a, which is accepted'''
        
        
        symE = sigma*A + (1-sigma)*B
        E = symE.subs({self.p_a:x})
        try:
            Ef = float(E)
        except TypeError as err:
            if E == sympy.zoo:
                Ef = np.Infinity
            print(err)
            print(symE.expand().simplify())
            print(E)
            print(Ef)
            
        
        return Ef
    
        
        
        
    def updatePlot(self, se, sv, wn, wPR, wB, wpi, wC_v, wC_r, wC_e, wC_d):
        '''Plot Expected values against p_a with corresponding nash eq values of sigma_v and sigma_e'''
        print(wn,wPR,wC_r,     wB,wpi,wC_v,     wC_e, wC_d)
        
        
        # Replace symbols in sigma_e with values of sliders, except for p_a
        self.f_nse = se.subs({self.n:wn, self.PR:wPR, self.pi:wpi, self.C_v:wC_v, self.C_r:wC_r})
        #  Replace symbols in sigma_v with values of sliders, except for p_a
        self.f_nsv = sv.subs({self.n:wn, self.PR:wPR, self.pi:wpi, self.C_e:wC_e, self.C_d:wC_d})

        y_nse = []
        y_nsv = []
        for v in self.x:
            a0 = self.f_nse.subs({self.p_a:v})
            b0 = self.f_nsv.subs({self.p_a:v})
            
            try :
                a1 = float(a0)
                b1 = float(b0)
            except TypeError as err:
                print("Type error: {0}".format(err))
                print("p_a: {}".format(v))
                print("value: {}, type:{}".format(a0, type(a1)))
                if a0 == sympy.zoo:
                    a1 = np.Infinity
                if b0 == sympy.zoo:
                    b1 = np.Infinity
            
            if a1<0:
                a2 = 0
            elif a1>1:
                a2 = 1
            else:
                a2 = a1
                
            if b1<0:
                b2 = 0
            elif b1>1:
                b2 = 1
            else:
                b2 = b1
                    
                
                
            y_nse.append(a2)
            y_nsv.append(b2)
 
                      
        
        D = wpi*(wPR + wn)
        
        f_E = np.vectorize(self.getE)
        f_E2 = np.vectorize(self.getE2)
        
        # Solve for JC expected value, varying p_a and using the corresponding mixed nash equlibrium value for sigma_v
        J1,J2,J3,J4 = self.JCU(wn, D, self.p_a, wB, wpi, wC_v, wC_r)
#         self.Ev = f_E(self.x, self.f_nse, J1, J3)
        self.Ev = f_E2(self.x, y_nse, J1,J3)
        
        # Solve for RP expected value, varying p_a and using the corresponding mixed nash equlibrium value for sigma_e
        R1,R2,R3,R4 = self.RPU(wn, D, self.p_a, wpi, wC_e, wC_d)
        self.Ee = f_E(self.x, self.f_nsv, R1, R2)
        
        
        # Generating plot
        fig = plt.figure(constrained_layout=True)
        
          
        spec = fig.add_gridspec(ncols=2, nrows=2)

        ax1 = fig.add_subplot(spec[0, 0])
        ax2 = fig.add_subplot(spec[1, 0])
#         ax3 = fig.add_subplot(spec[2:, 0])
        ax4 = fig.add_subplot(spec[0, 1])
        ax5 = fig.add_subplot(spec[1, 1])
#         ax6 = fig.add_subplot(spec[2:, 1])

        plt.xlabel("p_a")
    
        
        ax1.plot(self.x,self.Ev, label="E[v]")
        ax1.set_ylabel('E[v]')
        ax1.grid(visible=True)
        ax1.set_ylim([-2,2])
    
            

        ax2.plot(self.x,y_nse, label="nash se")
        ax2.set_ylabel('P(e)')
        ax2.grid(visible=True)
        ax2.set_ylim([0,2])
        

        ax4.plot(self.x,self.Ee, label="E[e]")
        ax4.set_ylabel('E[e]')
        ax4.grid(visible=True)
        ax4.set_ylim([-2,2])
        
        ax5.plot(self.x,y_nsv, label="nash sv")
        ax5.set_ylabel('P(v)')
        ax5.grid(visible=True)
        ax5.set_ylim([0,2])
#         ax2.set_yscale('log')

    def plotEvsS(self, wn, wPR, wp_a, wB, wpi, wC_v, wC_r, wC_e, wC_d):
        '''Plot JC's Expected value against sigma, holding p_a constant'''
        
        
        D = wpi*(wPR + wn)
        J1,J2,J3,J4 = self.JCU(wn, D, wp_a, wB, wpi, wC_v, wC_r)
        # Solve for JC expected value, varying sigma_e, holding p_a constant
        Ev = list(map(lambda s: 
                      float(s*J1+(1-s)*J3), self.x))
        
        
        
        
        plt.plot(self.x, Ev)
        plt.xlabel("sigma")
        plt.ylabel("E[v]")
        
        
        
    
    def plotEvsPa(self, wn, wPR, wC_r, wp_a, wB, wpi, wC_v, wp_v, wC_e, wC_d, wp_e):
        '''Plot Expected values against p_a using sliders to set sigma_v and sigma_e'''
        # n, PR,C_r, D, p_a, B, pi, C_v,  C_e, C_d, p_e        
        
        D = wpi*(wPR + wn)
        J1,J2,J3,J4 = self.JCU(wn, D, self.p_a, wB, wpi, wC_v, wC_r)
        
        Ev = wp_e*J1 + (1-wp_e)*J3
        
        display(Ev)
        Ev = list(map(lambda p: 
                      float(Ev.subs({self.p_a:p})), self.x))
        
        plt.plot(self.x, Ev, label="Ev")
       
      
        
        R1,R2,R3,R4 = self.RPU(wn, D, wp_a, wpi, wC_e, wC_d)
        Ee = list(map(lambda s: 
                      float(s*R1+(1-s)*R2), self.x))
        
        plt.plot(self.x, Ee, label="Ee")
        plt.xlabel("p_a")
        plt.ylabel("E")
        
        plt.legend()
        plt.grid(visible=True)
        
        
        my_max = max(Ev)
        my_p_a = self.x[Ev.index(my_max)]
        print(my_p_a)
        
        print("nsv-slider")
        print(nsv.subs({self.n:wn, self.PR:wPR, self.p_a:wp_a, self.pi:wpi, self.C_e:wC_e, self.C_d:wC_d}))
        print("nsv-max")
        print(nsv.subs({self.n:wn, self.PR:wPR, self.p_a:my_p_a, self.pi:wpi, self.C_e:wC_e, self.C_d:wC_d}))
        
        print("nse-slider")
        print(nse.subs({self.n:wn, self.PR:wPR, self.p_a:wp_a, self.pi:wpi, self.C_v:wC_v, self.C_r:wC_r}))
        print("nse-max")
        print(nse.subs({self.n:wn, self.PR:wPR, self.p_a:my_p_a, self.pi:wpi, self.C_v:wC_v, self.C_r:wC_r}))
        

    def plot(self, nse, nsv):

#       n, PR, D, p_a, B, pi, C_v, C_r, C_e, C_d

        self.interactive_plot = ipywidgets.interactive_output(self.updatePlot, {'se':ipywidgets.fixed(nse),
                                                                                'sv':ipywidgets.fixed(nsv),
                                                                                'wn':self.wn,
                                                                                'wPR':self.wPR,
                                                                                'wB':self.wB,
                                                                                'wpi':self.wpi,
                                                                                'wC_v':self.wC_v,
                                                                                'wC_r':self.wC_r,
                                                                                'wC_e':self.wC_e,
                                                                                'wC_d':self.wC_d})
        display(self.interactive_plot, self.ui1, self.ui2, self.ui3)
        
    def plot2(self):
        self.p2 = ipywidgets.interactive_output(self.plotEvsS, {'wn':self.wn,
                                                                'wPR':self.wPR,
                                                                'wp_a':self.wp_a,
                                                                'wB':self.wB,
                                                                'wpi':self.wpi,
                                                                'wC_v':self.wC_v,
                                                                'wC_r':self.wC_r,
                                                                'wC_e':self.wC_e, 
                                                                'wC_d':self.wC_d})
        display(self.p2, self.ui1, self.ui2, self.ui3)
        
    def plot3(self):
        self.p3 = ipywidgets.interactive_output(self.plotEvsPa, {'wn':self.wn,
                                                                'wPR':self.wPR,
                                                                'wC_r':self.wC_r,
                                                                 
                                                                'wp_a':self.wp_a,
                                                                'wB':self.wB,
                                                                'wpi':self.wpi,                                                                
                                                                'wC_v':self.wC_v,
                                                                'wp_v':self.wp_v,
                                                                
                                                                'wC_e':self.wC_e, 
                                                                'wC_d':self.wC_d,
                                                                'wp_e':self.wp_e})
        display(self.p3, self.ui1, self.ui2, self.ui3)
        
        