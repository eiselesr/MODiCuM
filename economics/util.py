from IPython.display import clear_output
import matplotlib.pyplot as plt
import numpy as np
import csv
import sympy
from numpy import array

def JCU(n,PR,C_r,  p_a,B,pi,p_v=1,C_v=0,  p_E=1):
    p2 = 1-p_a
    
    NDD = pi * PR + (pi* n)
     
    JC_HRP  =  p_v*p_E*p_a*(B-pi-C_v) # RP correctly (p_E) returns answer 1 (p_a) and pay up to max_pi (-pi).
    
    NDJC_HRP_Gain = p_v*p_E*p2*(p_a**n)*(B + pi -C_v) # receives gas pi because it had to spend some to request mediation.
    NDJC_HRP_Lose = p_v*p_E*p2*(1-(p_a**n))*(B-NDD-C_r-C_v) #RP returns answer 2 correctly or responds incorrectly (p2+(1-p_E)) and send to mediator who finds out nonDeterminism (1-p_a^n), pay fine and reward (f2+r)
    ND = NDJC_HRP_Gain + NDJC_HRP_Lose
    
    DRP_Gain = p_v*(1-p_E)*(p_a**n)*(pi - C_v) 
    DRP_Lose = p_v*(1-p_E)*(1-p_a**n)*(-NDD-C_r-C_v)# compute incorrectly (1-p_E) get sent to mediation, finds nondeterminism (1-p_a^n) get compensation.
#     DRP_scared = p_v*(1-p_E)*(-pi-C_v) # JC doesn't want to risk getting caught, just pay RP
#     DRP =  max(DRP_Gain+DRP_Lose, DRP_scared)
    DRP = DRP_Gain + DRP_Lose
    
    
    noID_HRP = (1-p_v)*p_E*(B-pi)
    noID_DRP = (1-p_v)*(1-p_E)*(-pi)
    noVerifyCost = noID_HRP + noID_DRP
    
    JCnonDet = JC_HRP + ND + DRP + noVerifyCost
    return JCnonDet

def RPU(n,PR,p_a,pi,p_v=1,C_f=0,p_E=1, ec=0):
    p2 = 1-p_a
#     D = pi * PR
#     D = pi * PR + pi*n
    D = pi * PR + (pi * n)
    
    
    execCost = ec

    #Cj is cost of execution for fraudulant job, picking a random number or "close" value
    fraudCost = C_f
    
    JC_HRP = p_v*p_E*p_a*(pi-execCost) #compute answer 1 (p_a) correctly (p_E) receive reward (pi)
    
    NDJC_HRP_Gain = p_v*p_E*p2*(1-p_a**n)*(pi-execCost) #compute answer 2 (p2) correctly (p_E) get sent to mediation, who finds nondeterminism (1-p_a^n) and recieve reward (r) and nonDet bonus (f2-f)*k kickback=k
    NDJC_HRP_Lose = p_v*p_E*p2*(p_a**n)*(-D-execCost) # compute answer 2 correctly (p2) get sent to mediation, who doesn't find nondeterminsm (p_a^n) and get fined (f)
    ND = NDJC_HRP_Gain + NDJC_HRP_Lose 
    
    DRP_Gain = p_v*(1-p_E)*(1-p_a**n)*(pi-fraudCost)# compute incorrectly (1-p_E) get sent to mediation, finds nondeterminism (1-p_a^n) get compensation.
    DRP_Lose = p_v*(1-p_E)*(p_a**n)*(-D-fraudCost) #compute incorrectly (1-p_E) get sent to mediation, who doesn't find nondeterminsm (p_a^n) and get fined (f)
    DRP = DRP_Gain+DRP_Lose
    
#     DRP_scared  = p_v*(1-p_E)*(pi-fraudCost) # JC doesn't want to risk getting caught, just pay RP. 
    
    #  noVerifypayout = (1-p_v)*pi #compute answer 1 (p_a) correctly (p_E) receive reward (pi)
    noID_HRP = (1-p_v) * p_E * (pi - execCost)
    noID_DRP = (1-p_v) * (1-p_E) * (pi - fraudCost)
    noID = noID_HRP + noID_DRP
    
    
    RCnonDet = JC_HRP + ND + DRP + noID
    return RCnonDet



class plotter():
    def __init__(self,xlabel):

        self.points = 51
        self.x,step = np.linspace(0,1,self.points,retstep=True)

        plt.ioff()

        self.fig, self.ax = plt.subplots()
        
        self.xlabel = xlabel
        # self.ax.grid(visible=True)
        # self.ax.set_xlabel(xlabel)
        # self.ax.set_ylabel('Util') 

        self.old = False
        

    def trace(self, n,PR,C_r,   B,pi,p_v,C_v,       C_f,p_E,ec):

        self.ax.cla()
        self.ax.grid(visible=True)
        self.ax.set_xlabel(self.xlabel)
        self.ax.set_ylabel('Util')
        p_a=np.ones(self.points)

        yHJU = JCU(n=n,PR=PR,C_r=C_r,     p_a=p_a,B=B,pi=pi,p_v=p_v,C_v=C_v,        p_E=p_E)

        if self.xlabel == "p_v":
            p_v = self.x
        elif self.xlabel == "p_E":
            p_E = self.x
        elif self.xlabel == "p_a":
            p_a = self.x

        
        yLJU = JCU(n=n,PR=PR,C_r=C_r,     p_a=p_a,B=B,pi=pi,p_v=p_v,C_v=C_v,        p_E=p_E)
        yLRU = RPU( n=n,PR=PR,     p_a=p_a,pi=pi,p_v=p_v,        C_f=C_f,p_E=p_E,ec=ec)

        ydU = yLJU - yHJU
        
        # if self.old:
        #     # self.lineJU[0].set_ydata(yLJU) 
        #     # self.lineRU[0].set_ydata(yLRU) 
        #     self.ax.lines = [] 

        # else:
        #     self.old = True
        #     # self.lineJU = self.ax.plot(self.x,yLJU, label="LJU")
        #     # print(type(self.lineJU))
        #     # print(self.lineJU)
        #     # self.lineRU = self.ax.plot(self.x,yLRU, label="LRU")
        #     # self.old = True
        
        self.ax.plot(self.x,yHJU, label="HJU")
        self.ax.plot(self.x,yLJU, label="LJU")
        self.ax.plot(self.x,yLRU, label="LRU")
        self.ax.plot(self.x,ydU, label="ydU")

        
        

        self.fig.legend()

        # plt.ion()

        display(self.fig)

        print("H gain: ", max(yHJU))
        print("D Gain: ", max(yLJU))
        print("D - H : ", max(yLJU)-max(yHJU))
        print("p_a for max util: " , self.x[np.argmax(yLJU)])