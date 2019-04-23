#Nick Troutman



JCnondet <- function(p1,b1,r,p2,b2,pj,p,n,c,f,q)
{
  return(-p1*p*cj +(p1*q)*(b1-r) +(p2*q)*(b2 - ((1-(p1^n))*(f2+r))))
}

RPnondet <- function(p1,b1,r,p2,b2,pj,p,n,c,f,q)
{
  return(-c+ p1*q*r + p2*q*(1-p1^n)*(r+ (f2-f)*k) - p2*q*(p1^n)*f -(1-q)*p1*p*f*(p1^n) + (1-q)*(p1*p)*(1-p1^n)*r + (1-q)*p2*r*(1-p1^n) -(1-q)*p2*(p1^n)*f)
}


Utility <- function(p1,b1,r,p2,b2,pj,p,n,c,f,q)
{
  
  JCnondet = JCnondet(p1,b1,r,p2,b2,pj,p,n,c,f,q)
  #general 
  #RPcc = -c+ p1*q*r + p2*q*(1-p1^n)*(r+ (f2-f)*k) - p2*q*(p1^n)*f - (1-q)*f
  
  RPnondet = RPnondet(p1,b1,r,p2,b2,pj,p,n,c,f,q)
  #ASSUMPTIONS: JC can discern between [Answer1,Answer2, and Junk answer]
  #ASSUMPTIONS: JC gets value from Answ1, and Answ2
  #ASSUMPTIONS: Mediator never makes mistakes, just always runs correctly
  
  #compute regular Utility without determinism
  JCcc0 = q*(b1-r) - p*cj
  
  RPcc0 = q*r -(1-q)*f -c 
  
  return(c(JCnondet, RPnondet,JCcc0,RPcc0))
}




#variables
p0 = 0.1 #probability that JC verifies job result
r0=1.5 #Reward paid to RP for completing job
f0=r0*5 #fine amount
f20=f0*40
b10=2 #JC's benefit from receiving job result 1
b20=2 #JC's benefit from receiving job result 2
c0=1 #cost to run jobf
cj0 = c0 #cost of j0
pj0=0.999 #probability that JC correctly identifies wrong result
q0=0.999 #probability that RP runs job correctly when trying
n0=7 #number of runs that mediator runs
k0=1 #kickback for nondeterminism being paid to RP

#initialize this run
p=p0
r=r0
f=f0
f2=f20
b1=b10
b2=b20
c=c0
#cd=cd0
cj=cj0
pj=pj0
#pm=pm0
q=q0
n=n0
k=k0


#CHANGES:

#Assume that we have Non-Deterministic job
#jobs are non-deterministic
#Now mediator runs n trials
#if mediator ever gets a non-1 answr then flag JC,
#if meidaotr only gets 1-aswer then flag RP


#New Utility Functions

#Only call mediator when recieving answer 2
#pj*p is prob of getting checked and sent to mediator



#value = Utility(p1,b1,r,p2,b2,pj,p,n,c,f)


p=p0
r=r0
f=f0
b1=b10
b2=b20
c=c0
#cd=cd0
cj=cj0
pj=pj0
#pm=pm0
q=q0

p1=0.9 #probability that job's answer is "1"
p2=1-p1 #probability that job's ansewr is "2"


JCcclist=c()
RPcclist=c()
p2list=c()

JCcc0list=c()
RPcc0list=c()

#Test different values of p1 & p2 (probability of nondeterminism)
for (i in 1:1001)
{
  p1 = (i-1)/1000
  p2= 1-p1
  
  value = Utility(p1,b1,r,p2,b2,pj,p,n,c,f, q)
  
  JCcclist=c(JCcclist, value[1])
  RPcclist=c(RPcclist, value[2])
  p2list=c(p2list,p2)
  
  JCcc0list=c(JCcc0list, value[3])
  RPcc0list=c(RPcc0list, value[4])
  
}
p2list=1-p2list

p2Changes=data.frame("JCcc"= JCcclist, "RPcc"= RPcclist, "p2" = p2list )

p2Changes2=data.frame("JCcc"= JCcclist, "RPcc"= RPcclist, "p1" = p2list, "JCcc" = JCcc0list, "RPcc" = RPcc0list)



write.csv(p2Changes2, file="nonDetUtility", row.names = FALSE)




i=match(max(JCcclist),JCcclist)
p1=(i/1000-1/1000)
p2=1-p1



miny=0
if (min(JCcclist) < min(RPcclist))
{miny=min(JCcclist)} else {miny=min(RPcclist)}

maxy=0
if (max(JCcclist) > max(RPcclist))
{maxy=max(JCcclist)} else {maxy=max(RPcclist)}

plot(p2list, JCcclist, main="Non-Determinism Analysis", type="l", col="red", ylim=c(miny,maxy), xlab="Probability Job returns Answer1 = p1, [1-p1 = p2]", ylab="Expected Utility")
#plot(1-p2list, JCcclist, main="Non-Determinism Analysis", type="l", col="red", ylim=c(0.35,0.45), xlab="Probability Job returns Answer1 = p1, [1-p1 = p2]", ylab="Expected Utility")

lines(p2list, RPcclist,col="blue", lty=1 )
#original values
lines(p2list, JCcc0list, col="red", lty=2)
lines(p2list, RPcc0list, col="blue", lty=2)

text(0.2,-60, paste0("JCnonDet - JCcc (want -) =", round(max(JCcclist) - JCcc0list[1001],11)))
text(0.3,max(RPcclist)*0.25, paste0("RPnonDet - RPcc (want +) =", round(min(RPcclist)-RPcc0list[1001],11)))

text(0.2,max(RPcclist)*0.5, paste0("Mediator Iterations =", round(n0,11)))

par(xpd=TRUE)
legend(0.7,-10, legend=c("JC-nonDet", "RP-nonDet", "JCcc", "RPcc"), col=c("red", "blue", "red","blue"), lty=c(1,1,2,2), cex=0.7, pt.cex = 0.3)


if(max(JCcclist)>max(JCcc0list))
{print("CHEATING POSSIBLE")}





cheatAmount=c()
numTrials=7

for (j in 1:numTrials)
{
  #run simulation
  n=j
  p=p0
  c=c0
  
  JCcclist=c()
  RPcclist=c()
  p2list=c()
  
  JCcc0list=c()
  RPcc0list=c()
  
  
  #Test different values of p1 & p2 (probability of nondeterminism)
  for (i in 1:1001)
  {
    p1 = (i-1)/1000
    p2= 1-p1
    
    value = Utility(p1,b1,r,p2,b2,pj,p,n,c,f, q)
    
    JCcclist=c(JCcclist, value[1])
    RPcclist=c(RPcclist, value[2])
    p2list=c(p2list,p2)
    
    JCcc0list=c(JCcc0list, value[3])
    RPcc0list=c(RPcc0list, value[4])
    
  }
  #get difference
  
  
  
  cheatAmount=c(cheatAmount,max(JCcclist)-JCcc0list[100])
}
#plot difference

#plot(1:numTrials,cheatAmount,ylim=c(min(cheatAmount),max(cheatAmount)), type="l", main="JC Utility Advantage of nonDeterminism VS. Mediator Iterations", ylab="JC Utility Advantage", xlab="Mediator Iterations" )
#points(1:numTrials,cheatAmount, pch=19)
#text(1:numTrials + 0.1,cheatAmount+max(cheatAmount)*0.05,labels=round(cheatAmount,6), srt=30, cex=(0.8))



mediterations=data.frame("JCadv" = cheatAmount, "Iterations"=1:numTrials)



write.csv(mediterations, file="nonDetMedIterations", row.names = FALSE)
