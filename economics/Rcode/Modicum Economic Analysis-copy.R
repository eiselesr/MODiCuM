#Nick Troutman
print("RUNNING Financial Analysis of Modicum\n")

UtilityInequalities <- function(p,r,b,c,cd,cj,pj,pm, q, zc, zd)
{
  #Get utility values
  #CC
  #JCcc = -r*(1-zc) +q*b -p*(cj+(1-q)*pj*(1-pm)*f) #old
  JCcc = q*(b-r) + (1-q)*f - p*cj
  
  #RPcc = r*(1-zc)-c-zc*f #old
  RPcc = q*r -(1-q) *f -c 
  
  #CD
#  JCcd = -r*(1-zd)-p*(cj+pj*(1-pm) *f)
  JCcd = -(1-p*pj)* + p*pj*f - p*cj
  
 # RPcd = -cd + r*(1-zd) - zd*f
  RPcd = (1-p*pj) * r - p*pj *f -cd
  
  #DC
#  JCdc = q*b - (r+f)*pm
 JCdc = q*(b+(1-pm)*f-pm*(f+r))+(1-q)*f
#  RPdc= -c+r*((1-q)*(1-pm)+q*pm)-f*(q*(1-pm)+(1-q)*pm)
 RPdc =  q*(pm*(f+r)-(1-pm)*f)-(1-q)*f  
  
  #DD
#  JCdd = -(r+f)*(1-pm)
 JCdd  = -(r+f)*(1-pm)
  
 # RPdd =  -cd -f *pm
  RPdd = -(r+f)*(1-pm)
  #Inequalities
  valid=1
  
  #JCcd & JCcc >0
  if (JCcc<0)
  {valid=0}
  #if (JCcd<0)
  #{valid=0}
  
  #JCdc<JCcc
  if (JCdc>JCcc)
  {valid=0}
  #JCdd < JCcd
  if (JCdd > 0)
  {valid=0}
  
  #RPcc>0
  if (RPcc < 0)
  {valid=0}
  #RPcd < RPcc
  if (RPcd>RPcc)
  {valid=0}
  #RPdd < RPdc
  if (RPdd > RPcc)
  {valid=0}
  if(JCdd > JCcc)
  {valid=0}
  
  
  return(valid)
}

#variables
p0 = 0.1
r0=1.5
f0=r0*8
b0=2
c0=1
cd0=0.1
cj0 = 2*c0
pj0=0.9
pm0=0.9
q0=0.99
zc0 = p0*(1-q0)*pj0*pm0
zd0 = p0*pj0*pm0


#variables
p = p0
r=r0
f=f0
b=b0
c=c0
cd=cd0
cj = cj0
pj=pj0
pm=pm0
q=q0
zc = p*(1-q)*pj*pm
zd = p*pj*pm


#Utility Functions

JCcc = q*(b-r) + (1-q)*f - p*cj

#RPcc = r*(1-zc)-c-zc*f #old
RPcc = q*r -(1-q) *f -c

#CD
#  JCcd = -r*(1-zd)-p*(cj+pj*(1-pm) *f)
JCcd = -(1-p*pj)* + p*pj*f - p*cj

# RPcd = -cd + r*(1-zd) - zd*f
RPcd = (1-p*pj) * r - p*pj *f -cd

#DC
#  JCdc = q*b - (r+f)*pm
JCdc = q*(b+(1-pm)*f-pm*(f+r))+(1-q)*f
#  RPdc= -c+r*((1-q)*(1-pm)+q*pm)-f*(q*(1-pm)+(1-q)*pm)
RPdc =  q*(pm*(f+r)-(1-pm)*f)-(1-q)*f  

#DD
#  JCdd = -(r+f)*(1-pm)
JCdd  = -(r+f)*(1-pm)

# RPdd =  -cd -f *pm
RPdd = -(r+f)*(1-pm)


#Check for profit and Nash Points

if (JCcc>0){
  print(paste0("JCcc = ", JCcc))
}else{
  print("ERORR JCcc<0")
}

if (JCcc>JCdc){
  print("JC Nash Point")
}else {print("ERROR, JC not Nash Point")}

if (RPcc>0)
{
  print(paste0("RPcc = ",RPcc))
} else {print("Error, RPcc<0")}

if (RPcc>RPcd)
{
  print("RP Nash Point")
}else{ print("ERROR, RP not NASH POINT")}

valid = UtilityInequalities(p,r,b,c,cd,cj,pj,pm,q,zc,zd)
print("VALIDITY:")
print(valid)



#Vary values to create graphs

#p, probability of checking

p2 = (1:1001)/ 1000 - 0.001
resultp2 = 1:1001
resultp2r = 1:1001
p2valid=1:1001

for (i in 1:1001)
{
  zc = p2[i]*(1-q)*pj*pm
  zd = p2[i]*pj*pm
  p = p2[i]
  resultp2[i]= q*(b-r) + (1-q)*f - p*cj
  resultp2r[i] =  q*r -(1-q) *f -c
  
  p2valid[i] = UtilityInequalities(p,r,b,c,cd,cj,pj,pm, q, zc, zd)
  
}

#variables
p = p0
r=r0
f=f0
b=b0
c=c0
cd=cd0
cj = cj0
pj=pj0
pm=pm0
q=q0
zc = p*(1-q)*pj*pm
zd = p*pj*pm

#vary fine
fine2 = (1:1001)/ 10  +0.98
fine2j = 1:1001
fine2r = 1:1001
fine2valid = 1:1001

for (i in 1:1001)
{

  f=fine2[i]
  fine2j[i]= q*(b-r) + (1-q)*f - p*cj
  fine2r[i] =  q*r -(1-q) *f -c
  fine2valid[i] = UtilityInequalities(p,r,b,c,cd,cj,pj,pm, q, zc, zd)
  }


#variables
#variables
p = p0
r=r0
f=f0
b=b0
c=c0
cd=cd0
cj = cj0
pj=pj0
pm=pm0
q=q0
zc = p*(1-q)*pj*pm
zd = p*pj*pm

#vary detection rate
d2 = (1:1001)/ 1000 - 0.001
d2j = 1:1001
d2r = 1:1001
d2valid = 1:1001

for (i in 1:1001)
{
  
  pj=d2[i]
  pm=d2[i]
  zc = p*(1-q)*pj*pm
  zd = p*pj*pm
  
  d2j[i]= q*(b-r) + (1-q)*f - p*cj
  d2r[i] = q*r -(1-q) *f -c

  d2valid[i]=  UtilityInequalities(p,r,b,c,cd,cj,pj,pm, q, zc, zd)
  }

#Create Inequality Comparison

DFp2 = data.frame("Probability" = p2,"JC" = resultp2, "RP"= resultp2r,   "RedBlack" = p2valid)
DFfine2 = data.frame("Fine" = fine2,"JC" = fine2j, "RP" = fine2r, "RedBlack" = fine2valid)
DFd2 = data.frame("Detection" = d2, "JC" = d2j,"RP" = d2r, "RedBlack" = d2valid)

par(mfrow=c(3,2))
plot(DFp2$Probability, DFp2$JC, col=ifelse(DFp2$RedBlack==1,'black','red'),pch=19, main="JC with varying p", xlab="p", ylab="Expected Utility of JC")
plot(DFp2$Probability, DFp2$RP, col=ifelse(DFp2$RedBlack==1,'black','red'),pch=19, main="RP with varying p", xlab="p", ylab="Expected Utility of RP")
plot(DFfine2$Fine, DFfine2$JC, col=ifelse(DFfine2$RedBlack==1,'black','red'),pch=19,  main="JC with varying f", xlab="f", ylab="Expected Utility of JC")
plot(DFfine2$Fine, DFfine2$RP, col=ifelse(DFfine2$RedBlack==1,'black','red'),pch=19, main="RP with varying f", xlab="f", ylab="Expected Utility of RP")
plot(DFd2$Detection, DFd2$JC, col=ifelse(DFd2$RedBlack==1,'black','red'),pch=19, main="JC with varying Detection Rate", xlab="pj & pm", ylab= "Expected Utility of JC")
plot(DFd2$Detection ,DFd2$RP, col=ifelse(DFd2$RedBlack==1,'black','red'),pch=19, main="RP with varying Detection Rate", xlab="pj & pm", ylab="Expected Utility of RP")
mtext(font=2, "Expected Utility of JC & RP (Cj, Cr)", side = 3, line = -1.5, outer = TRUE)


dev.off()
#V2 plot
  
  m <- matrix(c(1,2,3,4,5,6),nrow = 3,ncol = 2,byrow = TRUE)

layout(mat = m,heights = c(0.5,0.5,0.5), width = c(1,1))
margins = 0.51
par(oma=c(3,1,2.,1), mai=c(margins,margins,margins,margins)) #Change individual plot margins
plot(DFp2$Probability, DFp2$JC, col=ifelse(DFp2$RedBlack==1,'black','red'),pch=19, main="JC with varying p", xlab="p", ylab="Expected Utility of JC")
plot(DFp2$Probability, DFp2$RP, col=ifelse(DFp2$RedBlack==1,'black','red'),pch=19, main="RP with varying p", xlab="p", ylab="Expected Utility of RP")
plot(DFfine2$Fine, DFfine2$JC, col=ifelse(DFfine2$RedBlack==1,'black','red'),pch=19,  main="JC with varying f", xlab="f", ylab="Expected Utility of JC")
plot(DFfine2$Fine, DFfine2$RP, col=ifelse(DFfine2$RedBlack==1,'black','red'),pch=19, main="RP with varying f", xlab="f", ylab="Expected Utility of RP")
plot(DFd2$Detection, DFd2$JC, col=ifelse(DFd2$RedBlack==1,'black','red'),pch=19, main="JC with varying Detection Rate", xlab="pj & pm", ylab= "Expected Utility of JC")
plot(DFd2$Detection ,DFd2$RP, col=ifelse(DFd2$RedBlack==1,'black','red'),pch=19, main="RP with varying Detection Rate", xlab="pj & pm", ylab="Expected Utility of RP")

plot_colors <- c("black", "red")
#legend(x = "bottom",inset = c(-4,-0.5),  legend = c("Nash Equilibrium Point", "Not Equilibrium"),  col=plot_colors, lwd=6, cex=.8, horiz = TRUE, xpd=TRUE)


par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("bottom", c("Equilibrium", "No Equilibrium"), xpd = TRUE, horiz = TRUE, inset = c(0, 0), bty = "n", pch = c(19), col = plot_colors, cex = 2)
mtext(font=2, cex=1.7, "Expcted Utility of JC & RP (Cj, Cr)", side = 3, line = -2.5, outer = TRUE)

#1=red
#2=Black

#split into sepearte csv file
Detection <-split(DFd2, f = DFd2$RedBlack)
Fine <-split(DFfine2, f=DFfine2$RedBlack)
Probability <-split(DFp2, f=DFp2$RedBlack)

#write to csv

write.csv(Detection[1], file="DetectionRed.csv", row.names = FALSE)
write.csv(Detection[2], file="DetectionBlack.csv", row.names = FALSE)


write.csv(Fine[1], file="FineRed.csv", row.names = FALSE)
write.csv(Fine[2], file="FineBlack.csv", row.names = FALSE)


write.csv(Probability[1], file="ProbabilityRed.csv", row.names = FALSE)
write.csv(Probability[2], file="ProbabilityBlack.csv", row.names = FALSE)
