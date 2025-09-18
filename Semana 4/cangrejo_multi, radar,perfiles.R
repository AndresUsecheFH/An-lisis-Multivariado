cang
attach(cang)
table(sp)

P1 = colMeans(cang[sp=="B",-1])
P2 = colMeans(cang[sp=="O",-1])
M=rbind(P1,P2)

#la matriz
M=as.matrix(M)

#el grafico
library(profileR)

profileplot(M,person.id=rownames(M),standardize=FALSE)


#Diagrama de radar


library(BBmisc)
M1 = normalize(M,method="range",range=c(0.5,1.5),margin=2)
profileplot(M1,person.id=rownames(M),standardize=FALSE)
f1 = rep(0,5)
f2 = rep(1,5)

Me = rbind(f1,f2,M1)
D = as.data.frame(Me)
par(mfrow=c(1,2))
library(BBmisc)
Me
library(fmsb)
D
x<-c(1,2,3)
radarchart(D[x,],pcol="red",plwd=3,vlcex=1.2,title="0")
x<-c(1,2,4)
radarchart(D[x,],pcol="blue",plwd=3,vlcex=1.2,title="B")
