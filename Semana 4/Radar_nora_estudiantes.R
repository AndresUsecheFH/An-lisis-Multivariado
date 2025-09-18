M = matrix(nr=4,nc=5)
data.entry(M)

rownames(M)<-c("a","b","c","d")
colnames(M)<-c("A1","A2","A3","A4","A5")

library(BBmisc)

M1 = normalize(M,method="range",range=c(0.5,1.5),margin=2)

f1=rep(1.5,5)
f2=rep(0.5,5)
ME=rbind(f1,f2,M1)
D=as.data.frame(ME)

rownames(D)[1]="Max"
rownames(D)[2]="Min"

par(mfrow=c(2,2))

D
x<-c(1,2,3)
radarchart(D[x,],pcol="red",plwd=3,vlcex=1.2,title="Estudiante a")
x<-c(1,2,4)
radarchart(D[x,],pcol="blue",plwd=3,vlcex=1.2,title="Estudiante b")
x<-c(1,2,5)
radarchart(D[x,],pcol="green",plwd=3,vlcex=1.2,title="Estudiante c")
x<-c(1,2,6)
radarchart(D[x,],pcol="pink",plwd=3,vlcex=1.2,title="Estudiante d")
