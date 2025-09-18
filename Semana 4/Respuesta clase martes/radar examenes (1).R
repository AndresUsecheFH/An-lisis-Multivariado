#digitar
M=matrix(nr=4,nc=5)
data.entry(M)

#nombres
rownames(M)=c("a", "b","C","d")
colnames(M)=c("A1","A2","A3","A4","A5")

#los datos deben estandarizarse
library(BBmisc)

#los estandarizo
M1=normalize(M,method="range",range=c(0.5,1.5),margin=2)

#agrego maximos (1.5) y minimos (0.5)
f1=rep(1.5,4)
f2=rep(0.5,4)
#datos finales
M1F=rbind(f1,f2,M1)
#lo convierto a marco de datos
D=as.data.frame(M1F)
#le pongo nombre a las filas que agregue?
rownames(D)[1]="max"
rownames(D)[2]="min"


#graficos, son 4
#cargo el paquete
library(fmsb)

#para poner los graficos juntos (decido la posicion)
par(mfrow=c(2,2))

#grafico de estudiante a (filas 1,2 y 3)
radarchart(D[1:3,],pcol="red",plwd=3,vlcex = 1.2,title="est a")

#grafico de estudiante b (filas1,2,4)
x=c(1,2,4)
radarchart(D[x,],pcol="blue",plwd=3,vlcex = 1.2,title="est b")

#grafico de estudiante c (filas1,2,4)
x=c(1,2,5)
radarchart(D[x,],pcol="green",plwd=3,vlcex = 1.2,title="est f")

#grafico de estudiante b (filas1,2,4)
x=c(1,2,6)
radarchart(D[x,],pcol="magenta",plwd=3,vlcex = 1.2,title="est j")
