#divido los datos
G1=contenedor[contenedor$gp==1,2:10]
G2=contenedor[contenedor$gp==2,2:10]

#medias
m_G1=colMeans(G1)
m_G2=colMeans(G2)

#Uso los máximos y mínimos de los datos (no estandarizo inicialment)
max=apply(contenedor[,-1],2,max)
min=apply(contenedor[,-1],2,min)

M=rbind(max,min,m_G1,m_G2)

#lo convierto a marco de datos
D=as.data.frame(M)

#cargo el paquete
library(fmsb)

#para poner los graficos juntos (decido la posicion)
par(mfrow=c(2,1))

#grafico de contenedor 1 (filas 1,2 y 3)
radarchart(D[1:3,],pcol="red",plwd=3,vlcex = 1.2,title="contenedor 1")

#grafico de contenedor 2 (filas1,2,4)
x=c(1,2,4)
radarchart(D[x,],pcol="blue",plwd=3,vlcex = 1.2,title="contendor 2")