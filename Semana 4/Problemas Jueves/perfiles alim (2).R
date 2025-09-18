#base de datos alim. Perfil poligonal
#paso 1 calculo las medias

attach(alim)
#cuantos tamaños de familia hay
table(miembros)

#construccion de una matriz con las medias
#calculo de las medias
P2=colMeans(alim[miembros==2,-3])
P3=colMeans(alim[miembros==3,-3])
P4=colMeans(alim[miembros==4,-3])
P5=colMeans(alim[miembros==5,-3])

M=rbind(P2,P3,P4,P5)

#la matriz
M=as.matrix(M)

#el grafico
library(profileR)

profileplot(M,person.id=rownames(M),standardize=FALSE)
