#paquetes a utilizar
library(CCA)
library(CCP)

#estandarizacion
C_E=scale(C)
C_E=as.data.frame(C_E)

#base de datos para cada grupo de variables
C_E_X=C_E[,c(1,4,5,6)]
C_E_Y=C_E[,c(2,3)]

#calculos de las CC
cc_CE=cc(C_E_X,C_E_Y)


#correlaciones canonicas
cc_CE$cor

#Prueba de Wilks
r=cc_CE$cor
n=41
p=4
q=2
p.asym(r,n,p,q,tstat="Wilks")


