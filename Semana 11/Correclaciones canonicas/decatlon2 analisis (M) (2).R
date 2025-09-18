#paquetes a utilizar
library(CCA)
library(CCP)

#estandarizacion
DC2_E=scale(DC2)
DC2_E=as.data.frame(DC2_E)

#base de datos para cada grupo de variables
DC2_E_X=DC2_E[,c(1,4,5,7)]
DC2_E_Y=DC2_E[,c(2,3,6)]

#calculo de las CC
cc_DC2=cc(DC2_E_X,DC2_E_Y)

#coeficientes de las var C
cc_DC2$xcoef
cc_DC2$ycoef

#correlacines canonicas
cc_DC2$cor

#Prueba de Wilks
r=cc_DC2$cor
n=23
p=4
q=3
p.asym(r,n,p,q,tstat="Wilks")

#correlaciones dimension 1 con variables
library(corrplot)

#inferencia en X
U1=cc_DC2$scores$xscores[,1]
cor(U1,DC2_E_X)


DC2_X=data.frame(U1,DC2_E_X)
I_X=cor.mtest(DC2_X,conf.level=0.95)

#valores-p (solo la 1ra varC)
round(I_X$p[1,],4)

#Intervalos de confianza
#IC, limite inferior
round(I_X$lowCI[1,],2)
#IC, limite inferior
round(I_X$uppCI[1,],2)


#inferencia en Y
V1=cc_DC2$scores$yscores[,1]
cor(V1,DC2_E_Y)

DC2_Y=data.frame(V1,DC2_E_Y)
I_Y=cor.mtest(DC2_Y,conf.level=0.95)

#valores-p (solo la 1ra var C)
round(I_Y$p[1,],4)

#Intervalos de confianza
#IC, limite inferior
round(I_Y$lowCI[1,],2)
#IC, limite inferior
round(I_Y$uppCI[1,],2)