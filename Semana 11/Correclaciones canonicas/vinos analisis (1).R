#paquetes a utilizar
library(CCA)
library(CCP)

#estandarizacion
vinE=scale(vino)

#bases de datos para cada grupo

vinX=vinE[,c(4,5,6)]
vinY=vinE[,c(1,2,3,7)]

#calculos de las CC
cc_vino=cc(vinX,vinY)

#coeficientes de las var C
cc_vino$xcoef
cc_vino$ycoef


#correlaciones canonicas
cc_vino$cor

#Prueba de Wilks
r=cc_vino$cor
n=1599
p=3
q=4
p.asym(r,n,p,q,tstat="Wilks")

#correlaciones con cada dimensión
library(corrplot)

#Recordar
#calculos de las CC
#cc_vino=cc(vinX,vinY)
#bases de datos para cada grupo
#vinX=vinE[,c(4,5,6)]
#vinY=vinE[,c(1,2,3,7)]

#variables canonicas 1

#inferencia en X
U1=cc_vino$scores$xscores[,1]
cor(U1,vinX)

vin_X=data.frame(U1,vinX)
I_X=cor.mtest(vin_X,conf.level=0.95)

#valores-p (solo la 1ra varC)
round(I_X$p[1,],4)

#Intervalos de confianza
#IC, limite inferior
round(I_X$lowCI[1,],2)
#IC, limite inferior
round(I_X$uppCI[1,],2)


#inferencia en Y
V1=cc_vino$scores$yscores[,1]
cor(V1,vinY)

vin_Y=data.frame(V1,vinY)
I_Y=cor.mtest(vin_Y,conf.level=0.95)

#valores-p (solo la 1ra varC)
round(I_Y$p[1,],4)

#Intervalos de confianza
#IC, limite inferior
round(I_Y$lowCI[1,],2)
#IC, limite inferior
round(I_Y$uppCI[1,],2)

#variables canonicas 2

#inferencia en X
U2=cc_vino$scores$xscores[,2]
cor(U2,vinX)

vin_X=data.frame(U2,vinX)
I_X=cor.mtest(vin_X,conf.level=0.95)

#valores-p (solo la 1ra varC)
round(I_X$p[1,],4)

#Intervalos de confianza
#IC, limite inferior
round(I_X$lowCI[1,],2)
#IC, limite inferior
round(I_X$uppCI[1,],2)


#inferencia en Y
V2=cc_vino$scores$yscores[,2]
cor(V2,vinY)

vin_Y=data.frame(V2,vinY)
I_Y=cor.mtest(vin_Y,conf.level=0.95)

#valores-p (solo la 1ra varC)
round(I_Y$p[1,],4)

#Intervalos de confianza
#IC, limite inferior
round(I_Y$lowCI[1,],2)
#IC, limite inferior
round(I_Y$uppCI[1,],2)

