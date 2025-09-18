#paquetes a utilizar
library(CCA)
library(CCP)

#Paso 1 estandarizar las varibles
ACE=round(scale(AC),2)

#Paso 2
#crear bases de datos con cada uno de los grupos de variables
ACE_X=ACE[ , 1:3]
ACE_Y=ACE[ , 4:7]

#Paso 3 crear un objeto con los resultados
#del calculo
cc_acad=cc(ACE_X,ACE_Y)

#visualizacion de las correlaciones canonicas
cc_acad$cor

#visualización de los coeficientes
#de las variables canonicas
#$xcoef coeficientes del primer grupo de variables
#por eso se les llamo ACE_X
cc_acad$xcoef

#$ycoef coeficientes del segundo grupo de variables
#por eso se les llamo ACE_Y
cc_acad$ycoef

#Paso 4: Prueba de Wilks
#depositar en un objeto las correlaciones
r=cc_acad$cor
#registrar el número de datos (n), 
n=dim(ACE_X)[1]
n
#el numero de datos en
#el grupo X con variables psicologicas (p)
p=dim(ACE_X)[2]
p
#el grupo Y con las variables academicas (q)
q=dim(ACE_Y)[2]
q

#la prueba
p.asym(r,n,p,q,tstat="Wilks")

#valores de los individuos en las 
#variables canonicas (dimensiones)

#variables para el grupo X (psicologia)
#variables canonicas 1
U1=cc_acad$scores$xscores[,1]

#variables para el grupo Y(academicas)
#variables canonicas 1
V1=cc_acad$scores$yscores[,1]

#Para interpretar las variables canonicas
#calculamos su correlación con las variables
#que las constituyen

#paquete
library(corrplot)
#correlaciones
#variables X
#correlaciones
cor(U1,ACE_X)

#inferencias
D_X=data.frame(U1,ACE_X)
I_X=cor.mtest(D_X,conf.level=0.99)
#valor-p
round(I_X$p,4)
#IC, limite inferior
round(I_X$lowCI,2)
#IC, limite inferior
round(I_X$uppCI,2)

#variables Y
cor(V1,ACE_Y)
D_Y=data.frame(V1,ACE_Y)
I_Y=cor.mtest(D_Y,conf.level=0.99)
#valor-p
round(I_Y$p,4)
#IC, limite inferior
round(I_Y$lowCI,2)
#IC, limite inferior
round(I_Y$uppCI,2)