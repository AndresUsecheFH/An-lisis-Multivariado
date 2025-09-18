library(psych)
#Prueba de Bartlett
cortest.bartlett(carros)

#criterio KMO
KMO(carros)

#para el diagrama de pendiente
#trabajemos con datos estandarizados
#es decir con la matriz de correlaciones
#matriz de correlaciones
carros_cor=cor(carros)

#valores propios
vp_carros=eigen(carros_cor)

#varianza explicada por cada factor 
round(vp_carros$values,2)
#(proporcion)
round(vp_carros$values/sum(vp_carros$values),3)

#varianza acumulada por los factores
cumsum(vp_carros$values)

#proporcion de varianza acumulada
round(cumsum(vp_carros$values)/sum(vp_carros$values),3)

#grafico de pendiente
plot(vp_carros$values,type="b",ylab="valores propios",xlab="factor",
     cex.axis=1.5,cex.lab=1.5,cex=1.5,pch=16 )
axis(1,at=seq(1,9,by=2))

#otra forma

scree(carros_cor,pc=FALSE)

#analisis factorial
#se ingresa la matriz de correlaciones

fa_carros <- fa(r=carros_cor, 
              nfactors = 3, 
              # covar = FALSE, SMC = TRUE,son los implicitos
              #covar=FALSE porque se esta usando la correlacion
              #SMC es como obtener el valor inicial para las 
              #iteraciones (TRUE usa las correlaciones)
              fm="pa", # tipo de AF que usaremos ("pa" es principal axis factoring)
              max.iter=100, # (50 es el número de iteraciones programadas (default)
              #acá se cambio a 100
              rotate="varimax") # none no rotacion, "varimax" rota con ese criterio
print(fa_carros)

#grafico de factores y cargas
fa.diagram(fa_carros)
