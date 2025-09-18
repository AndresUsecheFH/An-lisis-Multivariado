#prueba de Box
#paquete mvhtests
library(mvhtests)

#se necesita digitar en data y group las posiciones de las variables
#visualización de la base de datos

head(ciudadesM)

#instruccion prueba de Box
x=as.matrix(ciudadesM[ ,2:7])
ina=ciudadesM[ ,1]
Mtest.cov(x,ina)

#detalles de las instrucciones
#x matriz de datos
#ina vector con los grupos de datos

