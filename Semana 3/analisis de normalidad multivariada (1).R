#analisis base de datos ciudades

#paquete MVN

library(MVN)
#Prueba de Royston

mvn(ciudades,mvnTest="royston")

#grafico ji-cuadrado
mvn(ciudades,multivariatePlot="qq")
