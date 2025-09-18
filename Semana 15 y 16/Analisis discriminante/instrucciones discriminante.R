attach(D)
#PAQUETE
library(MASS)

#CONSTRUCCION MODELO FUNCI?N DISCRIMINANTE
m=lda(grupo~FL+RW+CL+CW+BD)

#visualizar la salida
m

#grafico general: graficar los cangrejos (grupos) segun los criterios definidos 
#por las funciones discriminantes

#asigno colores caracteristicos a cada grupo
color=c("red","black","blue","green")
#represento los cangrejos en las distintas dimensiones. Grupo es la 
#variable de agrupacion
plot(m,col=color[grupo])

#escojo las dos primeras dimensiones y grafico

#primero hay que determinar las coordenadas de los datos en las dos
#primeras funciones discriminantes
g=predict(m,dimen=2)$x

#asigno colores caracteristicos a cada grupo
color=c("red","black","blue","green")

#despues el grafico utilizando esas coordenadas y los colores

plot(g,col=color[grupo],xlab="1a funcion disc",ylab="2da funcion disc",
     pch=16,cex.lab=1.5,cex.axis=1.5)

#dibujo los ejes
abline(h=0,col="black",lty=2)
abline(v=0,col="black",lty=2)

#leyenda
legend("topright",legend=c("grupo 1","grupo 2","grupo 3","grupo 4"),
       col=c("red","black","blue","green"),lty=1,cex=0.8)



#para analizar el modelo, lo uso para clasificar los cangrejos
prediccion=predict(m)$class

# y despues se compara este pronostico con la realidad
table(grupo,prediccion)

#Para estimar a que grupo deberia pertencer un cangrejo

#hago un marco de datos con los valores
PP=data.frame(FL=11.1,RW=6.8,CL=20,CW=23,BD=10)

#hago el pronostico
#opcion 1 da todas las probabilidades y yo debo identificar
predict(m,PP)


#mas concretamente el sistema hace la 
#discriminacion del cangrejo
predict(m,PP)$class