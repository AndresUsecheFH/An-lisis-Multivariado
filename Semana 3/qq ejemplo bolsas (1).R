#ejemplo bolsas

#prueba de Anderson-Darling
#paquete nortest
library(nortest)
ad.test(cont)

#prueba de shapiro wilks
shapiro.test(cont)

#grafico QQ
#paquete stats
library(stats)
qqnorm(cont,xlab="percentiles teóricos",ylab="percentiles muestrales",
       pch=16,cex=2,
       cex.lab=1.5,cex.axis=1.5, main="grafico QQ")
#para la línea de referencia
qqline(cont,distribution=qnorm,col="red",lwd=3)

#explicación: 
#pch (símbolo de los puntos) /cex (tamaño de los puntos)
#cex.lab (tamaño letras ejes) /cex.axis (tamaño numeros de los ejes)
#xlim y ylim (rango de valores a considerar en los ejes)

#análisis de los percentiles 

#parametros de la distribucion normal
m=mean(cont)
s=sd(cont)

#numero de datos
n=length(cont)

#ordena los datos
cont_ord=sort(cont)

#percentiles teóricos
i=1:n
orden=(i-0.5)/n
q=qnorm(orden,m,s)

#correlación entre datos (percentiles muestrales) y percentiles
#teoricos
cor.test(cont_ord,q,conf.level=0.99)


