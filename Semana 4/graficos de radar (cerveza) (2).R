#M matriz del ejemplo 
M=matrix(c(20,40,30,30,30,10),nr=2,nc=3,byrow=TRUE)
rownames(M)=c("hombres","mujeres")
colnames(M)=c("ligera","clara","oscura")
M1=round(prop.table(M,2),3)

#desde acá es el procedimiento: 
#M1 en este caso es proporciones
#en general debe ser una matriz normalizada

#defino máximos y mínimos, en el caso de las cervezas 
# como son proporciones  
#los pongo a 0,1. utilizo el comando de repetir 
f1=rep(1,3)
f2=rep(0,3)
#datos finales
MF=rbind(f1,f2,M1)
#lo convierto a marco de datos
D=as.data.frame(MF)

#le pongo nombre a las filas que añadí
rownames(D)[1]="max"
rownames(D)[2]="min"

#cargo el paquete
library(fmsb)

#para poner los graficos juntos (decido la posición)
par(mfrow=c(2,1))

#grafico de los hombres (filas 1,2 y 3)
radarchart(D[1:3,],pcol="red",plwd=3,vlcex = 1.2,title="hombres")

#grafico de las mujeres (filas1,2,4)
x=c(1,2,4)
radarchart(D[x,],pcol="blue",plwd=3,vlcex = 1.2,title="mujeres")

#significado
#pcol: color de la linea
#plwd: ancho de la linea
#vlcex:tamaño fuente etiqueta


#otra forma
#grafico de  todos juntos y les pongo relleno
radarchart(D,pcol=c("red","blue" ),pfcol=alpha(c("pink","light blue"),0.3) ,plwd=3,vlcex = 1.2)

#para identificarlos, una leyenda
colores=c("red","blue" )
legend(x=0.7, y=1, legend = rownames(D[-c(1,2),]), pch=20 , col=colores , text.col = "grey", cex=1.2, pt.cex=3) 


