#base y ejercicio estadistica

attach(L)
head(L)
library(MASS)
m = lda(Est~Mat+Fis+Lit+Ef)
m

prediccion = predict(m)$class
table(Est,prediccion)


g= predict(m,dimen=1)$x

z = list("aprobado"=g[Est==1],"no aprobados"=g[Est==0])
stripchart(z,method="jitter",pch=16)
abline(v=0,col="red",lty=2,lwd=2)

PP=data.frame(Mat=1.2,Fis=2.4,Lit=4,Ef=3.9)
predict(m,PP)$posterior




#base y ejercicio vinos

attach(wine)
head(wine)
m = lda(Type~Alcohol +Malic + Ash +Alcalinity+ Magnesium +Phenols +Flavanoids +Nonflavanoids+ Proanthocyanins +Color+ Hue +Dilution +Proline)
m

prediccion = predict(m)$class
table(Type,prediccion)
g = predict(m)$class


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


#base de datos diabetes
attach(diab)
head(diab)

m = lda(Tipo ~ RI+PG)
m
prediccion=predict(m)$class
table(Tipo,prediccion)
g= predict(m,dimen=1)$x

z=list("A"=g[Tipo==1], "B"=g[Tipo==0])
stripchart(z,method="jitter",pch=16,xlim=c(-4,4), xlab="funcion D",ylab= "tipo diabetes")
abline(v=0,col="red",lty=2,lwd=2)

prediccion=predict(m)$class
table(Tipo,prediccion)

#prediccion del individuo

PP=diab[-c(81:69),-3]
data.entry(PP)
pp=as.data.frame(PP)
predict(m,PP)$posterior