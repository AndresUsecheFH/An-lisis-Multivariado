#paquetes
library(FactoMineR)
library(factoextra)

library(tibble)

df <- tibble(
  hijos = c(1,1,4,0,2),
  Saario = c(723,900,800,1205,600),
  Casa = c(60,60,80,50,65)
)



df <- as.data.frame(df)      # Convertir a data.frame base
rownames(df) <- c("I1","I2","I3","I4","I5")

df
#Preparacion de los datos estandarizacion
Arr_E=scale(df)

#calcular la matriz de disimilaridades
#usemos la distancia euclideana es el implicito
D=dist(Arr_E,method="euclidean")

#elaboracion del cluster jerarquico
C_arrestos=hclust(D,method="average")
#metodos:
#cercano - single
#lejano - complete
#media - average
#centroid - centroid

#metodo de aglomeracion 
#single: vecino mas proximo
#complete: vecino mas lejano
#average: media de  grupos
#centroid: centroide

#elaboracion del dendograma
fviz_dend(C_arrestos,cex=0.5,ylab="distancias",main="dendograma iris")

#n?mero de conglomerados a considerar
library(UniversalCVI)
I=CH.IDX(Arr_E,kmax=5,kmin=2,method="hclust_average")
plot(I$k,I$CH,pch=5,main="numero conglomerados",xlab="conglomerados",ylab="indice CH")
lines(I$k,I$CH, col = "black")
df

#cortar el dendograma en diferentes grupos
#cortar en 5 grupos
grupos=cutree(C_arrestos,k=2)
#numero de miembros en cada conglomerado
table(grupos)
#nombre de las ciudades en los grupos, ejemplo en el 1
rownames(Arr_E)[grupos==1]

#para hacer el grafico con los grupos
fviz_dend(C_arrestos,k=2,cex=0.5,k_colors= c("red","darkgreen","black","blue","magenta"),color_labels_by_k = TRUE,
          ylab="distancias",main="dendograma ciudades")

#se pueden utilizar las componentes principales para caracterizar los grupos

#obtencion de las componentes
CP_CC=PCA(df,scale.unit=TRUE,ncp=6,graph=FALSE)
fviz_screeplot(CP_CC,choise="eigenvalue",addlabels=TRUE,geom="line", ylim=c(0,80),
               xlab="dimensiones",ylab="porcentaje varianza",main="",ncp=6)
#interpretacion de los dos primeros ejes
fviz_pca_var(CP_CC,axes=c(1,2))

#representacion de los conglomerados ejes 1 y 2
fviz_cluster(list(data=df,cluster=grupos),palette=c("red","darkgreen","black","blue","magenta")
             ,ellipse.type="convex",repel=TRUE,
             show.clust.cent = FALSE,ggtheme = theme_minimal(),main="agrupaciones")

#EJERCIO CON BASE DE DATOS EXAMENES


#Preparacion de los datos estandarizacion
Ex
Arr_E=scale(Ex)

#calcular la matriz de disimilaridades
#usemos la distancia euclideana es el implicito
D=dist(Arr_E,method="euclidean")

#elaboracion del cluster jerarquico
C_arrestos=hclust(D,method="average")
#metodos:
#cercano - single
#lejano - complete
#media - average
#centroid - centroid

#metodo de aglomeracion 
#single: vecino mas proximo
#complete: vecino mas lejano
#average: media de  grupos
#centroid: centroide

#elaboracion del dendograma
fviz_dend(C_arrestos,cex=0.5,ylab="distancias",main="dendograma iris")

#n?mero de conglomerados a considerar
library(UniversalCVI)
I=CH.IDX(Arr_E,kmax=10,kmin=2,method="hclust_average")
plot(I$k,I$CH,pch=5,main="numero conglomerados",xlab="conglomerados",ylab="indice CH")
lines(I$k,I$CH, col = "black")


#cortar el dendograma en diferentes grupos
#cortar en 5 grupos
grupos=cutree(C_arrestos,k=4)
#numero de miembros en cada conglomerado
table(grupos)
#nombre de las ciudades en los grupos, ejemplo en el 1
rownames(Arr)[grupos==1]

#para hacer el grafico con los grupos
fviz_dend(C_arrestos,k=4,cex=0.5,k_colors= c("red","darkgreen","black","blue","magenta"),color_labels_by_k = TRUE,
          ylab="distancias",main="dendograma ciudades")

#se pueden utilizar las componentes principales para caracterizar los grupos

#obtencion de las componentes
CP_CC=PCA(Ex,scale.unit=TRUE,ncp=6,graph=FALSE)
fviz_screeplot(CP_CC,choise="eigenvalue",addlabels=TRUE,geom="line", ylim=c(0,80),
               xlab="dimensiones",ylab="porcentaje varianza",main="",ncp=6)
#interpretacion de los dos primeros ejes
fviz_pca_var(CP_CC,axes=c(1,2))

#representacion de los conglomerados ejes 1 y 2
fviz_cluster(list(data=Ex,cluster=grupos),palette=c("red","darkgreen","black","blue","magenta")
             ,ellipse.type="convex",repel=TRUE,
             show.clust.cent = FALSE,ggtheme = theme_minimal(),main="agrupaciones")



#BASE DE DATOS FLORES
Arr_E=scale(flores)

#calcular la matriz de disimilaridades
#usemos la distancia euclideana es el implicito
D=dist(Arr_E,method="euclidean")

#elaboracion del cluster jerarquico
C_arrestos=hclust(D,method="complete")
#metodos:
#cercano - single
#lejano - complete
#media - average
#centroid - centroid

#metodo de aglomeracion 
#single: vecino mas proximo
#complete: vecino mas lejano
#average: media de  grupos
#centroid: centroide

#elaboracion del dendograma
fviz_dend(C_arrestos,cex=0.5,ylab="distancias",main="dendograma iris")

#n?mero de conglomerados a considerar
library(UniversalCVI)
I=CH.IDX(Arr_E,kmax=30,kmin=2,method="hclust_average")
plot(I$k,I$CH,pch=5,main="numero conglomerados",xlab="conglomerados",ylab="indice CH")
lines(I$k,I$CH, col = "black")


#cortar el dendograma en diferentes grupos
#cortar en 5 grupos
grupos=cutree(C_arrestos,k=3)
#numero de miembros en cada conglomerado
table(grupos)
#nombre de las ciudades en los grupos, ejemplo en el 1
rownames(flores)[grupos==1]

#para hacer el grafico con los grupos
fviz_dend(C_arrestos,k=3,cex=0.5,k_colors= c("red","darkgreen","black","blue","magenta"),color_labels_by_k = TRUE,
          ylab="distancias",main="dendograma ciudades")

#se pueden utilizar las componentes principales para caracterizar los grupos

#obtencion de las componentes
CP_CC=PCA(flores,scale.unit=TRUE,ncp=6,graph=FALSE)
fviz_screeplot(CP_CC,choise="eigenvalue",addlabels=TRUE,geom="line", ylim=c(0,80),
               xlab="dimensiones",ylab="porcentaje varianza",main="",ncp=6)
#interpretacion de los dos primeros ejes
fviz_pca_var(CP_CC,axes=c(1,2))

#representacion de los conglomerados ejes 1 y 2
fviz_cluster(list(data=flores,cluster=grupos),palette=c("red","darkgreen","black","blue","magenta")
             ,ellipse.type="convex",repel=TRUE,
             show.clust.cent = FALSE,ggtheme = theme_minimal(),main="agrupaciones")

head(flores)






#Base de datos cangrejos


head(cangrejos)

Arr_E=scale(cangrejos)

#calcular la matriz de disimilaridades
#usemos la distancia euclideana es el implicito
D=dist(Arr_E,method="euclidean")

#elaboracion del cluster jerarquico
C_arrestos=hclust(D,method="average")
#metodos:
#cercano - single
#lejano - complete
#media - average
#centroid - centroid

#metodo de aglomeracion 
#single: vecino mas proximo
#complete: vecino mas lejano
#average: media de  grupos
#centroid: centroide

#elaboracion del dendograma
fviz_dend(C_arrestos,cex=0.5,ylab="distancias",main="dendograma iris")

#n?mero de conglomerados a considerar
library(UniversalCVI)
I=CH.IDX(Arr_E,kmax=15,kmin=2,method="hclust_average")
plot(I$k,I$CH,pch=5,main="numero conglomerados",xlab="conglomerados",ylab="indice CH")
lines(I$k,I$CH, col = "black")


#cortar el dendograma en diferentes grupos
#cortar en 5 grupos
grupos=cutree(C_arrestos,k=4)
#numero de miembros en cada conglomerado
table(grupos)
#nombre de las ciudades en los grupos, ejemplo en el 1
rownames(cangrejos)[grupos==1]

#para hacer el grafico con los grupos
fviz_dend(C_arrestos,k=4,cex=0.5,k_colors= c("red","darkgreen","black","blue","magenta"),color_labels_by_k = TRUE,
          ylab="distancias",main="dendograma ciudades")

#se pueden utilizar las componentes principales para caracterizar los grupos

#obtencion de las componentes
CP_CC=PCA(cangrejos,scale.unit=TRUE,ncp=6,graph=FALSE)
fviz_screeplot(CP_CC,choise="eigenvalue",addlabels=TRUE,geom="line", ylim=c(0,80),
               xlab="dimensiones",ylab="porcentaje varianza",main="",ncp=6)
#interpretacion de los dos primeros ejes
fviz_pca_var(CP_CC,axes=c(1,2))

#representacion de los conglomerados ejes 1 y 2
fviz_cluster(list(data=cangrejos,cluster=grupos),palette=c("red","darkgreen","black","blue","magenta")
             ,ellipse.type="convex",repel=TRUE,
             show.clust.cent = FALSE,ggtheme = theme_minimal(),main="agrupaciones")

