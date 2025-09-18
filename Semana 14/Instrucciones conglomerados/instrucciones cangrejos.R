#paquetes
library(FactoMineR)
library(factoextra)

#Preparacion de los datos estandarizacion
cangrejos_E=scale(cangrejos)

#calcular la matriz de disimilaridades
#usemos la distancia euclideana es el implicito
D=dist(cangrejos_E,method="euclidean")

#elaboracion del cluster jerarquico
C_cangrejos=hclust(D,method="complete")

#elaboracion del dendograma
fviz_dend(C_cangrejos,cex=0.5,ylab="distancias",main="dendograma cangrejos")

#numero de conglomerados a considerar (indice CH)
library(UniversalCVI)
I=CH.IDX(cangrejos_E,kmax=5,kmin=2,method="hclust_average")
plot(I$k,I$CH,pch=5,main="numero conglomerados",xlab="conglomerados"
     ,ylab="indice CH")
lines(I$k,I$CH, col = "black")

#cortar el dendograma en diferentes grupos
#cortar en 2 grupos
grupos=cutree(C_cangrejos,k=4)
#numero de miembros en cada conglomerado
table(grupos)

#para hacer el grafico con los grupos
fviz_dend(C_cangrejos,k=4,cex=0.5,k_colors= c("red","darkblue","green","black"),
          color_labels_by_k = TRUE,
          ylab="distancias",main="dendograma cangrejos")

#obtencion de las componentes
CP_cangrejos=PCA(cangrejos,scale.unit=TRUE,ncp=5,graph=FALSE)
fviz_screeplot(CP_cangrejos,choise="eigenvalue",addlabels=TRUE,geom="line", ylim=c(0,100),
               xlab="dimensiones",ylab="porcentaje varianza",main="",ncp=5)
#interpretacion de los dos primeros ejes
fviz_pca_var(CP_cangrejos,axes=c(1,2))

#representacion de los conglomerados ejes 1 y 2
fviz_cluster(list(data=cangrejos,cluster=grupos),palette=c("red","darkgreen","black","blue","magenta")
             ,ellipse.type="convex",repel=TRUE,
             show.clust.cent = FALSE,ggtheme = theme_minimal(),main="agrupaciones")