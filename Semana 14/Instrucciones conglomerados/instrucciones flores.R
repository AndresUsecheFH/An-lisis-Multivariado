#paquetes
library(FactoMineR)
library(factoextra)

#Preparacion de los datos estandarizacion
flores_E=scale(flores)

#calcular la matriz de disimilaridades
#usemos la distancia euclideana es el implicito
D=dist(flores_E,method="euclidean")

#elaboracion del cluster jerarquico
C_flores=hclust(D,method="complete")

#elaboracion del dendograma
fviz_dend(C_flores,cex=0.5,ylab="distancias",main="dendograma iris")

#cortar el dendograma en diferentes grupos
#cortar en 5 grupos
grupos=cutree(C_flores,k=3)
#numero de miembros en cada conglomerado
table(grupos)

#para hacer el grafico con los grupos
fviz_dend(C_flores,k=3,cex=0.5,k_colors= c("red","darkgreen","black"),
          color_labels_by_k = TRUE,
          ylab="distancias",main="dendograma flores")

#obtencion de las componentes
CP_flores=PCA(flores,scale.unit=TRUE,ncp=4,graph=FALSE)
fviz_screeplot(CP_flores,choise="eigenvalue",addlabels=TRUE,geom="line", ylim=c(0,80),
               xlab="dimensiones",ylab="porcentaje varianza",main="",ncp=4)
#interpretacion de los dos primeros ejes
fviz_pca_var(CP_flores,axes=c(1,2))

#representacion de los conglomerados ejes 1 y 2
fviz_cluster(list(data=flores,cluster=grupos),palette=c("red","darkgreen","black","blue","magenta")
             ,ellipse.type="convex",repel=TRUE,
             show.clust.cent = FALSE,ggtheme = theme_minimal(),main="agrupaciones")