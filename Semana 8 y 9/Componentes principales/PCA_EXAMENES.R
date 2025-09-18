Ex
#PASO 1: Obterner las CP 
#Las variables deben estar estandarizadas


library(FactoMineR)

CP_Ex=PCA(Ex,scale.unit=TRUE,ncp=5,graph = FALSE)
#Paso 2: Para analizar usar el paquete factoextra
library(factoextra)

#Obtencion de las componentes principales(dimensiones,variables sinteticas)

var_Ex=get_pca_var(CP_Ex)

#Las coordenadas son los vectores propios que constituyen los coeficientes de las CP
var_Ex$coord

#Valores propios
vp=get_eigenvalue(CP_Ex)
vp



#Diagrama de pendiente(SCREE)

fviz_screeplot(CP_Ex,choise="eigenvalue",geom="line", ylim=c(0,100),
               xlab="dimensiones",ylab="porcentaje varianza",main="",npc=5)



#Para representar las varibales y ver coomo influyen (se relacionan) sobres los
#ejes principlaes se utilizan las correlaciones

var_Ex$cor
#componente 1 Y1 = 0.89*mat+0.72*fis+0.61*esp+0.59*hist -0.91*ef
#componente 2 Y2 = -0.34*mat-0.64*fis+0.71*esp+0.74*hist -0.11*ef

#El grafico
fviz_pca_var(CP_Ex,axes=c(1,2),col.var="black",col.circle = "blue")


#Cudrado de los cosenos
round(var_Ex$cos2,3)

100*0.802/(sum(var_Ex$cos2[,1]))

round(var_Ex$contrib,1)




#Representar a los individuos

ind=get_pca_ind(CP_Ex)

ind$coord

fviz_pca_ind(CP_Ex,axes=c(1,2))


#Coordenada de Lucia en el eje 1: 14.4
#Y1 = 0.89*mat+0.72*fis+0.61*esp+0.59*hist -0.91*ef
#Coordenada de Lucia en el eje 2: 7.36
#Y2 = -0.34*mat-0.64*fis+0.71*esp+0.74*hist -0.11*ef

ind$cos2

ind$contrib


#Aplicación:Ana y José se asemejan y se caracterizan por alto
#desempeño en mat y física,pero bajo en EF.El eje 1 es el más
#importante para caracterizarlos, porque