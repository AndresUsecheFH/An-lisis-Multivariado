#construccion de las componentes
#problema examenes

#paquete FactoMineR
library(FactoMineR)

#obtención de las CP y las deposito
#en un objeto

CP_Ex=PCA(Ex,scale.unit=TRUE,ncp=5,graph=FALSE)

#cuando se estandarizan las variables scale.unit=TRUE
#ncp: número de componentes a analizar 

#para analizar se utiliza el
#paquete factoextra
library(factoextra)

#Las variables
#la funcion get_pca_var brinda varias info

var_Ex=get_pca_var(CP_Ex)

#se depositan en este objeto, sus diferentes componentes
#contienen diversos criterios
#caso donde se obtienen las  coordenadas
#es decir, los vectores propios que constituyen
#los coeficientes de las CP

#visualizacion de las coordenadas
var_Ex$coord

#valores propios
vp=get_eigenvalue(CP_Ex)
vp

#diagrama de pendiente

fviz_screeplot(CP_Ex,choise="eigenvalue",addlabels=TRUE,geom="line", ylim=c(0,80),
               xlab="dimensiones",ylab="porcentaje varianza",main="",ncp=5)

#ncp: numero de componentes

#VARIABLES
#Representación de las variables

#diagrama circulo de correlacion
#se retoman los objetos que se encuentran en 
#var_Ex=get_pca_var(CP_Ex)
#las correlaciones se encuentran en:
var_Ex$cor

#el gráfico
fviz_pca_var(CP_Ex,axes=c(1,2),col.var="black",col.circle="blue")

#cuadrado de los cosenos
#se retoman los objetos que se encuentran en 
#var_Ex=get_pca_var(CP_Ex)
#los cuadrados de los cosenos se encuentran en:

round(var_Ex$cos2,3)

#contribucion de cada variable a los ejes principales
#se retoman los objetos que se encuentran en 
#var_Ex=get_pca_var(CP_Ex)

round(var_Ex$contrib,1)

#INDIVIDUOS
#Para representar los individuos

#se utiliza el comando get_pca_ind
#recuerde que para las variables se usaba get_pca_var

#como en el caso de las variables
#este comando tiene diversas informaciones

#sobre los individuos
ind=get_pca_ind(CP_Ex)


#las coordenadas se encuentran en:
ind$coord

#para graficar los individuos
fviz_pca_ind(CP_Ex,axes=c(1,2))

#tambien ind=get_pca_ind(CP_Ex)
#contiene informacion sobre la contribucion
#de cada individuo
ind$contrib

#contiene información del angulo (coseno cuadrado)
ind$cos2