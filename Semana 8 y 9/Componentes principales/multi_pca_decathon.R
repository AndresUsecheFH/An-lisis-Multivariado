attach (DC)
library(FactoMineR)
CP_Ex=PCA(DC,scale.unit=TRUE, ncp=10)#puede agregar graph=FALSE
library(factoextra)
Var_Ex=get_pca_var(CP_Ex)
#valores propios
Var_Ex$coord
#vectores propios 
vp=get_eigenvalue(CP_Ex)
vp #aca revisar solo las que sean 1 el eigenvalue
#o mas del 10%individualmente en var%

round(Var_Ex$cor[,1:3],2)
cbind(round(Var_Ex$cos2))
round(Var_Ex$contrib[,1:3],2)

#diagrama de pendiente
fviz_screeplot(CP_Ex,choise="eigenvalue", geom="line",yline=c(0,100), xlab="dimensiones", ylab="porcentaje varianza", main="", ncp=10)
#circulo correlaciones
fviz_pca_var(CP_Ex,axes=c(1,2), col.var="black", col.circle="blue")

fviz_pca_var(CP_Ex,axes=c(2,3), col.var="black", col.circle="blue")


ind = get_pca_ind(CP_Ex)
ind$coord[,1:3]
ind$contrib[,1:3]

fviz_pca_ind(CP_Ex, axes = c(2,3))
