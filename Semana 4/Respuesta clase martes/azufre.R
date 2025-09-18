# paso 1 digitar los datos
# M matriz con los datos azufre

M=matrix(nr=3 , nc= 3 )
data.entry(M)

#nombres
rownames(M)=c("SO2 alto","SO2 normal","SO2 bajo")
colnames(M)=c("cloro_p alto","cloro_p normal","cloro_p bajo")

#proporciones por columna
T1=round(prop.table(M,2),3)

# el gráfico. 

#el comando con matrices
T1=as.matrix(T1)

# el gráfico. 
# Atencion: No se estandariza porque los datos ya lo están, 
# si no hay habría que hacerlo
profileplot(T1,person.id=rownames(T1),standardize=FALSE)

