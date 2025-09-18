#perfiles con grafico  de lineas (poligonal)

#M matriz con los datos cerveza
#digitar los datos.
M=matrix(nr=2 , nc= 3 )
data.entry(M)

#nombres
rownames(M)=c("hombres","mujeres")
colnames(M)=c("ligera","clara","oscura")

#calculo proporciones por columna (valores por sexo)
T1=round(prop.table(M,2),3)

#al escribir prop.table(M,2) indico que las
#proporciones son por columna
#por fila seria prop.table(M,1)
#datos a representar
T1[1,]
T1[2,]

#para los perfiles con líneas:
#cargar paquete profileR y activarlo:
library(profileR)

#el comando con matrices
T1=as.matrix(T1)

# el gráfico. 
# Atencion: No se estandariza porque los datos ya lo están, 
# si no hay habría que hacerlo
profileplot(T1,person.id=rownames(T1),standardize=FALSE)

