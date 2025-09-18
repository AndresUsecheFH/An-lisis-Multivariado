M=matrix(nr=2 , nc= 2 )
data.entry(M)

rownames(M)=c("grupo 1","grupo 2")
colnames(M)=c("Parcial 1","Parcial 2")

#el grafico
library(profileR)

profileplot(M,person.id=rownames(M),standardize=FALSE)
