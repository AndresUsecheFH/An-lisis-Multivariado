#Analisis del problema 2 poblaciones hornos

#prueba de Box
library(mvhtests)
x=as.matrix(hornos2[ ,2:3])
ina=hornos2[ ,1]
Mtest.cov(x,ina)

#Prueba Hotelling dos poblaciones
library(MVTests)
TwoSamplesHT2(hornos2[,2:3],hornos2$grupo )

#Evaluacvion de la pertenencia a la Region de confianza
#para saber cuantos datos hay
attach(hornos2)
table(grupo)

#percentil
n1=42
n2=40
p=2
alfa=0.01
c=((n1+n2-2)*p*qf(1-alfa,p,n1+n2-p-1))/(n1+n2-p-1)

#calculo del valor del estadistico


#matriz varianzas y cov combinada
S1=cov(hornos2[hornos2$grupo==1,2:3])
S2=cov(hornos2[hornos2$grupo==2,2:3])
Scomb=((n1-1)*S1+(n2-1)*S2 )/(n1+n2-2)

St=((1/n1)+(1/n2))*Scomb

#Medias del vector (cerrada,abierta) en cada grupo
Med1=colMeans(hornos2[hornos2$grupo==1,2:3])
Med2=colMeans(hornos2[hornos2$grupo==2,2:3])

#diferencia de las medias para pertenencia
#a la region
dif=c(0.5,0.4)

#valor del estadístico
t(Med1-Med2-dif)%*%solve(St)%*%(Med1-Med2-dif)

#intervalos de confianza simultáneos
#usando lo ya calculado

#Para cerrada
#hago dif para mas comodidad
dif=Med1-Med2
#para cerrada
#LI
dif[1]-sqrt(c*St[1,1])
#LS
dif[1]+sqrt(c*St[1,1])

#para abierta
#LI
dif[2]-sqrt(c*St[2,2])
#LS
dif[2]+sqrt(c*St[2,2])

#visualización. Perfiles
#medias por grupo
M=rbind(Med1,Med2)
row.names(M)=c("grupo 1","grupo 2")
#el grafico
library(profileR)

profileplot(M,person.id=rownames(M),standardize=FALSE)

#caso varianzas diferentes
#prueba de Box
library(mvhtests)
x=as.matrix(hornos3[ ,2:3])
ina=hornos3[ ,1]
Mtest.cov(x,ina)

#prueba medias (2) matrices var
#y cov diferentes (James)
library(mvhtests)

#los datos deben estar en dos matrices diferentes

M1=hornos3[hornos3$grupo==1,]
M2=hornos3[hornos3$grupo==2,]
#elimino variable grupo
M1=M1[,-1]
M2=M2[,-1]
#los convierto en matrices
M1=as.matrix(M1)
M2=as.matrix(M2)
#la prueba de james
james(M1,M2,R=1)
#R=1 para que no haga bootstrap, se usa el F clasico
