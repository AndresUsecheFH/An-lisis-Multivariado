#########     PARCIAL MULTIVARIADO  #######


#PREGUNTA 1
#a.)


#paquete MVN

library(MVN)
#Prueba de Royston

head(res)

mvn(res,mvnTest="royston",alpha=0.01)


#b)

install.packages("MVTests")  # Instala el paquete
library(MVTests)  # Carga el paquete

OneSampleHT2(res[,1:4],c(72,82,75,80),alpha=0.01)


#C,)


#Intervalos de confianza simultaneos

#se calculan los estadisticos

#vector de medias muestrales
med=colMeans(res[ , 1:4])

#matriz de varianzas y covarianzas
S=cov(res[ , 1:4])
str(res)
#el percentil
#datos para el calculo
n=130
p=4
alfa=0.01
p_alfa=((n-1)*p*qf(1-alfa,p,n-p))/(n*(n-p))

#limites de confianza para la primera variable F
#limite superior
med[1]+sqrt(p_alfa*S[1,1])
#limite inferior
med[1]-sqrt(p_alfa*S[1,1])

med
p_alfa
S




#PREGUNTA 2

head(esc)

#b.)

#prueba de Box
library(mvhtests)
x=as.matrix(esc[ ,2:5])
ina=esc[ ,1]
Mtest.cov(x,ina, a=0.01)

  #Prueba Hotelling dos poblaciones
library(MVTests)
TwoSamplesHT2(esc[,2:5],esc$grupo, alpha=0.01 )




#c


#para saber cuantos datos hay
attach(esc)
table(grupo)
str(esc)
#percentil
n1=80
n2=90
p=2
alfa=0.01
c=((n1+n2-2)*p*qf(1-alfa,p,n1+n2-p-1))/(n1+n2-p-1)
c
S1=cov(esc[esc$grupo==1,2:5])
S2=cov(esc[esc$grupo==2,2:5])
Scomb=((n1-1)*S1+(n2-1)*S2 )/(n1+n2-2)

St=((1/n1)+(1/n2))*Scomb
St

#Medias del vector (Comunista, No comunista) en cada grupo
Med1=colMeans(esc[esc$grupo==1,2:5])
Med2=colMeans(esc[esc$grupo==2,2:5])
Med1
Med2



#hago dif para mas comodidad
dif=Med1-Med2
dif
#para cerrada
#LI
dif[2]-sqrt(c*St[2,2])
#LS
dif[2]+sqrt(c*St[2,2])

#para abierta
#LI
dif[2]-sqrt(c*St[2,2])
#LS
dif[2]+sqrt(c*St[2,2])

