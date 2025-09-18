#evaluacion de la pertenencia a la region de confianza

#calculo del percentil
#caracteristicas de la base de datos

str(hornos)

#Valores
n= 42
p=2
alfa=0.01

#percentil
((n-1)*p*qf(1-alfa,p,n-p))/(n-p)

#valor observado del estadístico

#vector de medias muestrales
med=colMeans(hornos[ , -1])

#matriz de varianzas y covarianzas
S=cov(hornos[ , -1])

#valor donde se evalúa:
mu=c(0.56,0.58)

#valor del estadístico en mu
u=med-mu
n*t(u)%*%solve(S)%*%(u)

#Intervalos de confianza simultaneos

#se calculan los estadisticos

#vector de medias muestrales
med=colMeans(hornos[ , -1])

#matriz de varianzas y covarianzas
S=cov(hornos[ , -1])

#el percentil
#datos para el calculo
n=42
p=2
alfa=0.01
p_alfa=((n-1)*p*qf(1-alfa,p,n-p))/(n*(n-p))
  
#limites de confianza para la primera variable (X1, cerrada)
#limite superior
med[1]+sqrt(p_alfa*S[1,1])
#limite inferior
med[1]-sqrt(p_alfa*S[1,1])

#limites de confianza para la segunda variable (X2, abierta)
#limite superior
med[2]+sqrt(p_alfa*S[2,2])
#limite inferior
med[2]-sqrt(p_alfa*S[2,2])

