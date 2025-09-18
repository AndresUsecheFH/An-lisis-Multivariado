#correlacion de Pearson
#paquete corrplot
#para hacer la prueba de hipótesis e IC
#datos en data frame o matriz de datos

cor(compu)
cor.mtest(compu,conf.level=0.99)
#salidas
#T$p: valores p
#T$lowCI: limite inferior IC
#T$uppCI: limite superior IC

#para las correlaciones parciales
#paquete ppcor
pcor(compu)
