library(psych)
#pregunta a
#Prueba de Bartlett
cortest.bartlett(textura)

#criterio KMO
KMO(textura)

#pregunta b
text_cor=cor(textura)

#valores propios
vp_text=eigen(text_cor)

#varianza total explicada por cada factor 
round(vp_text$values,1)

#(proporcion)
round(vp_text$values/sum(vp_text$values),3)

# varianza explicada (prop) acumulada
round(cumsum(vp_text$values/5),3)

#grafico de pendiente
plot(vp_text$values,type="b",ylab="valores propios",xlab="factor",
     cex.axis=1.5,cex.lab=1.5,cex=1.5,pch=16 )

fa_text = fa(r=text_cor, 
                nfactors = 2, 
                fm="pa", 
                max.iter=100, 
                rotate="varimax") 
print(fa_text)

#grafico de factores y cargas
fa.diagram(fa_text)