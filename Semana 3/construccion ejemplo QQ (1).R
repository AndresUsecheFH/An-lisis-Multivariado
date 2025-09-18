#criterio construccion QQ
# datos de la distribución
x=round(rnorm(5,1,0.5),1)

x_ord=sort(x)
i=1:5
orden=(i-0.5)/5
q=round(qnorm(orden,1,0.5),1)

cbind(x,x_ord,orden,q)

plot(q,x_ord,pch=16,cex=2,cex.lab=1.5,cex.axis=1.5)
r=lm(x_ord~q)
abline(r,lwd=3,col="red")

cor.test(q,x_ord,conf.level=0.99)
