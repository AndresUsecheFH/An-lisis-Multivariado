#grafico por grupos

#grafico coloreado por grupos
fviz_pca_ind(alim_PCA,geom.ind="point", col.ind=alim$Comunist,palette=c("lightslateblue","indianred1"),
             legend.title="comunist",addEllipses=TRUE,axes=c(1,2))
