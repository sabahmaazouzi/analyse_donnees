library("readxl")
data<-read.csv2("donnees.csv")
print(data[1:9,c("po","maxmin","minmin","pf","volume","capital","negotiation","me",	"mb",	"diff")])

col.actifs <- data[,1:10]
#centrage et réduction 
col.actifs_cr<-scale(col.actifs,center=T,scale=T)
#kmeans
groupes.kmeans <- kmeans(col.actifs_cr,centers=8,nstart=5)

print(groupes.kmeans)

groupes.kmeans$betweenss


#l'inertie explicative
inertie.expl <- rep(0,times=10)
for (k in 2:10){
  clus <- kmeans(col.actifs_cr,centers=k,nstart=5)
  inertie.expl[k] <- clus$betweenss/clus$totss
}

plot(1:10,inertie.expl,type="b",xlab="Nb. de groupes",ylab="% inertie expliqu?e")

#CAH
library(FactoMineR)
#Les donn?es ont ?t? pr?alablement centr?es et r?duites sur Excel
autos_cr<-read_xlsx("data_centree_reduite.xlsx",1)

print(autos_cr)
autos_cr.actifs<-autos_cr[,1:6]

Res<-HCPC(autos_cr.actifs,nb.clust=-1)

cor(autos_cr.actifs)
Res$data.clust
Res$desc.var
Res$desc.ind
Res$desc.var$test.chi2
Res$desc.var$category
##Calcul du taux d'inertie
I<-Res$call
I
#####gain d'inerite
Res$call$'t'$inert.gain

