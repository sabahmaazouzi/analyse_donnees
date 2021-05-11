library(xlsx)
data <- read.xlsx("donnees.xlsx", 1)
str(data)
Don<-data[,1:11]
library(FactoMineR)

#Application de l'ACP
res<-PCA(Don,ncp=5,scale.unit=TRUE,axes=c(1,2),graph=T)
attributes(res)


#Test de sph?ricit? de Bartlett
bartlett.test(Don)

#Calcul de l'indice KMO et des MSAi
library(psych)
KMO(cor(Don))


#Calcul des valeurs propres de la matrice de corr?lation
res$eig

#calcul des corr?lations entre variables
cor(Don)

#Graphique des valeurs propres
plot(1:11,res$eig[,1],type="b",ylab="Valeurs propres",xlab="Composantes",main="graphique des valeurs propres")
sum(res$eig[,1])

#Nuage des individus
#Calcul de : coord, cor, cos2, contrib
res$var$cos2

#Cumul des cos2
print(t(apply(res$var$cos2,1,cumsum)),digit=2)

#Contribution des variables au sous espace
cont<-res$var$contrib

write.table(cont[,1:5],"var.csv",sep=";",col.names=TRUE,dec=',', row.names=TRUE)
Don<-read.csv2("var.csv",row.names=1)
cont<-HCPC(Don)
cont$desc.var
cont$data.clust



#Nuage des individus
#Calcul de : coord, cos2, contrib
res$ind$cos2

#Cumul des cos2
cum<-print(t(apply(res$ind$cos2,1,cumsum)),digit=2)



#Contribution des variables au sous espace
cont<-res$ind$contrib

write.table(cont[,1:5],"ind.csv",sep=";",col.names=TRUE,dec=',', row.names=TRUE)
Don1<-read.csv2("ind.csv",row.names=1)
cont<-HCPC(Don1)
cont$desc.var
cont$data.clust

#nuage des individus
library(factoextra)
library(ggplot2)
fviz_pca_ind(res, col.var = "black")

