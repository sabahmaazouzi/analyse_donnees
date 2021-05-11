
Ventes <- read.csv2("donnees.csv")

library(FactoMineR)

clusters <- kmeans(Ventes[,1:2], 3)
Ventes$Borough <- as.factor(clusters$cluster)
write.table(Ventes,"aida.csv",sep=";",col.names=FALSE,append=TRUE,dec=',', row.names=FALSE)

clusters <- kmeans(Ventes[,2:3], 3)
inertie <- clusters$betweenss/clusters$totss
inertie
Ventes$col2 <- as.factor(clusters$cluster)
write.table(Ventes,"aida.csv",sep=";",col.names=FALSE,dec=',', row.names=FALSE)

clusters <- kmeans(Ventes[,3:4], 3)
inertie <- clusters$betweenss/clusters$totss
inertie
Ventes$col3 <- as.factor(clusters$cluster)
write.table(Ventes,"aida.csv",sep=";",col.names=FALSE,dec=',', row.names=FALSE)

clusters <- kmeans(Ventes[,4:5], 3)
inertie <- clusters$betweenss/clusters$totss
inertie
Ventes$col4 <- as.factor(clusters$cluster)
write.table(Ventes,"aida.csv",sep=";",col.names=FALSE,dec=',', row.names=FALSE)

clusters <- kmeans(Ventes[,5:6], 3)
inertie <- clusters$betweenss/clusters$totss
inertie
Ventes$col5 <- as.factor(clusters$cluster)
write.table(Ventes,"aida.csv",sep=";",col.names=FALSE,dec=',', row.names=FALSE)

clusters <- kmeans(Ventes[,6:7], 3)
inertie <- clusters$betweenss/clusters$totss
inertie
Ventes$col6 <- as.factor(clusters$cluster)
write.table(Ventes,"aida.csv",sep=";",col.names=FALSE,dec=',', row.names=FALSE)

clusters <- kmeans(Ventes[,7:8], 3)
inertie <- clusters$betweenss/clusters$totss
inertie
Ventes$col7 <- as.factor(clusters$cluster)
write.table(Ventes,"aida.csv",sep=";",col.names=FALSE,dec=',', row.names=FALSE)

clusters <- kmeans(Ventes[,8:9], 3)
inertie <- clusters$betweenss/clusters$totss
inertie
Ventes$col8 <- as.factor(clusters$cluster)
write.table(Ventes,"aida.csv",sep=";",col.names=FALSE,dec=',', row.names=FALSE)

clusters <- kmeans(Ventes[,9:10], 3)
inertie <- clusters$betweenss/clusters$totss
inertie
Ventes$col9 <- as.factor(clusters$cluster)
write.table(Ventes,"aida.csv",sep=";",col.names=FALSE,dec=',', row.names=FALSE)

clusters <- kmeans(Ventes[,10:11], 3)
inertie <- clusters$betweenss/clusters$totss
inertie
Ventes$col10 <- as.factor(clusters$cluster)
write.table(Ventes,"aida.csv",sep=";",col.names=FALSE,dec=',', row.names=FALSE)

clusters <- kmeans(Ventes[,11:12], 3)
inertie <- clusters$betweenss/clusters$totss
inertie
Ventes$col11 <- as.factor(clusters$cluster)
write.table(Ventes,"aida.csv",sep=";",col.names=FALSE,dec=',', row.names=FALSE)




vv<- read.csv2("qualitatif.csv")
new_data <- as.data.frame(vv)
library(FactoMineR)

col_names <- names(new_data)
new_data[,col_names] <- lapply(new_data[,col_names] , factor)

#Construction du tableau disjonctif complet
K <- tab.disjonctif(new_data) 
K
apply(K,2,sum)
#Existe t-il des modalit?s de faibles fr?quences ?
propmod=apply(K,2,sum)/(nrow(K))
propmod
K<- K[,-19]
K<- K[,-11] 
K<- K[,-12] 
K<- K[,-14] 
 
#relecure apres la suppression des valeur de faible frequence 

vv<- read.csv2("/Users/macbookpro/Desktop/qualitatif2.csv")
res<-MCA(vv,ncp=8)




#Calcul des valeurs propres
res$eig
plot(res$eig[,1],type="b",main="Valeurs propres ")
12*var(res$eig[4:16,1])/(15*var(res$eig[,1]))

#cos2 des modalit?s
res$var$cos2
print(t(apply(res$var$cos2,1,cumsum)),digit=2)
#toutes les modalit?s sont bien projet?es dans le sous espace
#contributions modalit?s
contrib <- res$var$contrib
contrib
write.table(contrib[,1:8],"mod.csv",sep=";",col.names=TRUE,dec=',', row.names=TRUE)
Don1<-read.csv2("mod.csv",row.names=1)
cont<-HCPC(Don1)
cont$desc.var
cont$data.clust

#cos2 des individus
res$ind$cos2

print(t(apply(res$ind$cos2,1,cumsum)),digit=2)
#contributions des Individus
contrib <- res$ind$contrib
contrib
write.table(contrib[,1:5],"Ind.csv",sep=";",col.names=TRUE,dec=',', row.names=TRUE)
Don2<-read.csv2("Ind.csv",row.names=1)
cont<-HCPC(Don2)
cont$desc.var
cont$data.clust

#Nuage des variables
#Calcul des coefficients de corr?lation des variables avec les projections sur les axes
res$var$eta2
#graphique des coefficients de corr?lation des variables avec les facteurs du 1er plan factoriel
plot(res,choix="var") 
