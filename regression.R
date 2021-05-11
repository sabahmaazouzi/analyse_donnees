library(xlsx)
res <- read.xlsx("donnees.xlsx", 1)

#modele
modele <- lm(cible~ po+maxmin+minmin+negociation+pf+volume+capital+me+mb+diff,data=res)
modele$coefficients

#fisher p-value
summary(modele)

#step
r<-step(modele)

#AIC 

AIC(r)
summary(r)


#homosce..
library("lmtest")
bptest(modele)
#nuage
pairs(res)
#normalite
e <- residuals(modele)
e <- modele$residuals 
qqnorm(e,datax=TRUE,ylab="Quantiles observés",xlab="Quantiles théoriques")
residus<-residuals(modele)
#shapiro
shapiro.test(residus)

library("lmtest")
library(MASS)
r <- step(modele,direction="forward")
bptest(r)
AIC(r)
