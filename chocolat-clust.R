library(summarytools)
library(corrplot)
library(PerformanceAnalytics)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(dplyr)
library(lattice)
library(cluster)
library(visreg)
library(coefplot)
library(car)

# importation du fichier
data = read.csv('CRM - DataChocolat.csv', row.names=1)

summary(data)
Age_scaled = (data$Age - 17)/(50-17)*10
data_scaled = data.frame(select(data, -Age), Age_scaled)
summary(data)

res.kmeans = kmeans(data[,c(1, 5:9)], centers=3, iter.max=10)

res.acp = PCA(data[,c(1,5:9)])
plot(res.acp, col.ind=hcl.colors(levels(factor(res.kmeans$cluster))[factor(res.kmeans$cluster)]))
fviz_pca_biplot(res.acp, repel = TRUE, col.ind=factor(res.kmeans$cluster))

groupes = factor(res.kmeans$cluster)
parallelplot(~data[,c(1, 5:9)] | groupes, data)

cah.res = agnes(data[,c(1, 5:9)], method='ward')
plot(cah.res, which.plot=2)
rect.hclust(cah.res, k=2, border=1:2)
cutree(cah.res, k=2)


reg.croq <- lm(AfterEight~Croquant, data=data_scaled)
summary(reg.croq)
visreg(reg.croq)
reg.int <- lm(AfterEight~Intensite, data=data_scaled)
summary(reg.int)
reg.glac <- lm(AfterEight~Glace, data=data_scaled)
summary(reg.glac)
reg.fourr <- lm(AfterEight~Fourrage, data=data_scaled)
summary(reg.fourr)
reg.age <- lm(AfterEight~Age_scaled, data=data_scaled)
summary(reg.age)

reg.after <- lm(AfterEight~Croquant+Intensite+Glace+Fourrage+Age_scaled, data=data_scaled)
summary(reg.after)

par(mfrow=c(2,3))
visreg(reg.after)

reg.simple_lindt <- lm(Lindt~Croquant+Intensite+Glace+Fourrage+Age_scaled, data=data_scaled)
summary(reg.simple_lindt)

par(mfrow=c(2,3))
visreg(reg.simple_lindt)

par(mfrow=c(1,1))
coefplot(reg.after)
coefplot(reg.simple_lindt)
vif(reg.simple_lindt)
vif(reg.after)


reg.after_optimale <- step(reg.after, direction="backward")
reg.lindt_optimale <- step(reg.simple_lindt, direction="backward")
par(mfrow=c(2,2))
visreg(reg.lindt_optimale)
visreg(reg.after_optimale)

summary(reg.after)
summary(reg.after_optimale)
