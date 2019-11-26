# Dimensions du jeu de données
dim(choco)
ncol(choco)
nrow(choco)
head(choco, 10)
tail(choco)
summary(choco)
choco$Croquant
moyenne = mean(choco$Croquant)
ecarttype <- sd(choco$Croquant)
quantile(choco$Croquant)
choco[,2:3]
choco[c(3,4:8),4:6]
triaplat <- table(choco$MarquePref)
max(choco$Intensite)
barplot(sort(triaplat, decreasing=TRUE), col=hcl.colors(dim(triaplat), alpha=0.6), xlab='Marque', ylab='Effectifs', main='Diagramme en bâtons')
# seq(0,1000000000,1)
# a <- 0:1000000000
boxplot(choco$Croquant, col='lightgreen', border='blue', main='boxplot')
par(mfrow=c(1,2))
hist(choco$Croquant, col='lightblue', main='histogramme')

library(dplyr)
filter(choco, Croquant < 5 & Note.nouveau.produit <=5)
cor(choco$Age, choco$Intensite)
dotchart(choco$Age, choco$Intensite, pch='+', main='Dot chart')
matcor=cor(choco[, c(1, 5:9)])
matcorr

library(corrplot)
corrplot(matcor)
barplot(tapply(choco$Intensite, choco$MarquePref, mean))
barplot(prop.table(table(choco$MarquePref, choco$SucreSale), 2), legend=T, col=hcl.colors(3, alpha=0.6), args.legend=list(x="top", inset=-0.2, horiz=T))

library(help="stats")
screeplot(choco)

library(summarytools)
view(dfSummary(choco))
pairs(choco[, c(1, 5:9)], col=hcl.colors(3)[choco$MarquePref], pch=19, lower.panel=NULL)

library(PerformanceAnalytics)
chart.Correlation(choco[, c(1, 5:9)], histogram=T, pch=19)

library(ggplot2)
qplot(choco$Intensite, Note.nouveau.produit, data=choco, color=MarquePref, geom=c('point','smooth'))

