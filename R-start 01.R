# Dimensions du jeu de données
dim(choco)

# Afficher nombre de colonnes
ncol(choco)

# Afficher nombre de lignes
nrow(choco)

# Afficher l'en-tête du jeu de données
head(choco)

# Afficher les 10 premieres lignes
head(choco, 10)

# Afficher les 6 dernières lignes
tail(choco,6)

# Afficher les stats univariées de toutes les col
summary(choco)

# Sélectionner une colonne par son nom
choco$Croquant

# Calculer la moyenne de Croquant
moyenneCroquant = mean(choco$Croquant)

# Calculer l'écart-type de Croquant
sd(choco$Croquant)

# Calculer les quantiles de Croquant
quantile(choco$Croquant)

# Sélectionner des sous-parties du data.frame avec 
# des indices

# Sélectionner la colonne 4
choco[,4]

# Sélectionne la ligne 5
choco[5,]

# Sélectionner la ligne 4, colonne 6
choco[4,6]

# Sélectionner les lignes 3 à 7, colonnes 2 à 4
choco[3:7 , 2:4]

# Sélectionner les lignes 3, 5, 8, colonnes 2 à 4
choco[c(3, 5, 8) , 2:4]

# Sélectionner colonnes 1 et 3 à 8, toutes les lignes
choco[ , c(1, 3:8) ]

# Filtrer des lignes d'un jeu de données
library(dplyr)
# Filtrer les lignes de choco correspondant
# à Croquant < 3 ; fonction filter()
filter(choco, Croquant<3)

# Filtrer les lignes de choco correspondant
# à Croquant < 3 ET (&) Glace inférieur ou égalà 3
JeuFiltre<-filter(choco, Croquant<3 & Glace <= 3)

# Décrire une variable catégorielle
TriAPlat<-table(choco$MarquePref)
TriAPlat
# Titre: "Diagramme en batons"
# Couleur: autre couleur que le gris
# Titre axe des Y: "Effectifs"
# Titre axe des X: "Marque"
# Ré-afficher le graphique avec les barres
# rangées par ordre décroissant d'effectifs
TriAPlat<-sort(TriAPlat, decreasing=TRUE)
barplot(TriAPlat, main="Diagramme en batons",
        col="blue", ylab="Effectifs",
        xlab="Marques")

# Décrire une variable quantitative
mean(choco$Croquant) # moyenne
sd(choco$Croquant) # écart type
quantile(choco$Croquant) # Quantiles
# Calculer les déciles de Croquant
quantile(choco$Croquant, probs=0:10*0.1)
quantile(choco$Croquant, probs=seq(0, 1, 0.1))

# Graphiques univariés
par(mfrow=c(1,2))
# Dessiner un boxplot de Croquant
# Changer la couleur
boxplot(choco$Croquant, col="blue")
# Dessiner un histogramme de Croquant
hist(choco$Croquant, main="Histogramme",
     col="grey")


# Etude du lien entre 2 variables quanti
matcor<-cor(choco[, c(2, 6:10)])
library(corrplot)
corrplot(matcor)

# Dessiner un nuage de points entre Intensite (Y)
# Et Age (X).
# Changer le titre.
# Changer le figuré du point (argument pch)
plot(choco$Age, choco$Intensite, pch=16)

# Lien entre une quanti et une quali
# Calculer la moyenne d'intensité par marque préférée
# tapply(var. à découper, 
#        var. de découpage, 
#         fonction à appliquer sur chaque morceau)
tapply(choco$Intensite, choco$MarquePref, mean)
# Faire la même chose avec Croquant
MoyenneSucreSale<-tapply(choco$Croquant, choco$SucreSale, mean)
# Afficher le résultat sous forme de barplot
barplot(MoyenneSucreSale)
# boxplots multiples
boxplot(choco$Intensite~choco$MarquePref)

# Lien entre 2 variables qualitatives
# notamment entre SucreSale et MarquePref
# table(variable 1, variable 2)
TriCroise<-table(choco$MarquePref, choco$SucreSale)
prop.table(TriCroise,1)
prop.table(TriCroise,2)


# Binariser (0 - 1) la colonne Note.nouveau.produit
choco$PrefBin<-ifelse(choco$Note.nouveau.produit<6, 0, 1)

boxplot(choco$Glace~choco$PrefBin)


