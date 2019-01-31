################################################################################
# R-simulation 
# Thomas DENECKER  Gaëlle Lelandais
# DU omics - session 2 - 2019
#
################################################################################

################################################################################
# Installation de packages
################################################################################

install.packages("tidyverse")
install.packages("dplyr")
install.packages("gridExtra")
install.packages("cowplot")

################################################################################
# Library
################################################################################

library("tidyverse")
library("dplyr")
library("gridExtra")
library("cowplot")

################################################################################
# Fonctions 
################################################################################

source("fonctions.R", encoding="utf-8")

################################################################################
# Main 
################################################################################

#===============================================================================
# Lecture des données
#===============================================================================

pop0 = runif(n = 10000,min = 0,max = 300)
# saveRDS(pop0, file = "pop0.rds")
# pop0 = readRDS(file = "pop0.rds")

popA = rnorm(n = 10000,mean = 150, sd = 20)
popB = rnorm(n = 10000,mean = 180, sd = 20)
popC = rnorm(n = 10000,mean = 160, sd = 20)

#===============================================================================
# Population 0
#===============================================================================

#-------------------------------------------------------------------------------
# Echantillonage
#-------------------------------------------------------------------------------

ech0 = echantillonage(population = pop0, taille = 100)

#-------------------------------------------------------------------------------
# Echantillonage multiple
#-------------------------------------------------------------------------------

# Sans valeur de référence
vecteurMoyenne = echantillonageMultiple(population = pop0, taille = 100, 
                                        nbEchantillon = 10)

# Avec une valeur de référence
vecteurMoyenne = echantillonageMultiple(population = pop0, taille = 100, 
                                        nbEchantillon = 100, valeurRef = 150)

par(mfrow=c(1,1), mar= c(5, 4, 4, 2) + 0.1)

#-------------------------------------------------------------------------------
# Distribution de la population 
#-------------------------------------------------------------------------------

hist(pop0, main ="Histogramme de la population 0", 
     col = adjustcolor("red", alpha.f = 0.3), 
     xlab = "Valeurs dans la population 0")

#===============================================================================
# Population A, B et C
#===============================================================================

#-------------------------------------------------------------------------------
# Histogrammes multiples
#-------------------------------------------------------------------------------

multiHistogrammes(popA, popB, popC, c("popA", "popB", "popC"))


#-------------------------------------------------------------------------------
# Test de Student
#-------------------------------------------------------------------------------

# A VS A
t.test(creatationEchantillon(popA, 100), creatationEchantillon(popA, 100))
t.test(creatationEchantillon(popA, 100), creatationEchantillon(popA, 100))
t.test(creatationEchantillon(popA, 100), creatationEchantillon(popA, 100))

# A VS B
t.test(creatationEchantillon(popA, 100), creatationEchantillon(popB, 100))
t.test(creatationEchantillon(popA, 100), creatationEchantillon(popB, 100))
t.test(creatationEchantillon(popA, 100), creatationEchantillon(popB, 100))

# A VS C
t.test(creatationEchantillon(popA, 100), creatationEchantillon(popC, 100))
t.test(creatationEchantillon(popA, 100), creatationEchantillon(popC, 100))
t.test(creatationEchantillon(popA, 100), creatationEchantillon(popC, 100))

#-------------------------------------------------------------------------------
# Test de Student répété
#-------------------------------------------------------------------------------

# A VS A
repetStudent(popA,popA , 100, 100, 10, reponseAttendue = "H0")
repetStudent(popA,popA , 100, 100, 10, reponseAttendue = "H0")
repetStudent(popA,popA , 100, 100, 10, reponseAttendue = "H0")
repetStudentAdjust(popA,popA , 100, 100, 10, reponseAttendue = "H0")

# A VS B
repetStudent(popA,popB , 100, 100, 10, reponseAttendue = "H0")
repetStudent(popA,popB , 100, 100, 10, reponseAttendue = "H0")
repetStudent(popA,popB , 100, 100, 10, reponseAttendue = "H0")
repetStudentAdjust(popA,popB , 100, 100, 10, reponseAttendue = "H0")

# A VS C
repetStudent(popA,popC , 100, 100, 10, reponseAttendue = "H0")
repetStudent(popA,popC , 100, 100, 10, reponseAttendue = "H0")
repetStudent(popA,popC , 100, 100, 10, reponseAttendue = "H0")
repetStudentAdjust(popA,popC , 100, 100, 10, reponseAttendue = "H0")


#-------------------------------------------------------------------------------
# Test de Student répété Taille de l'échantillon
#-------------------------------------------------------------------------------

# A VS A, B et C
vecErreurAA = NULL
vecErreurAB = NULL
vecErreurAC = NULL

for( sizeEch in seq(5,1000, 5)){
  vecErreurAA = c(vecErreurAA,repetStudent(popA,popA , sizeEch, sizeEch, 100, reponseAttendue = "H0", verbose = F))
  vecErreurAB = c(vecErreurAB,repetStudent(popA,popB , sizeEch, sizeEch, 100, reponseAttendue = "H1", verbose = F) )
  vecErreurAC = c(vecErreurAC,repetStudent(popA, popC, sizeEch, sizeEch, 100, reponseAttendue = "H1", verbose = F) )
}

plot(seq(5,1000, 5),vecErreurAA, type = "l", col = "red", las = 2,
     xlab = "Taille de l'échantillon",
     ylab = "Pourcentage d'erreur",
     main = "Effet de la taille de l'échantillon - PopA",
     ylim = c(0, 100))
lines(seq(5,1000, 5),vecErreurAB, col = "royalblue")
lines(seq(5,1000, 5),vecErreurAC, col = "forestgreen")
legend("topright", legend = c("A VS A", "A VS B", "A VS C"), lty = 1, col = c("red", "royalblue", "forestgreen"),
       box.lty = 0, inset = 0.01)

