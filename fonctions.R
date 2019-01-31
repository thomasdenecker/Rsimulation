################################################################################
# creatationEchantillon
################################################################################
#-----------------------------------------------------------------------------
# Objectifs
#-----------------------------------------------------------------------------
# Création d'un échantillon de la population
#-----------------------------------------------------------------------------
# Arguments
#-----------------------------------------------------------------------------
# population : un vecteur de valeurs
# taille : taille de l'échantillon (inférieur à la taille de la population)
#-----------------------------------------------------------------------------
# Retour
#-----------------------------------------------------------------------------
# Un vecteur de valeurs tirées aléatoirement

creatationEchantillon <- function(population, taille) {
  if (length(population) < taille) {
    print(
      paste(
        "La taille de l'échantillon demandé est plus grande que la taille de la population (",
        length(population),
        "<",
        taille,
        ")."
      )
    )
    return(NULL)
  } else {
    echantillon = sample(population, size = taille, replace = F)
    return(echantillon)
  }
}

################################################################################
# echantillonage
################################################################################
#-----------------------------------------------------------------------------
# Objectifs
#-----------------------------------------------------------------------------
# Création d'un échantillon de la population et visualisation de
# l'échantillon
#-----------------------------------------------------------------------------
# Arguments
#-----------------------------------------------------------------------------
# population : un vecteur de valeurs
# taille : taille de l'échantillon (inférieur à la taille de la population)
#-----------------------------------------------------------------------------
# Retour
#-----------------------------------------------------------------------------
# Un vecteur de valeurs tirees aleatoirement

echantillonage <- function(population, taille, titlePlot = NULL) {
  ech = creatationEchantillon(population, taille)
  if (!is.null(ech)) {
    if (is.null("titlePlot")) {
      plot(
        ech,
        rep(0, length(ech)),
        pch = 16,
        yaxt = 'n',
        ylab = "",
        ylim = c(-2, 2),
        xlab = "Valeur dans l'echantillon",
        col = adjustcolor("red", alpha.f = 0.3),
        cex.main = 1.2,
        main = "Distribution de l'echantillon"
      )
    } else {
      plot(
        ech,
        rep(0, length(ech)),
        pch = 16,
        yaxt = 'n',
        ylab = "",
        ylim = c(-2, 2),
        xlab = "Valeur dans l'echantillon",
        col = adjustcolor("red", alpha.f = 0.3),
        cex.main = 1.2,
        main = titlePlot
      )
    }
    
    abline(
      v = mean(ech),
      col = "royalblue",
      lty = 2,
      lwd = 2
    )
    text(
      mean(ech),
      y = 1,
      labels = paste0("Moyenne = ", round(mean(ech), 2)),
      pos = 4
    )
  }
  return(ech)
}

################################################################################
# echantillonageMultiple
################################################################################
#---------------------------------------------------------------------------
# Objectifs
#---------------------------------------------------------------------------
# Realisation d'un echantillonage multiple, representation graphique des 5
# premier echantillonage et visualisation graphique en histogramme des
# moyennes des echantillons
#---------------------------------------------------------------------------
# Arguments
#---------------------------------------------------------------------------
# population : un vecteur de valeurs
# taille : taille de l'echantillon (inferieur à la taille de la population)
# nbEchantillon : nombre d'échantillonage à réaliser
# valeurRef : valeur de référence
#---------------------------------------------------------------------------
# Retour
#---------------------------------------------------------------------------
# vecMean : un vecteur des moyennes des échantillons

echantillonageMultiple <-
  function(population,
           taille,
           nbEchantillon,
           valeurRef = NULL) {

    
    vecMean = NULL
    if (nbEchantillon < 5) {
      layout(matrix(c(
        1:(nbEchantillon + 1),
        1,
        rep(nbEchantillon + 2, nbEchantillon)
      ), nbEchantillon + 1, 2, byrow = FALSE))
    } else {
      layout(matrix(c(1:6, 1, rep(7, 5)), 6, 2, byrow = FALSE), 
             heights = c(0.1, rep(0.18, 5)))
    }
    
    par(mar = rep(0, 4))
    plot.new()
    text(0.5,
         0.5,
         "Echantillonage multiple",
         cex = 1.5,
         font = 2)
    
    par(mar =  c(2, 2, 2, 2))
    
    for (i in 1:nbEchantillon) {
      if (i <= 5) {
        ech = echantillonage(population,
                             taille,
                             paste("Distribution de l'échantillon", i))
      } else {
        ech = creatationEchantillon(population, taille)
      }
      
      vecMean = c(vecMean, mean(ech))
    }
    
    hist(
      vecMean,
      breaks = 10,
      col = adjustcolor("red", alpha.f = 0.3),
      border = "red",
      main = "Distribution des moyennes"
    )
    if (!is.null(valeurRef)) {
      abline(v = valeurRef, lty = 2, lwd = 2)
      
    }
    par(mfrow=c(1,1), mar= c(5, 4, 4, 2) + 0.1)
    return(vecMean)
  }

################################################################################
# multiHistogrammes
################################################################################
#-----------------------------------------------------------------------------
# Objectifs
#-----------------------------------------------------------------------------
# Réalisation d'une figure avec en A un histogramme des populations en
# fréquence et en B un histogramme des densités
#-----------------------------------------------------------------------------
# Arguments
#-----------------------------------------------------------------------------
# pop1, pop2, pop3 : des vecteurs de valeurs
# names : vecteur de taille 3 avec les noms des populations
#-----------------------------------------------------------------------------
# Retour
#-----------------------------------------------------------------------------
# Pas de valeur retournée

# Préparation de la table


multiHistogrammes <- function(pop1, pop2, pop3, names) {
  dat = cbind(c(pop1, pop2, pop3),
              c(
                rep(names[1], length(pop1)),
                rep(names[2], length(pop2)),
                rep(names[3], length(pop3))
              ))
  
  colnames(dat) = c('Value', 'Population')
  dat = as.data.frame(dat)
  dat[, 1] = as.numeric(as.character(dat[, 1]))
  
  meanDat <- dat %>%
    group_by(Population) %>%
    summarise(avg = mean(Value))
  
  histo <- ggplot(dat, aes(x = Value, fill = Population)) +
    geom_histogram(binwidth = .5,
                   alpha = .5,
                   position = "identity") +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(
        size = 0.5,
        linetype = "solid",
        colour = "black"
      )
    ) + labs(y = "Frequency")
  
  
  densite <- ggplot(dat, aes(x = Value, fill = Population)) +
    geom_density(alpha = .3) +
    geom_vline(
      data = meanDat,
      aes(xintercept = avg,  colour = Population),
      linetype = "dashed",
      size = 1
    ) +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(
        size = 0.5,
        linetype = "solid",
        colour = "black"
      ),
      legend.position = "none"
    ) + labs(y = "Density")
  
  legend <- get_legend(histo)
  histo <- histo + theme(legend.position = "none")
  grid.arrange(histo,
               densite,
               legend,
               ncol = 3,
               widths = c(2.3, 2.3, 0.8))
  
  
  ggdraw() +
    draw_plot(histo, x = 0, width = 0.43) +
    draw_plot(densite, x = 0.43, width = 0.43) +
    draw_plot(legend, x = 0.86, width = 0.14) +
    draw_plot_label(c("A", "B", ""), c(0, 0.43, 0.86), c(1, 1, 1), size = 15)
  
  
}

################################################################################
# repetStudent
################################################################################
#---------------------------------------------------------------------------
# Objectifs
#---------------------------------------------------------------------------
# Réalisation de tests multiples pour comparer l'échantillonage entre
# 2 populations. Le test réalisé est un test de student par défaut
#---------------------------------------------------------------------------
# Arguments
#---------------------------------------------------------------------------
# pop1, pop2 : des vecteurs de valeurs
# taillePop1, taillePop2 : vecteur de taille 3 avec les noms des populations
# pvalue : seuil pour déterminer si H0 ou H1
# reponseAttendue : H0 ou H1
# verbose : avec ou sans print
#---------------------------------------------------------------------------
# Retour
#---------------------------------------------------------------------------
# Pas de valeur retournée

repetStudent <-
  function(pop1,
           pop2 ,
           tailleEch1,
           tailleEch2,
           nbrTest,
           pvalue = 0.05,
           reponseAttendue,
           verbose = FALSE) {
    
    if (reponseAttendue != "H0" && reponseAttendue != "H1") {
      stop("reponseAtendue doit être soit H0 soit H1")
    } else {
      if (verbose) {
        print("Rappel des hypothèses : ")
        print("H0 : les deux échantillons font partie de la même population.")
        print("H1 : les deux échantillons ne font pas partie de la même population.")
        print(paste("Pvalue sélectionnée :", pvalue))
      }
      
      erreur = 0
      
      for (i in 1:nbrTest) {
        test = t.test(
          creatationEchantillon(pop1, tailleEch1),
          creatationEchantillon(pop2, tailleEch2)
        )
        if (test$p.value > pvalue) {
          reponse = "H0"
        } else {
          reponse = "H1"
        }
        
        if (reponseAttendue == reponse) {
          if (verbose) {
            print(paste(i, "- Bonne réponse"))
          }
        } else {
          if (verbose) {
            print(paste(i, "- Mauvaise réponse"))
          }
          erreur = erreur + 1
        }
      }
      
      if (verbose) {
        print(paste("Pourcentage d'erreur :", erreur * 100 / nbrTest, "%"))
      }
      
      return(erreur * 100 / nbrTest)
    }
  }

################################################################################
# repetStudentAdjust
################################################################################

#---------------------------------------------------------------------------
# Objectifs
#---------------------------------------------------------------------------
# Réalisation de tests multiples pour comparer l'échantillonage entre
# 2 populations. Le test réalisé est un test de student par défaut
#---------------------------------------------------------------------------
# Arguments
#---------------------------------------------------------------------------
# pop1, pop2 : des vecteurs de valeurs
# taillePop1, taillePop2 : vecteur de taille 3 avec les noms des populations
# pvalue : seuil pour déterminer si H0 ou H1
# reponseAttendue : H0 ou H1
# methodAdjust : méthode d'ajustement de la P-value
#---------------------------------------------------------------------------
# Retour
#---------------------------------------------------------------------------
# Pas de valeur retournée

repetStudentAdjust <-
  function(pop1,
           pop2 ,
           tailleEch1,
           tailleEch2,
           nbrTest,
           pvalue = 0.05,
           reponseAttendue,
           methodAdjust = "bonferroni") {
    
    if (reponseAttendue != "H0" && reponseAttendue != "H1") {
      stop("reponseAtendue doit être soit H0 soit H1")
    } else {
      print("Rappel des hypothèses : ")
      print("H0 : les deux échantillons font partie de la même population.")
      print("H1 : les deux échantillons ne font pas partie de la même population.")
      print(paste("Pvalue sélectionnée :", pvalue))
      
      erreur = 0
      pvalueVec = NULL
      for (i in 1:nbrTest) {
        test = t.test(
          creatationEchantillon(pop1, tailleEch1),
          creatationEchantillon(pop2, tailleEch2)
        )
        pvalueVec = c(pvalueVec, test$p.value)
      }
      
      padjust = p.adjust(pvalueVec, method = methodAdjust)
      
      hypotheses = rep("H1", length(padjust))
      hypotheses[which(padjust > pvalue)] = "H0"
      
      erreur = sum(hypotheses != reponseAttendue) * 100 / nbrTest
      print(paste("Pourcentage d'erreur :", erreur , "%"))
      
      return(erreur)
    }
  }
