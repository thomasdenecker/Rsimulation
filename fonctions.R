

creatationEchantillon <- function(population, taille){
  #-----------------------------------------------------------------------------
  # Objectifs
  #-----------------------------------------------------------------------------
  # Cr�ation d'un �chantillon de la population
  #-----------------------------------------------------------------------------
  # Arguments 
  #-----------------------------------------------------------------------------
  # population : un vecteur de valeurs
  # taille : taille de l'�chantillon (inf�rieur � la taille de la population)
  #-----------------------------------------------------------------------------
  # Retour
  #-----------------------------------------------------------------------------
  # Un vecteur de valeurs tir�es al�atoirement
  
  if(length(population) < taille){
    print(paste("La taille de l'�chantillon demand� est plus grande que la taille de la population (", 
                length(population), "<", taille, ")."))
    return(NULL)
  } else {
    echantillon = sample(population, size = taille, replace = F)
    return(echantillon)
  }
}

echantillonage <- function(population, taille, titlePlot = NULL) {
  #-----------------------------------------------------------------------------
  # Objectifs
  #-----------------------------------------------------------------------------
  # Cr�ation d'un �chantillon de la population et visualisation de 
  # l'�chantillon
  #-----------------------------------------------------------------------------
  # Arguments 
  #-----------------------------------------------------------------------------
  # population : un vecteur de valeurs
  # taille : taille de l'�chantillon (inf�rieur � la taille de la population)
  #-----------------------------------------------------------------------------
  # Retour
  #-----------------------------------------------------------------------------
  # Un vecteur de valeurs tir�es al�atoirement
  
  ech = creatationEchantillon(population, taille)
  if(!is.null(ech)){
    
    if(is.null("titlePlot")){
      plot(ech, rep(0, length(ech)), pch = 16,
           yaxt='n', ylab = "", ylim = c(-2,2),
           xlab = "Valeur dans l'�chantillon",
           col = adjustcolor("red", alpha.f = 0.3),
           cex.main=1.2,
           main = "Distribution de l'�chantillon")
    } else {
      plot(ech, rep(0, length(ech)), pch = 16,
           yaxt='n', ylab = "", ylim = c(-2,2),
           xlab = "Valeur dans l'�chantillon", 
           col = adjustcolor("red", alpha.f = 0.3),
           cex.main=1.2,
           main = titlePlot)
    }
    
    abline(v = mean(ech),  col = "royalblue", lty = 2, lwd = 2)
    text(mean(ech), y= 1, 
         labels = paste0("Moyenne = ", round(mean(ech),2)), 
         pos = 4)
  }
  return(ech)
}

echantillonageMultiple <- function(population, taille, nbEchantillon, valeurRef = NULL) {
  #-----------------------------------------------------------------------------
  # Objectifs
  #-----------------------------------------------------------------------------
  # 
  #-----------------------------------------------------------------------------
  # Arguments 
  #-----------------------------------------------------------------------------
  # 
  #-----------------------------------------------------------------------------
  # Retour
  #-----------------------------------------------------------------------------
  # 
  
  vecMean = NULL
  if(nbEchantillon < 5){
    layout(matrix(c(1:(nbEchantillon+1), 1, rep(nbEchantillon + 2, nbEchantillon)), nbEchantillon+1, 2, byrow = FALSE))
  } else {
    layout(matrix(c(1:6, 1, rep(7, 5)), 6, 2, byrow = FALSE), heights = c(0.1, rep(0.18,5)))
  }
  
  par(mar = rep(0, 4))
  plot.new()
  text(0.5,0.5,"Echantillonage multiple",cex=1.5,font=2)
  
  par(mar =  c(2, 2, 2, 2))
  
  for(i in 1:nbEchantillon){
    if(i <= 5){
      ech = echantillonage(population, taille, paste("Distribution de l'�chantillon",i))
    } else {
      ech = creatationEchantillon(population, taille)
    }
    
    vecMean = c(vecMean, mean(ech))
  }
  
  hist(vecMean, breaks = 10, col=adjustcolor("red", alpha.f = 0.3), border = "red",
       main="Distribution des moyennes")
  if (!is.null(valeurRef)){
    abline(v = valeurRef, lty = 2, lwd = 2)
    
  } 

  return(vecMean)
}
