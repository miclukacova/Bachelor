##############################################################################
###Indlæsning af pakker og data:
library(randomForest)
library(quantregForest)
library(tidyverse)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(xtable)

set.seed(777)
leafs <- read.csv('Data/leafs.csv')
wood<- read.csv('Data/wood.csv')
roots <- read.csv('Data/roots.csv')
##############################################################################


#----------------------Creating Bootstrap function---------------------------

#Model skal spytte et alpha og et beta estimat ud.


bootstrap <- function(model, data, B) {
  
  for (i in 1:nrow(data)) {
    #Fitting model on training-data:
    fitted_model <- model(data$Sc[-i])
    
    #Computing residuals:
    log_e <- log(data$Kgp[-i]) - log(fitted_model$beta) - log(data$Sc[-1])*fitted_model$alpha
    
    #Bootstrapping:
    for (b in 1:B) {
      boot_data <- data.frame
    }
  }
  
  
  
  #
}
  
  
  
  
  {
  #Obtaining standardized residuals 
  Y_p <- model$par[1]*(data_test$Sc)^(model$par[2])
  
  rep_e_n1 <- function(nlr, data, X_n_1){
    #Make bootstrapped data
    boot_data <- data[sample(nrow(data), replace = TRUE),]
    
    # Do bootstrap NLR
    boot_parameters <- optim(par = starting_point_leafs, fn = MSE_NLR, data = boot_data)
    boot_model <- function(x){boot_parameters$par[1]*(x)^(boot_parameters$par[2])}
    
    # Calculating residuals
    bs_res <- data$Kgp - boot_model(data$Sc)
    
    # Calculate draw on prediction error
    e_N_1 <- model$par[1]*(X_n_1)^(model$par[2]) - boot_model(X_n_1) + sample(bs_res,size=1)
    
    return(unname(e_N_1))
  }
  
  #Replicate
  
  ep <- matrix(nrow = B, ncol = nrow(data_test))
  
  for (i in (1:nrow(data_test))){
    for (j in (1:B)){
      ep[j,i] <- rep_e_n1(model, data_train, data_test[i,1])
    }
  }
  return(list(ep, Y_p))
}

leafs_boot <- boot(NLE_leafs, leafs_train, test_leafs, 100)
wood_boot <- boot(NLE_wood, wood_train, test_wood, 100)
roots_boot <- boot(NLE_roots, roots_train, test_roots, 100)


#Creating the prediction intervals

pred_int <- function(boot, data, alpha) {
  down <- c()
  up <- c()
  
  for (i in ncol(boot[[1]])){
    down <- boot[[2]] + quantile(boot[[1]][,i], probs = alpha/2)
    up <- boot[[2]] + quantile(boot[[1]][,i], probs = 1-alpha/2)
  } 
  
  plot_data <- tibble("Sc" = data$Sc, "Kgp" = data$Kgp,
                      "Pred" = boot[[2]], "down" = down, "up" = up) %>%
    mutate(Sc = Sc, Kgp = Kgp, down = down, up = up)%>%
    mutate(indicator = if_else((down <= Kgp)&(Kgp <= up),"in", "out"))
  
  return(plot_data)
}











