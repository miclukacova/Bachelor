#################################--Indlæsning af pakker og data---###############################

train_leafs_log <- read.csv('Data/train_leafs_log.csv')
train_roots_log <- read.csv('Data/train_roots_log.csv')
train_wood_log<- read.csv('Data/train_wood_log.csv')

test_leafs_log <- read.csv('Data/test_leafs_log.csv')
test_roots_log <- read.csv('Data/test_roots_log.csv')
test_wood_log<- read.csv('Data/test_wood_log.csv')

leafs_log <- read.csv('Data/leafs_log.csv')
roots_log <- read.csv('Data/roots_log.csv')
wood_log<- read.csv('Data/wood_log.csv')

leafs_train <- read.csv('Data/train_leafs.csv')
roots_train <- read.csv('Data/train_roots.csv')
wood_train<- read.csv('Data/train_wood.csv')

test_leafs <- read.csv('Data/test_leafs.csv')
test_roots <- read.csv('Data/test_roots.csv')
test_wood <- read.csv('Data/test_wood.csv')

library(tidyverse)
library(readr)
library(infer)
library(foreign)
library(xtable)
library(stargazer)
###################################################################################################

###################################################################################################
#Log-log OLS
###################################################################################################

lm_leafs_log <- lm(Kgp ~ Sc, data = train_leafs_log)
lm_roots_log <- lm(Kgp ~ Sc, data = train_roots_log)
lm_wood_log <- lm(Kgp ~ Sc, data = train_wood_log)

##Bootstrap som på stackexchange:
#(koden er taget herfra: https://stats.stackexchange.com/questions/226565/bootstrap-prediction-interval)

set.seed(72)
boot <- function(model, data_train, data_test, B) {
  #Obtaining standardized residuals 
  Y_p <- coef(model)["(Intercept)"] + coef(model)["Sc"]*data_test$Sc
  leverage <- influence(model)$hat
  s_res <- residuals(model)/sqrt(1-leverage)
  my_s_res <- s_res - mean(s_res)
  
  rep_e_n1 <- function(lm, residuals, X_n_1){
    # Make bootstrap residuals
    e_star <- sample(residuals, size = length(lm$residuals), replace = TRUE)
    
    # Make bootstrap Y
    y_star <- fitted(lm)+e_star
    
    # Do bootstrap regression
    x <- model.frame(lm)[,2]
    new_model <- lm(y_star~x)
    
    # Create bootstrapped adjusted residuals
    bs_lev <- influence(new_model)$hat
    bs_res   <- residuals(new_model)/sqrt(1-bs_lev)
    bs_res   <- bs_res - mean(bs_res)
    
    # Calculate draw on prediction error
    beta_beta <- c(coef(model)["(Intercept)"] - coef(new_model)["(Intercept)"],
                   (coef(model)["Sc"] - coef(new_model)["x"]))
    e_N_1 <- t(beta_beta)%*%c(1,X_n_1) + sample(bs_res,size=1)
    
    return(unname(e_N_1))
  }
  
  #Replicate
  
  ep <- matrix(nrow = B, ncol = nrow(data_test))
  
  for (i in (1:nrow(data_test))){
    for (j in (1:B)){
      ep[j,i] <- rep_e_n1(model, my_s_res, data_test[i,1])
    }
  }
  return(list("error" = ep, "Pred" = Y_p))
}

#Performing the bootstrap

loo_boot <- function(data){
  error <- c()
  pred <- c()
  for (i in (1:nrow(data))){
    lm <- lm(Kgp ~ Sc, data = data[-i,])
    boot <- boot(lm, data[-i,], data[i,], 100)
    append(error, boot[[1]])
    append(pred, boot[[2]])
  }
}





wood_boot <- boot(lm_wood_log, train_wood_log, test_wood_log, 100)
roots_boot <- boot(lm_roots_log, train_roots_log, test_roots_log, 100)

#Creating the prediction intervals

pred_int <- function(boot, data,alpha) {
  down <- c()
  up <- c()
  
  for (i in ncol(boot[[1]])){
    down <- boot[[2]] + quantile(boot[[1]][,i], probs = alpha/2)
    up <- boot[[2]] + quantile(boot[[1]][,i], probs = 1-alpha/2)
  } 
  
  plot_data <- tibble("Sc" = data$Sc, "Kgp" = data$Kgp,
                            "Pred" = boot[[2]], "down" = down, "up" = up) %>%
    mutate(Sc = exp(Sc), Kgp = exp(Kgp), down = exp(down), up = exp(up))%>%
    mutate(indicator = if_else((down <= Kgp)&(Kgp <= up),"in", "out"))
  
  return(plot_data)
}

#Plot

plot_func <- function(lm, pred_int, title){
  color <- c("in" = "darkolivegreen", "out" = "darkolivegreen3")
  
  pred_func <- function(x) exp(coef(lm)["(Intercept)"])*
    x^coef(lm)["Sc"]*exp(var(lm$residuals)/2)
  
  ggplot(pred_int) + 
    geom_point(aes(x = Sc, y = down), color = 'hotpink') +
    geom_point(aes(x = Sc, y = up), color = 'hotpink') +
    geom_function(fun = pred_leafs, color = "hotpink4") +
    geom_point(aes(x = Sc, y = Kgp, color = indicator)) +
    theme_bw() +
    xlab('Sc') + 
    ylab('Kgp')+
    labs(title = title)+
    scale_color_manual(values = color)
}


pred_int_leafs <- pred_int(leafs_boot, leafs_log_test, 0.2) 
pred_int_wood <- pred_int(wood_boot, wood_log_test, 0.2)
pred_int_roots <- pred_int(roots_boot, roots_log_test, 0.2)





ggplot(pred_int_wood) + 
  geom_point(aes(x = Sc, y = down), color = 'hotpink') +
  geom_point(aes(x = Sc, y = up), color = 'hotpink') +
  geom_function(fun = pred_wood, color = "hotpink4") +
  geom_point(aes(x = Sc, y = Kgp, color = indicator)) +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Wood")+
  scale_color_manual(values = color)

ggplot(pred_int_roots)+ 
  geom_point(aes(x = Sc, y = down), color = 'hotpink') +
  geom_point(aes(x = Sc, y = up), color = 'hotpink') +
  geom_function(fun = pred_roots, color = "hotpink4") +
  geom_point(aes(x = Sc, y = Kgp, color = indicator)) +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Roots")+
  scale_color_manual(values = color)

#Coverage of test sets

coverage <- function(pred_int){
  mean(pred_int$down <= pred_int$Kgp & pred_int$Kgp <= pred_int$up)
}

a <- coverage(pred_int_leafs)
b <- coverage(pred_int_wood)
c <- coverage(pred_int_roots)

xtable(tibble("Data" = c("Leafs", "Wood", "Roots"), "Coverage" = c(a,b,c)), type = latex)

#For different alphas

alphas <- c(0.01, 0.05, 0.1, 0.2)
cov_alpha_l <- c()
cov_alpha_w <- c()
cov_alpha_r <- c()

for (i in (1:4)){
  pred_int_alpha <- pred_int(leafs_boot, leafs_log_test, alphas[i]) 
  cov_alpha_l[i] <- coverage(pred_int_alpha)
}

for (i in (1:4)){
  pred_int_alpha <- pred_int(wood_boot, wood_log_test, alphas[i]) 
  cov_alpha_w[i] <- coverage(pred_int_alpha)
}

for (i in (1:4)){
  pred_int_alpha <- pred_int(roots_boot, roots_log_test, alphas[i]) 
  cov_alpha_r[i] <- coverage(pred_int_alpha)
}

xtable(tibble("Signif. level" = alphas, "Leafs" = cov_alpha_l, 
              "Wood" = cov_alpha_w, "Roots" = cov_alpha_r))


#Checking for correct coverage

rs_cov <- function(data, k, alpha) {
  cov <- c()
  n <- nrow(data)
  sample_size <- floor(0.8*n)
  
  for (i in (1:k)){
    # Test and train
    picked_rs <- sample(n,size = sample_size)
    train_rs = data[picked_rs,]
    test_rs = data[-picked_rs,]
    
    # Fit model
    lm_rs <- lm(Kgp ~ Sc, data = train_rs)
    
    #Perform bootstrap
    boot_rs <- boot(lm_rs, train_rs, test_rs, 100)
    
    # Quantiles
    pred_int_rs <- pred_int(boot_rs, test_rs, alpha) 
    
    #Definere
    cov[i] <- coverage(pred_int_rs)
  }
  return(tibble("Coverage" = cov))
}

set.seed(4)
a <- rs_cov(leafs_log, 30, 0.1)
b <- rs_cov(wood_log, 30, 0.1)
c <- rs_cov(roots_log, 30,0.1)

#Mean coverage:
mean_a <- mean(a$Coverage)
mean_b <- mean(b$Coverage)
mean_c <- mean(c$Coverage) 

median(a$Coverage)
median(b$Coverage)
median(c$Coverage) 

xtable(tibble(Data = c("Leafs", "Wood", "Roots"), 
              "Mean coverage" =c(mean_a, mean_b, mean_c)), type = latex)

a %>%
  ggplot() +
  geom_histogram(aes(x = Coverage, y = ..density..), color = "white", 
                 fill = "darkolivegreen3", bins = 30)+
  geom_vline(xintercept = 0.9, color = "hotpink") +
  xlim(0,1)+
  theme_bw()+
  labs(title = "Foliage")

b %>%
  ggplot() +
  geom_histogram(aes(x = Coverage, y = ..density..), color = "white", 
                 fill = "darkolivegreen3", bins = 30)+
  geom_vline(xintercept = 0.9, color = "hotpink") +
  theme_bw()+
  xlim(c(0.5,1))+
  labs(title = "Wood")

c %>%
  ggplot() +
  geom_histogram(aes(x = Coverage, y = ..density..), color = "white", 
                 fill = "darkolivegreen3", bins = 30)+
  geom_vline(xintercept = 0.9, color = "hotpink") +
  theme_bw()+
  xlim(c(0,1.1))+
  labs(title = "Roots")


###################################################################################################
#OLS
###################################################################################################

lm_leafs <- lm(Kgp ~ Sc, data = leafs_train)
lm_roots <- lm(Kgp ~ Sc, data = roots_train)
lm_wood <- lm(Kgp ~ Sc, data = wood_train)

#Performing the bootstrap

leafs_boot2 <- boot(lm_leafs, leafs_train, test_leafs, 100)
wood_boot2 <- boot(lm_wood, wood_train, test_wood, 100)
roots_boot2 <- boot(lm_roots, roots_train, test_roots, 100)

#Creating the prediction intervals

pred_int2 <- function(boot, data, alpha) {
  down <- c()
  up <- c()
  
  for (i in ncol(boot[[1]])){
    down <- boot[[2]] + quantile(boot[[1]][,i], probs = alpha/2)
    up <- boot[[2]] + quantile(boot[[1]][,i], probs = 1-alpha/2)
  } 
  
  plot_data <- tibble("Sc" = data$Sc, "Kgp" = data$Kgp,
                      "Pred" = boot[[2]], "down" = down, "up" = up) %>%
    mutate(indicator = if_else((down <= Kgp)&(Kgp <= up),"in", "out"))
  
  return(plot_data)
}

pred_int_leafs2 <- pred_int2(leafs_boot2, test_leafs, 0.1) 
pred_int_wood2 <- pred_int2(wood_boot2, test_wood, 0.1)
pred_int_roots2 <- pred_int2(roots_boot2, test_roots, 0.1)

#Plot

pred_leafs2 <- function(x) coef(lm_leafs)["(Intercept)"] + x*coef(lm_leafs)["Sc"]
pred_wood2 <- function(x) coef(lm_wood)["(Intercept)"] + x*coef(lm_wood)["Sc"]
pred_roots2 <- function(x) coef(lm_roots)["(Intercept)"] +x*coef(lm_roots)["Sc"]

color <- c("in" = "darkolivegreen", "out" = "darkolivegreen3")

ggplot(pred_int_leafs2) + 
  geom_point(aes(x = Sc, y = down), color = 'hotpink') +
  geom_point(aes(x = Sc, y = up), color = 'hotpink') +
  geom_function(fun = pred_leafs2, color = "hotpink4") +
  geom_point(aes(x = Sc, y = Kgp, color = indicator)) +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Leafs")+
  scale_color_manual(values = color)


ggplot(pred_int_wood2) + 
  geom_point(aes(x = Sc, y = down), color = 'hotpink') +
  geom_point(aes(x = Sc, y = up), color = 'hotpink') +
  geom_function(fun = pred_wood2, color = "hotpink4") +
  geom_point(aes(x = Sc, y = Kgp, color = indicator)) +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Wood")+
  scale_color_manual(values = color)

ggplot(pred_int_roots2)+ 
  geom_point(aes(x = Sc, y = down), color = 'hotpink') +
  geom_point(aes(x = Sc, y = up), color = 'hotpink') +
  geom_function(fun = pred_roots2, color = "hotpink4") +
  geom_point(aes(x = Sc, y = Kgp, color = indicator)) +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Roots")+
  scale_color_manual(values = color)

#Coverage

coverage <- function(pred_int){
  mean(pred_int$down <= pred_int$Kgp & pred_int$Kgp <= pred_int$up)
}

a2 <- coverage(pred_int_leafs2)
b2 <- coverage(pred_int_wood2)
c2 <- coverage(pred_int_roots2)

xtable(tibble("Data" = c("Leafs", "Wood", "Roots"), "Coverage" = c(a2,b2,c2)), type = latex)

#For different alphas

alphas <- c(0.01, 0.05, 0.1, 0.2)
cov_alpha_l2 <- c()
cov_alpha_w2 <- c()
cov_alpha_r2 <- c()

for (i in (1:4)){
  pred_int_alpha <- pred_int2(leafs_boot2, leafs_test, alphas[i]) 
  cov_alpha_l2[i] <- coverage(pred_int_alpha)
}

for (i in (1:4)){
  pred_int_alpha <- pred_int2(wood_boot2, wood_test, alphas[i]) 
  cov_alpha_w2[i] <- coverage(pred_int_alpha)
}

for (i in (1:4)){
  pred_int_alpha <- pred_int2(roots_boot2, roots_test, alphas[i]) 
  cov_alpha_r2[i] <- coverage(pred_int_alpha)
}


#Checking for correct coverage

rs_cov <- function(data, k, alpha) {
  cov <- c()
  n <- nrow(data)
  sample_size <- floor(0.8*n)
  
  for (i in (1:k)){
    # Test and train
    picked_rs <- sample(n,size = sample_size)
    train_rs = data[picked_rs,]
    test_rs = data[-picked_rs,]
    
    # Fit model
    lm_rs <- lm(Kgp ~ Sc, data = train_rs)
    
    #Perform bootstrap
    boot_rs <- boot(lm_rs, train_rs, test_rs, 100)
    
    # Quantiles
    pred_int_rs <- pred_int2(boot_rs, test_rs, alpha) 
    
    #Definere
    cov[i] <- coverage(pred_int_rs)
  }
  return(tibble("Coverage" = cov))
}

#Det her tager ægte lang tid, jeg ved ikke om det går godt...

set.seed(4)
a <- rs_cov(leafs, 30, 0.1)
b <- rs_cov(wood, 30, 0.1)
c <- rs_cov(roots, 30,0.1)

#Mean coverage:
mean_a <- mean(a$Coverage)
mean_b <- mean(b$Coverage)
mean_c <- mean(c$Coverage) 

xtable(tibble(Data = c("Leafs", "Wood", "Roots"), 
              "Mean coverage" =c(mean_a, mean_b, mean_c)), type = latex)

a %>%
  ggplot() +
  geom_histogram(aes(x = Coverage, y = ..density..), color = "white", 
                 fill = "darkolivegreen3", bins = 30)+
  geom_vline(xintercept = 0.9, color = "hotpink") +
  xlim(0,1)+
  theme_bw()+
  labs(title = "Foliage")

b %>%
  ggplot() +
  geom_histogram(aes(x = Coverage, y = ..density..), color = "white", 
                 fill = "darkolivegreen3", bins = 30)+
  geom_vline(xintercept = 0.9, color = "hotpink") +
  theme_bw()+
  xlim(c(0.5,1))+
  labs(title = "Wood")

c %>%
  ggplot() +
  geom_histogram(aes(x = Coverage, y = ..density..), color = "white", 
                 fill = "darkolivegreen3", bins = 30)+
  geom_vline(xintercept = 0.9, color = "hotpink") +
  theme_bw()+
  xlim(c(0,1))+
  labs(title = "Roots")


##############################################################################
#Attempting to create a non-linear bootstrap:

#Boostrap using NLR:

#Defining the models and the starting-points we found when obtaining the model:
#Creating function to be minimized
MSE_NLR <- function(par, data){
  with(data, sum((Kgp-par[1]*Sc^par[2])^2))
}


starting_point_leafs <- c(0.61,0.81)
starting_point_wood <- c(5.51,0.73)
starting_point_roots <- c(3.66,0.1)

NLE_leafs <- optim(par = starting_point_leafs, fn = MSE_NLR, data = leafs_train)
NLE_wood <- optim(par = starting_point_wood, fn = MSE_NLR, data = wood_train)
NLE_roots <- optim(par = starting_point_roots, fn = MSE_NLR, data = roots_train)

leafs_model <- function(x){NLE_leafs$par[1]*(x)^(NLE_leafs$par[2])}
wood_model <- function(x){NLE_wood$par[1]*(x)^(NLE_wood$par[2])}
roots_model <- function(x){NLE_roots$par[1]*(x)^(NLE_roots$par[2])}


#Defining NLR-specific bootstrapping function:
set.seed(72)
boot <- function(model, data_train, data_test, B) {
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

#Plot

pred_int_leafs <- pred_int(leafs_boot, test_leafs, 0.1) 
pred_int_wood <- pred_int(wood_boot, test_wood, 0.1)
pred_int_roots <- pred_int(roots_boot, test_roots, 0.1)

color <- c("in" = "darkolivegreen", "out" = "darkolivegreen3")

ggplot(pred_int_leafs) + 
  geom_point(aes(x = Sc, y = down), color = 'hotpink') +
  geom_point(aes(x = Sc, y = up), color = 'hotpink') +
  geom_function(fun = leafs_model, color = "hotpink4") +
  geom_point(aes(x = Sc, y = Kgp, color = indicator)) +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Leafs")+
  scale_color_manual(values = color)


ggplot(pred_int_wood) + 
  geom_point(aes(x = Sc, y = down), color = 'hotpink') +
  geom_point(aes(x = Sc, y = up), color = 'hotpink') +
  geom_function(fun = wood_model, color = "hotpink4") +
  geom_point(aes(x = Sc, y = Kgp, color = indicator)) +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Wood")+
  scale_color_manual(values = color)

ggplot(pred_int_roots)+ 
  geom_point(aes(x = Sc, y = down), color = 'hotpink') +
  geom_point(aes(x = Sc, y = up), color = 'hotpink') +
  geom_function(fun = roots_model, color = "hotpink4") +
  geom_point(aes(x = Sc, y = Kgp, color = indicator)) +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Roots")+
  scale_color_manual(values = color)

#Coverage

coverage <- function(pred_int){
  mean(pred_int$down <= pred_int$Kgp & pred_int$Kgp <= pred_int$up)
}

a2 <- coverage(pred_int_leafs)
b2 <- coverage(pred_int_wood)
c2 <- coverage(pred_int_roots)

xtable(tibble("Data" = c("Leafs", "Wood", "Roots"), "Coverage" = c(a2,b2,c2)), type = latex)

#For different alphas

alphas <- c(0.01, 0.05, 0.1, 0.2)
cov_alpha_l2 <- c()
cov_alpha_w2 <- c()
cov_alpha_r2 <- c()

for (i in (1:4)){
  pred_int_alpha <- pred_int2(leafs_boot2, leafs_test, alphas[i]) 
  cov_alpha_l2[i] <- coverage(pred_int_alpha)
}

for (i in (1:4)){
  pred_int_alpha <- pred_int2(wood_boot2, wood_test, alphas[i]) 
  cov_alpha_w2[i] <- coverage(pred_int_alpha)
}

for (i in (1:4)){
  pred_int_alpha <- pred_int2(roots_boot2, roots_test, alphas[i]) 
  cov_alpha_r2[i] <- coverage(pred_int_alpha)
}


#Checking for correct coverage
leafs <- read.csv("Data/leafs.csv")
wood <- read.csv("Data/wood.csv")
roots <- read.csv("Data/roots.csv")

rs_cov <- function(data, k, alpha, starting_points) {
  cov <- c()
  n <- nrow(data)
  sample_size <- floor(0.8*n)
  
  for (i in (1:k)){
    # Test and train
    picked_rs <- sample(n,size = sample_size)
    train_rs = data[picked_rs,]
    test_rs = data[-picked_rs,]
    
    # Fit NLR-model
    lm_rs <- optim(par = starting_points, fn = MSE_NLR, data = train_rs)
    
    #Perform bootstrap
    boot_rs <- boot(lm_rs, train_rs, test_rs, 100)
    
    # Quantiles
    pred_int_rs <- pred_int(boot_rs, test_rs, alpha) 
    
    #Definere
    cov[i] <- coverage(pred_int_rs)
  }
  return(tibble("Coverage" = cov))
}

#Det her tager ægte lang tid, jeg ved ikke om det går godt...

set.seed(4)
a <- rs_cov(leafs, 30, 0.1, starting_point_leafs)
b <- rs_cov(wood, 30, 0.1, starting_point_wood)
c <- rs_cov(roots, 30,0.1, starting_point_roots)


#Mean coverage:
mean_a <- mean(a$Coverage)
mean_b <- mean(b$Coverage)
mean_c <- mean(c$Coverage) 

xtable(tibble(Data = c("Leafs", "Wood", "Roots"), 
              "Mean coverage" =c(mean_a, mean_b, mean_c)), type = latex)

a %>%
  ggplot() +
  geom_histogram(aes(x = Coverage, y = ..density..), color = "white", 
                 fill = "darkolivegreen3", bins = 30)+
  geom_vline(xintercept = 0.9, color = "hotpink") +
  xlim(0,1)+
  theme_bw()+
  labs(title = "Foliage")

b %>%
  ggplot() +
  geom_histogram(aes(x = Coverage, y = ..density..), color = "white", 
                 fill = "darkolivegreen3", bins = 30)+
  geom_vline(xintercept = 0.9, color = "hotpink") +
  theme_bw()+
  xlim(c(0.5,1))+
  labs(title = "Wood")

c %>%
  ggplot() +
  geom_histogram(aes(x = Coverage, y = ..density..), color = "white", 
                 fill = "darkolivegreen3", bins = 30)+
  geom_vline(xintercept = 0.9, color = "hotpink") +
  theme_bw()+
  xlim(c(0,1))+
  labs(title = "Roots")

###Michaela start her og slut i bunden:
###################################################################################################
#NLR del 2 - multiplikativt:
#Defining the models and the starting-points we found when obtaining the model:
#Creating function to be minimized
MSE_NLR <- function(par, data){
  with(data, sum((Kgp-par[1]*Sc^par[2])^2))
}


starting_point_leafs <- c(0.61,0.81)
starting_point_wood <- c(5.51,0.73)
starting_point_roots <- c(3.66,0.1)

NLE_leafs <- optim(par = starting_point_leafs, fn = MSE_NLR, data = leafs_train)
NLE_wood <- optim(par = starting_point_wood, fn = MSE_NLR, data = wood_train)
NLE_roots <- optim(par = starting_point_roots, fn = MSE_NLR, data = roots_train)

leafs_model <- function(x){NLE_leafs$par[1]*(x)^(NLE_leafs$par[2])}
wood_model <- function(x){NLE_wood$par[1]*(x)^(NLE_wood$par[2])}
roots_model <- function(x){NLE_roots$par[1]*(x)^(NLE_roots$par[2])}


#Defining NLR-specific bootstrapping function:
set.seed(72)
boot <- function(model, data_train, data_test, B) {
  #Obtaining predicted values
  Y_p <- model$par[1]*(data_test$Sc)^(model$par[2])
  
  rep_e_n1 <- function(nlr, data, X_n_1){
    #Make bootstrapped data
    boot_data <- data[sample(nrow(data), replace = TRUE),]
    
    # Doing NLR on bootstrapped data, and calculating estimates of beta/beta and alpha-alpha
    boot_parameters <- optim(par = starting_point_leafs, fn = MSE_NLR, data = boot_data)
    boot_model <- function(x){boot_parameters$par[1]*(x)^(boot_parameters$par[2])}
    beta <- model$par[1]/boot_parameters$par[1]
    alph <- model$par[2]-boot_parameters$par[2]
    
    # Obtaining residuals
    bs_res <- data$Kgp/boot_model(data$Sc)
    
    # Calculate draw on prediction error
    e_N_1 <- beta * X_n_1^alph * sample(bs_res,size=1)
    print(e_N_1)
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

leafs_boot <- boot(NLE_leafs, leafs_train, test_leafs, 1)
wood_boot <- boot(NLE_wood, wood_train, test_wood, 100)
roots_boot <- boot(NLE_roots, roots_train, test_roots, 100)


#Creating the prediction intervals

pred_int <- function(boot, data, alpha) {
  down <- c()
  up <- c()
  
  for (i in ncol(boot[[1]])){
    down <- boot[[2]] * quantile(boot[[1]][,i], probs = alpha/2)
    up <- boot[[2]] * quantile(boot[[1]][,i], probs = 1-alpha/2)
  } 
  
  plot_data <- tibble("Sc" = data$Sc, "Kgp" = data$Kgp,
                      "Pred" = boot[[2]], "down" = down, "up" = up) %>%
    mutate(Sc = Sc, Kgp = Kgp, down = down, up = up)%>%
    mutate(indicator = if_else((down <= Kgp)&(Kgp <= up),"in", "out"))
  
  return(plot_data)
}

#Plot

pred_int_leafs <- pred_int(leafs_boot, test_leafs, 0.1) 
pred_int_wood <- pred_int(wood_boot, test_wood, 0.1)
pred_int_roots <- pred_int(roots_boot, test_roots, 0.1)

color <- c("in" = "darkolivegreen", "out" = "darkolivegreen3")

ggplot(pred_int_leafs) + 
  geom_point(aes(x = Sc, y = down), color = 'hotpink') +
  geom_point(aes(x = Sc, y = up), color = 'hotpink') +
  geom_function(fun = leafs_model, color = "hotpink4") +
  geom_point(aes(x = Sc, y = Kgp, color = indicator)) +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Leafs")+
  scale_color_manual(values = color)


ggplot(pred_int_wood) + 
  geom_point(aes(x = Sc, y = down), color = 'hotpink') +
  geom_point(aes(x = Sc, y = up), color = 'hotpink') +
  geom_function(fun = wood_model, color = "hotpink4") +
  geom_point(aes(x = Sc, y = Kgp, color = indicator)) +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Wood")+
  scale_color_manual(values = color)

ggplot(pred_int_roots)+ 
  geom_point(aes(x = Sc, y = down), color = 'hotpink') +
  geom_point(aes(x = Sc, y = up), color = 'hotpink') +
  geom_function(fun = roots_model, color = "hotpink4") +
  geom_point(aes(x = Sc, y = Kgp, color = indicator)) +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Roots")+
  scale_color_manual(values = color)

#Checking for correct coverage
leafs <- read.csv("Data/leafs.csv")
wood <- read.csv("Data/wood.csv")
roots <- read.csv("Data/roots.csv")

rs_cov <- function(data, k, alpha, starting_points) {
  cov <- c()
  n <- nrow(data)
  sample_size <- floor(0.8*n)
  
  for (i in (1:k)){
    # Test and train
    picked_rs <- sample(n,size = sample_size)
    train_rs = data[picked_rs,]
    test_rs = data[-picked_rs,]
    
    # Fit NLR-model
    lm_rs <- optim(par = starting_points, fn = MSE_NLR, data = train_rs)
    
    #Perform bootstrap
    boot_rs <- boot(lm_rs, train_rs, test_rs, 100)
    
    # Quantiles
    pred_int_rs <- pred_int(boot_rs, test_rs, alpha) 
    
    #Definere
    cov[i] <- coverage(pred_int_rs)
  }
  return(tibble("Coverage" = cov))
}

#Det her tager ægte lang tid, jeg ved ikke om det går godt...

set.seed(4)
a <- rs_cov(leafs, 30, 0.1, starting_point_leafs)
b <- rs_cov(wood, 30, 0.1, starting_point_wood)
c <- rs_cov(roots, 30,0.1, starting_point_roots)


#Mean coverage:
mean_a <- mean(a$Coverage)
mean_b <- mean(b$Coverage)
mean_c <- mean(c$Coverage) 

xtable(tibble(Data = c("Leafs", "Wood", "Roots"), 
              "Mean coverage" =c(mean_a, mean_b, mean_c)), type = latex)

a %>%
  ggplot() +
  geom_histogram(aes(x = Coverage, y = ..density..), color = "white", 
                 fill = "darkolivegreen3", bins = 30)+
  geom_vline(xintercept = 0.9, color = "hotpink") +
  xlim(0,1)+
  theme_bw()+
  labs(title = "Foliage")

b %>%
  ggplot() +
  geom_histogram(aes(x = Coverage, y = ..density..), color = "white", 
                 fill = "darkolivegreen3", bins = 30)+
  geom_vline(xintercept = 0.9, color = "hotpink") +
  theme_bw()+
  xlim(c(0.5,1))+
  labs(title = "Wood")

c %>%
  ggplot() +
  geom_histogram(aes(x = Coverage, y = ..density..), color = "white", 
                 fill = "darkolivegreen3", bins = 30)+
  geom_vline(xintercept = 0.9, color = "hotpink") +
  theme_bw()+
  xlim(c(0,1.1))+
  labs(title = "Roots")

###################################################################################################
#NLR del 3 - multiplikativt med residual-sampling:
#Defining the models and the starting-points we found when obtaining the model:
#Creating function to be minimized
MSE_NLR <- function(par, data){
  with(data, sum((Kgp-par[1]*Sc^par[2])^2))
}


starting_point_leafs <- c(0.61,0.81)
starting_point_wood <- c(5.51,0.73)
starting_point_roots <- c(3.66,0.1)

NLE_leafs <- optim(par = starting_point_leafs, fn = MSE_NLR, data = leafs_train)
NLE_wood <- optim(par = starting_point_wood, fn = MSE_NLR, data = wood_train)
NLE_roots <- optim(par = starting_point_roots, fn = MSE_NLR, data = roots_train)

leafs_model <- function(x){NLE_leafs$par[1]*(x)^(NLE_leafs$par[2])}
wood_model <- function(x){NLE_wood$par[1]*(x)^(NLE_wood$par[2])}
roots_model <- function(x){NLE_roots$par[1]*(x)^(NLE_roots$par[2])}


#Defining NLR-specific bootstrapping function:
set.seed(72)
boot <- function(model, data_train, data_test, B) {{
  #Obtaining predicted values and residuals:
  Y_p <- model$par[1]*(data_test$Sc)^(model$par[2])
  res <- data_train$Kgp/(model$par[1]*(data_train$Sc)^(model$par[2]))}
  
  rep_e_n1 <- function(nlr, data, X_n_1){
    #Make bootstrapped data
    samp_res <- sample(res, nrow(data), replace = TRUE)
    boot_y <- model$par[1]*(data$Sc)^(model$par[2])*samp_res
    boot_data <- data.frame(Sc=data$Sc, Kgp = boot_y)
    
    # Doing NLR on bootstrapped data, and calculating estimates of beta/beta and alpha-alpha
    boot_parameters <- optim(par = starting_point_leafs, fn = MSE_NLR, data = boot_data)
    boot_model <- function(x){boot_parameters$par[1]*(x)^(boot_parameters$par[2])}
    beta <- model$par[1]/boot_parameters$par[1]
    alph <- model$par[2]-boot_parameters$par[2]
    
    # Obtaining residuals
    bs_res <- data$Kgp/boot_model(data$Sc)
    
    # Calculate draw on prediction error
    e_N_1 <- beta * X_n_1^alph * sample(bs_res,size=1)
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

leafs_boot <- boot(NLE_leafs, leafs_train, test_leafs, 10)
wood_boot <- boot(NLE_wood, wood_train, test_wood, 100)
roots_boot <- boot(NLE_roots, roots_train, test_roots, 100)


#Creating the prediction intervals

pred_int <- function(boot, data, alpha) {
  down <- c()
  up <- c()
  
  for (i in ncol(boot[[1]])){
    down <- boot[[2]] * quantile(boot[[1]][,i], probs = alpha/2)
    up <- boot[[2]] * quantile(boot[[1]][,i], probs = 1-alpha/2)
  } 
  
  plot_data <- tibble("Sc" = data$Sc, "Kgp" = data$Kgp,
                      "Pred" = boot[[2]], "down" = down, "up" = up) %>%
    mutate(Sc = Sc, Kgp = Kgp, down = down, up = up)%>%
    mutate(indicator = if_else((down <= Kgp)&(Kgp <= up),"in", "out"))
  
  return(plot_data)
}

#Plot

pred_int_leafs <- pred_int(leafs_boot, test_leafs, 0.1) 
pred_int_wood <- pred_int(wood_boot, test_wood, 0.1)
pred_int_roots <- pred_int(roots_boot, test_roots, 0.1)

color <- c("in" = "darkolivegreen", "out" = "darkolivegreen3")

ggplot(pred_int_leafs) + 
  geom_point(aes(x = Sc, y = down), color = 'hotpink') +
  geom_point(aes(x = Sc, y = up), color = 'hotpink') +
  geom_function(fun = leafs_model, color = "hotpink4") +
  geom_point(aes(x = Sc, y = Kgp, color = indicator)) +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Leafs")+
  scale_color_manual(values = color)


ggplot(pred_int_wood) + 
  geom_point(aes(x = Sc, y = down), color = 'hotpink') +
  geom_point(aes(x = Sc, y = up), color = 'hotpink') +
  geom_function(fun = wood_model, color = "hotpink4") +
  geom_point(aes(x = Sc, y = Kgp, color = indicator)) +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Wood")+
  scale_color_manual(values = color)

ggplot(pred_int_roots)+ 
  geom_point(aes(x = Sc, y = down), color = 'hotpink') +
  geom_point(aes(x = Sc, y = up), color = 'hotpink') +
  geom_function(fun = roots_model, color = "hotpink4") +
  geom_point(aes(x = Sc, y = Kgp, color = indicator)) +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Roots")+
  scale_color_manual(values = color)

#Checking for correct coverage
leafs <- read.csv("Data/leafs.csv")
wood <- read.csv("Data/wood.csv")
roots <- read.csv("Data/roots.csv")

rs_cov <- function(data, k, alpha, starting_points) {
  cov <- c()
  n <- nrow(data)
  sample_size <- floor(0.8*n)
  
  for (i in (1:k)){
    # Test and train
    picked_rs <- sample(n,size = sample_size)
    train_rs = data[picked_rs,]
    test_rs = data[-picked_rs,]
    
    # Fit NLR-model
    lm_rs <- optim(par = starting_points, fn = MSE_NLR, data = train_rs)
    
    #Perform bootstrap
    boot_rs <- boot(lm_rs, train_rs, test_rs, 100)
    
    # Quantiles
    pred_int_rs <- pred_int(boot_rs, test_rs, alpha) 
    
    #Definere
    cov[i] <- coverage(pred_int_rs)
  }
  return(tibble("Coverage" = cov))
}

#Det her tager ægte lang tid, jeg ved ikke om det går godt...

set.seed(4)
a <- rs_cov(leafs, 30, 0.1, starting_point_leafs)
b <- rs_cov(wood, 30, 0.1, starting_point_wood)
c <- rs_cov(roots, 30,0.1, starting_point_roots)


#Mean coverage:
mean_a <- mean(a$Coverage)
mean_b <- mean(b$Coverage)
mean_c <- mean(c$Coverage) 

xtable(tibble(Data = c("Leafs", "Wood", "Roots"), 
              "Mean coverage" =c(mean_a, mean_b, mean_c)), type = latex)

a %>%
  ggplot() +
  geom_histogram(aes(x = Coverage, y = ..density..), color = "white", 
                 fill = "darkolivegreen3", bins = 30)+
  geom_vline(xintercept = 0.9, color = "hotpink") +
  xlim(0,1)+
  theme_bw()+
  labs(title = "Foliage")

b %>%
  ggplot() +
  geom_histogram(aes(x = Coverage, y = ..density..), color = "white", 
                 fill = "darkolivegreen3", bins = 30)+
  geom_vline(xintercept = 0.9, color = "hotpink") +
  theme_bw()+
  xlim(c(0.5,1))+
  labs(title = "Wood")

c %>%
  ggplot() +
  geom_histogram(aes(x = Coverage, y = ..density..), color = "white", 
                 fill = "darkolivegreen3", bins = 30)+
  geom_vline(xintercept = 0.9, color = "hotpink") +
  theme_bw()+
  xlim(c(0,1.1))+
  labs(title = "Roots")

