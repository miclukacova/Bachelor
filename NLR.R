#Packages
library(tidyverse)
library(readr)
library(infer)
library(foreign)
library(xtable)
library(stargazer)


#Getting data
leafs <- read.csv('Data/leafs.csv')
wood <- read.csv("Data/wood.csv")
roots <- read.csv("Data/roots.csv")

leafs_train <- read.csv('Data/train_leafs.csv')
roots_train <- read.csv('Data/train_roots.csv')
wood_train<- read.csv('Data/train_wood.csv')

leafs_test <- read.csv('Data/test_leafs.csv')
roots_test <- read.csv('Data/test_roots.csv')
wood_test <- read.csv('Data/test_wood.csv')

#Creating function to be minimized
MSE_NLR <- function(par, data){
  with(data, sum((Kgp-par[1]*Sc^par[2])^2))
}



#Testing function:
MSE_NLR(c(1,1),leafs)
sum((leafs$Kgp-1*leafs$Sc^1)^2)


#Trying minimization out with initial parameters 1,1

NLE_leafs <- optim(par = c(1,1), fn = MSE_NLR, data = leafs)
NLE_wood <- optim(par = c(1,1), fn = MSE_NLR, data = wood)
NLE_roots <- optim(par = c(1,1), fn = MSE_NLR, data = roots)

#Trying out different methods
optim(par = c(1,1), fn = MSE_NLR, data = leafs, method = "BFGS")$par
optim(par = c(1,1), fn = MSE_NLR, data = wood, method = "BFGS")$par
optim(par = c(1,1), fn = MSE_NLR, data = roots, method = "BFGS")$par

optim(par = c(1,1), fn = MSE_NLR, data = leafs, method = "CG")$par
optim(par = c(1,1), fn = MSE_NLR, data = wood, method = "CG")$par
optim(par = c(1,1), fn = MSE_NLR, data = roots, method = "CG")$par


#Trying different starting points
par_art_l <- c(0.3668,0.9441)
par_art_w <- c(5.6658,1.1068)
par_art_r <- c(1.1921,1.1730)

optim(par = par_art_l, fn = MSE_NLR, data = leafs, method = "BFGS")$par
optim(par = par_art_w, fn = MSE_NLR, data = wood, method = "BFGS")$par
optim(par = par_art_l, fn = MSE_NLR, data = roots, method = "BFGS")$par


#Using test and training to get MSE:
#Estimater

NLE_leafs <- optim(par = c(1,1), fn = MSE_NLR, data = leafs_train)
NLE_wood <- optim(par = c(1,1), fn = MSE_NLR, data = wood_train)
NLE_roots <- optim(par = c(1,1), fn = MSE_NLR, data = roots_train)

hat_beta <- c(NLE_leafs$par[[1]],
              NLE_wood$par[[1]],
              NLE_roots$par[[1]])

hat_alpha <- c(NLE_leafs$par[[2]],
               NLE_wood$par[[2]],
               NLE_roots$par[[2]])

#Ved ikke helt med denne her
#var_hat <- c(var(lm_leafs_log$residuals),
#             var(lm_roots_log$residuals),
#             var(lm_wood_log$residuals))

xtable(tibble("Data" = c("Leafs", "Wood", "Roots"), "alpha" = hat_alpha, 
              "beta" = hat_beta), type = "latex")


#Evaluering af de to modeller---------------------------------------------------


#Y_hat estimater

f_hat_leafs <- function(x) hat_beta[1]*x^hat_alpha[1]
f_hat_wood <- function(x) hat_beta[2]*x^hat_alpha[2]
f_hat_roots <- function(x) hat_beta[3]*x^hat_alpha[3]

#MSE

l1 <- mean((f_hat_leafs(leafs_test$Sc)-leafs_test$Kgp)^2)
w1 <- mean((f_hat_wood(wood_test$Sc)-wood_test$Kgp)^2)
r1 <- mean((f_hat_roots(roots_test$Sc)-roots_test$Kgp)^2)

mse_f <- tibble("Model" = c("Leafs", "Wood" 
                            ,"Roots"),
                "MSE" = c(l1, w1, r1))

xtable(mse_f, type = "latex")

#Bias

l1 <- mean((f_hat_leafs(leafs_test$Sc)-leafs_test$Kgp))
r1 <- mean((f_hat_roots(roots_test$Sc)-roots_test$Kgp))
w1 <- mean((f_hat_wood(wood_test$Sc)-wood_log_test$Kgp))

bias_f <- tibble("Model" = c("Leafs", "Wood",  "Roots"),
                 "Bias" = c(l1, w1, r1))

xtable(bias_f, type = "latex")

#Plot

ggplot(leafs_test, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'darkolivegreen', fill = 'darkolivegreen3', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab(bquote('Crown area'~m^2/plant)) + 
  ylab('Dry mass (kg/plant)')+
  geom_function(fun = f_hat_leafs, colour = "hotpink")+
  labs(title = "Foliage")

ggplot(wood_test, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'darkolivegreen', fill = 'darkolivegreen3', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab(bquote('Crown area'~m^2/plant)) + 
  ylab('Dry mass (kg/plant)')+
  geom_function(fun = f_hat_wood, colour = "hotpink")+
  labs(title = "Wood")

ggplot(roots_test, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'darkolivegreen', fill = 'darkolivegreen3', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab(bquote('Crown area'~m^2/plant)) + 
  ylab('Dry mass (kg/plant)')+
  geom_function(fun = f_hat_roots, colour = "hotpink")+
  labs(title = "Roots")

#På alt data

ggplot(leafs, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'darkolivegreen', fill = 'darkolivegreen3', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab(bquote('Crown area'~m^2/plant)) + 
  ylab('Dry mass (kg/plant)')+
  geom_function(fun = f_hat_leafs, colour = "hotpink")+
  labs(title = "Foliage")

ggplot(wood, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'darkolivegreen', fill = 'darkolivegreen3', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab(bquote('Crown area'~m^2/plant)) + 
  ylab('Dry mass (kg/plant)')+
  geom_function(fun = f_hat_wood, colour = "hotpink")+
  labs(title = "Wood")

ggplot(roots, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'darkolivegreen', fill = 'darkolivegreen3', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab(bquote('Crown area'~m^2/plant)) + 
  ylab('Dry mass (kg/plant)')+
  geom_function(fun = f_hat_roots, colour = "hotpink")+
  labs(title = "Roots")

# k-fold cv MSE and bias

MSE_NLR <- function(par, data){
  with(data, sum((Kgp-par[1]*Sc^par[2])^2))
}


#Using test and training to get MSE:
#Estimater



hat_beta <- c(NLE_leafs$par[[1]],
              NLE_wood$par[[1]],
              NLE_roots$par[[1]])

hat_alpha <- c(NLE_leafs$par[[2]],
               NLE_wood$par[[2]],
               NLE_roots$par[[2]])


cv <- function(data, k) {
  MSE <- c()
  Bias <- c()
  n <- nrow(data)
  group <- sample(rep(1:k, length.out = n))
  for (i in (1:k)){
    #Fit model
    NLE_cv <- optim(par = c(1,1), fn = MSE_NLR, data = data)
    f_model <- function(x) NLE_cv$par[[1]]*x^NLE_cv$par[[2]]
    
    #MSE
    MSE[i] <- mean((f_model(data$Sc)-data$Kgp)^2)
    Bias[i] <-  mean((f_model(data$Sc)-data$Kgp))
  }
  return(tibble("MSE" = MSE, "Bias" = Bias))
}

set.seed(1)
a <- cv(leafs, nrow(leafs_log))
b <- cv(wood, nrow(wood_log))
c <- cv(roots, nrow(roots_log))

cv_mse <- tibble("Model" = c("Leafs", "Wood", "Roots"),
                 "Mean of CV-MSE" = c(mean(a$MSE), mean(b$MSE), mean(c$MSE)),
                 "Mean of CV-Bias" = c(mean(a$Bias), mean(b$Bias), mean(c$Bias)))

xtable(cv_mse, type = latex)
