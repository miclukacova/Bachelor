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
leafs_log <- read.csv('Data/leafs_log.csv')
wood_log <- read.csv("Data/wood_log.csv")
roots_log <- read.csv("Data/roots_log.csv")

#Creating function to be minimized
MSE_NLR <- function(par, data){
  with(data, sum((Kgp-par[1]*Sc^par[2])^2))
}

#Testing function:
MSE_NLR(c(1,1),leafs)
sum((leafs$Kgp-1*leafs$Sc^1)^2)

#Trying out different methods
optim(par = c(1,1), fn = MSE_NLR, data = leafs, method = "BFGS")$par
optim(par = c(1,1), fn = MSE_NLR, data = wood, method = "BFGS")$par
optim(par = c(1,1), fn = MSE_NLR, data = roots, method = "BFGS")$par

optim(par = c(1,1), fn = MSE_NLR, data = leafs, method = "CG")$par
optim(par = c(1,1), fn = MSE_NLR, data = wood, method = "CG")$par
optim(par = c(1,1), fn = MSE_NLR, data = roots, method = "CG")$par


#Grid search

#starting values

lm_leafs_log <- lm(Kgp ~ Sc, data = leafs_log)
lm_wood_log <- lm(Kgp ~ Sc, data = wood_log)
lm_roots_log <- lm(Kgp ~ Sc, data = roots_log)

par_leafs <- c(exp(lm_leafs_log$coefficients[[1]]), lm_leafs_log$coefficients[[2]])
par_wood <- c(exp(lm_wood_log$coefficients[[1]]), lm_wood_log$coefficients[[2]])
par_roots <- c(exp(lm_roots_log$coefficients[[1]]), lm_roots_log$coefficients[[2]])

#Grids
hej <- TRUE

for (i in seq(par_leafs[1]-3, par_leafs[1]+3, length.out = 50)){
  if (hej){
    grid_leafs <- tibble(b = rep(i,50), a = seq(0, par_leafs[2]+3, length.out = 50))
    hej <- FALSE
  }
  else {
    for (j in seq(0, par_leafs[2]+3, length.out = 50)){
      grid_leafs <- add_row(grid_leafs, b = i, a = j)
    }
  }
}

ggplot(grid_leafs, aes(x = a, y = b)) +
  geom_point()+
  geom_point(aes(x=hat_alpha[1], y = hat_beta[1]), color = "red")+
  geom_point(aes(x=par_leafs[2], y = par_leafs[1]), color = "blue")+
  theme_bw()

#Parameters along grid

hej <- TRUE
for (i in (1:nrow(grid_leafs))){
  c <- c(grid_leafs[i,1], grid_leafs[i,2])
  if (hej){
    par_grid <- data.frame(b = optim(par = c, fn = MSE_NLR, data = leafs)$par[[1]],
                           a = optim(par = c, fn = MSE_NLR, data = leafs)$par[[2]])
    hej <- FALSE
    }
  else{
    par_grid <- par_grid %>% add_row(b = optim(par = c, fn = MSE_NLR, data = leafs)$par[[1]],
                                     a = optim(par = c, fn = MSE_NLR, data = leafs)$par[[2]])
    }
}


ggplot(par_grid, aes(x = a, y = b)) +
  geom_point()+
  geom_point(aes(x=hat_alpha[1], y = hat_beta[1]), color = "red")+
  geom_point(aes(x=par_leafs[2], y = par_leafs[1]), color = "blue")+
  theme_bw()

#Estimater

NLE_leafs <- optim(par = c(1,1), fn = MSE_NLR, data = leafs)
NLE_wood <- optim(par = c(1,1), fn = MSE_NLR, data = wood)
NLE_roots <- optim(par = c(1,1), fn = MSE_NLR, data = roots)

hat_beta <- c(NLE_leafs$par[[1]],
              NLE_wood$par[[1]],
              NLE_roots$par[[1]])

hat_alpha <- c(NLE_leafs$par[[2]],
               NLE_wood$par[[2]],
               NLE_roots$par[[2]])

xtable(tibble("Data" = c("Leafs", "Wood", "Roots"), "alpha" = hat_alpha, 
              "beta" = hat_beta), type = "latex")


#Evaluering af de to modeller---------------------------------------------------


#Y_hat estimater

f_hat_leafs <- function(x) hat_beta[1]*x^hat_alpha[1]
f_hat_wood <- function(x) hat_beta[2]*x^hat_alpha[2]
f_hat_roots <- function(x) hat_beta[3]*x^hat_alpha[3]

#Plot

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
