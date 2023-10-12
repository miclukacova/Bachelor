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

#Grid search

#starting values

lm_leafs_log <- lm(Kgp ~ Sc, data = leafs_log)
lm_wood_log <- lm(Kgp ~ Sc, data = wood_log)
lm_roots_log <- lm(Kgp ~ Sc, data = roots_log)

par_leafs <- c(exp(lm_leafs_log$coefficients[[1]]), lm_leafs_log$coefficients[[2]])
par_wood <- c(exp(lm_wood_log$coefficients[[1]]), lm_wood_log$coefficients[[2]])
par_roots <- c(exp(lm_roots_log$coefficients[[1]]), lm_roots_log$coefficients[[2]])

#Funktion

grid_search <- function(data, par_ols) {
  hej <- TRUE
  for (i in seq(0, par_ols[1]+4, length.out = 50)){
    if (hej){
      grid <- tibble(b = rep(i,50), a = seq(0, par_ols[2]+4, length.out = 50))
      hej <- FALSE
    }
    else {
      for (j in seq(0, par_ols[2]+4, length.out = 50)){
        grid <- add_row(grid, b = i, a = j)
      }
    }
  }
  hej <- TRUE
  for (i in (1:nrow(grid))){
    c <- c(grid[i,1], grid[i,2])
    if (hej){
      NLR_loop <- optim(par = c, fn = MSE_NLR, data = data)
      par_grid <- data.frame(b = NLR_loop$par[[1]],
                             a = NLR_loop$par[[2]],
                             MSE = NLR_loop$value)
      hej <- FALSE
    }
    else{
      NLR_loop <- optim(par = c, fn = MSE_NLR, data = data)
      par_grid <- par_grid %>% add_row(b = NLR_loop$par[[1]],
                                       a = NLR_loop$par[[2]],
                                       MSE = NLR_loop$value)
    }
  }
  return(list(grid, par_grid))
}

grid_search_leafs <- grid_search(leafs, par_leafs)
grid_search_wood <- grid_search(wood, par_wood)
grid_search_roots <- grid_search(roots, par_leafs)

# Grid plot

ggplot(grid_search_leafs[[1]], aes(x = a, y = b)) +
  geom_point(size = 0.9)+
  geom_point(aes(x=par_leafs[2], y = par_leafs[1]), color = "red", size = 4)+
  geom_text(aes(x=par_leafs[2], y = par_leafs[1]), label = "OLS", hjust = -.5, color = "red", size = 5)+
  theme_bw()

# Mindste MSE

min_mse_leafs <- which.min(grid_search_leafs[[2]]$MSE)
starting_point_leafs <- grid_search_leafs[[1]][min_mse_leafs,]

min_mse_wood <- which.min(grid_search_wood[[2]]$MSE)
starting_point_wood <- grid_search_wood[[1]][min_mse_wood,]

min_mse_roots <- which.min(grid_search_roots[[2]]$MSE)
starting_point_roots <- grid_search_roots[[1]][min_mse_roots,]

# a plottet mod b

ggplot(grid_search_leafs[[2]], aes(x = a, y = b)) +
  geom_point()+
  geom_point(aes(x=par_leafs[2], y = par_leafs[1]), color = "red", size = 3)+
  geom_point(aes(x=par_grid[min_mse,1], y = par_grid[min_mse,2]), color = "blue", size = 3)+
  geom_text(aes(x=par_leafs[2], y = par_leafs[1]), label = "OLS", hjust = -.5, color = "red", size = 5)+
  geom_text(aes(x=par_grid[min_mse,1], y = par_grid[min_mse,2]), label = "Min. MSE", hjust = -.5, color = "blue", size = 5)+
  theme_bw()

# a plottet mod MSE

ggplot(grid_search_leafs[[2]], aes(x = a, y = MSE)) +
  geom_point()+
  geom_point(aes(x=par_leafs[2], y = MSE_NLR(par_leafs, leafs)), color = "red", size = 3)+
  geom_point(aes(x=par_grid[min_mse,1], y = par_grid[min_mse,3]), color = "blue", size = 3)+
  geom_text(aes(x=par_leafs[2], y = par_leafs[1]), label = "OLS", vjust = -2, color = "red", size = 5)+
  geom_text(aes(x=par_grid[min_mse,1], y = par_grid[min_mse,2]), label = "Min. MSE", color = "blue", size = 5)+
  theme_bw()

# b plottet mod MSE

ggplot(grid_search_leafs[[2]], aes(x = b, y = MSE)) +
  geom_point()+
  geom_point(aes(x=par_leafs[1], y = MSE_NLR(par_leafs, leafs)), color = "red", size = 3)+
  geom_point(aes(x=par_grid[min_mse,2], y = par_grid[min_mse,3]), color = "blue", size = 3)+
  geom_text(aes(x=par_leafs[2], y = par_leafs[1]), label = "OLS", hjust = 2, color = "red", size = 5)+
  geom_text(aes(x=par_grid[min_mse,1], y = par_grid[min_mse,2]), hjust = -0.05, label = "Min. MSE", color = "blue", size = 5)+
  theme_bw()


#Estimater

NLE_leafs <- optim(par = starting_point_leafs, fn = MSE_NLR, data = leafs)
NLE_wood <- optim(par = starting_point_wood, fn = MSE_NLR, data = wood)
NLE_roots <- optim(par = starting_point_roots, fn = MSE_NLR, data = roots)

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
