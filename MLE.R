library(tidyverse)
library(readr)
library(infer)
library(foreign)
library(xtable)
library(stargazer)

leafs_train <- read.csv('Data/train_leafs.csv')
wood_train <- read.csv('Data/train_wood.csv')
roots_train <- read.csv('Data/train_roots.csv')

logxi_l <- log(leafs_train$Sc)
logyi_l <- log(leafs_train$Kgp)
logxi_w <- log(wood_train$Sc)
logyi_w <- log(wood_train$Kgp)
logxi_r <- log(roots_train$Sc)
logyi_r <- log(roots_train$Kgp)
n_l <- nrow(leafs_train)
n_w <- nrow(wood_train)
n_r <- nrow(roots_train)

param_MLE <- function(logxi, logyi, n){
  alpha1 <- sum(logxi*logyi-1/n*sum(logyi)*logxi)/sum(logxi^2-1/(n)*sum(logxi)*logxi)
  log_beta <- exp(1/n * sum(logyi-alpha1*logxi))
  return("param_MLE" = c(alpha1, exp(log_beta)))
}

param_l <- param_MLE(logxi_l, logyi_l, n_l)
param_w <- param_MLE(logxi_w, logyi_w, n_w)
param_r <- param_MLE(logxi_r, logyi_r, n_r)

f_hat_l <- function(x) {
  param_l[2]*x^(param_l[1])
  }
f_hat_w <- function(x) {
  param_w[2]*x^(param_w[1])
}
f_hat_r <- function(x) {
  param_r[2]*x^(param_r[1])
  }


ggplot(leafs_train, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'pink', fill = 'hotpink', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab(bquote('Crown area'~m^2/plant)) + 
  ylab('log(Dry mass (kg/plant))')+
  geom_function(fun = f_hat_l, colour = "darkolivegreen")+
  labs(title = "Foliage")

ggplot(wood_train, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'pink', fill = 'hotpink', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab(bquote('Crown area'~m^2/plant)) + 
  ylab('log(Dry mass (kg/plant))')+
  geom_function(fun = f_hat_w, colour = "darkolivegreen")+
  labs(title = "Wood")

 ggplot(roots_train, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'pink', fill = 'hotpink', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab(bquote('Crown area'~m^2/plant)) + 
  ylab('log(Dry mass (kg/plant))')+
  geom_function(fun = f_hat_r, colour = "darkolivegreen")+
  labs(title = "Roots")

#For all data-points:
 leafs_train <- read.csv('Data/leafs.csv')
 wood_train <- read.csv('Data/wood.csv')
 roots_train <- read.csv('Data/roots.csv')
 
 logxi_l <- log(leafs_train$Sc)
 logyi_l <- log(leafs_train$Kgp)
 logxi_w <- log(wood_train$Sc)
 logyi_w <- log(wood_train$Kgp)
 logxi_r <- log(roots_train$Sc)
 logyi_r <- log(roots_train$Kgp)
 n_l <- nrow(leafs_train)
 n_w <- nrow(wood_train)
 n_r <- nrow(roots_train)
 
 param_MLE <- function(logxi, logyi, n){
   alpha1 <- sum(logxi*logyi-1/n*sum(logyi)*logxi)/sum(logxi^2-1/(n)*sum(logxi)*logxi)
   log_beta <- exp(1/n * sum(logyi-alpha1*logxi))
   return("param_MLE" = c(alpha1, exp(log_beta)))
 }
 
 param_l <- param_MLE(logxi_l, logyi_l, n_l)
 param_w <- param_MLE(logxi_w, logyi_w, n_w)
 param_r <- param_MLE(logxi_r, logyi_r, n_r)
 
 f_hat_l <- function(x) {
   param_l[2]*x^(param_l[1])
 }
 f_hat_w <- function(x) {
   param_w[2]*x^(param_w[1])
 }
 f_hat_r <- function(x) {
   param_r[2]*x^(param_r[1])
 }
 
 
 ggplot(leafs_train, aes(x = Sc, y = Kgp)) + 
   geom_point(color = 'pink', fill = 'hotpink', alpha = 0.6, shape = 21) + 
   theme_bw() +
   xlab(bquote('Crown area'~m^2/plant)) + 
   ylab('log(Dry mass (kg/plant))')+
   geom_function(fun = f_hat_l, colour = "darkolivegreen")+
   labs(title = "Foliage")
 
 ggplot(wood_train, aes(x = Sc, y = Kgp)) + 
   geom_point(color = 'pink', fill = 'hotpink', alpha = 0.6, shape = 21) + 
   theme_bw() +
   xlab(bquote('Crown area'~m^2/plant)) + 
   ylab('log(Dry mass (kg/plant))')+
   geom_function(fun = f_hat_w, colour = "darkolivegreen")+
   labs(title = "Wood")
 
 ggplot(roots_train, aes(x = Sc, y = Kgp)) + 
   geom_point(color = 'pink', fill = 'hotpink', alpha = 0.6, shape = 21) + 
   theme_bw() +
   xlab(bquote('Crown area'~m^2/plant)) + 
   ylab('log(Dry mass (kg/plant))')+
   geom_function(fun = f_hat_r, colour = "darkolivegreen")+
   labs(title = "Roots")

parameters <- data.frame("alpha" = c(param_l[1],param_w[1],param_r[1])
                         , "beta"=c(param_l[2],param_w[2],param_r[2]))
 
xtable(data.frame("Data" = c("Leafs", "Wood", "Roots"), "alpha" = c(param_l[1],param_w[1],param_r[1])
                  , "beta"=c(param_l[2],param_w[2],param_r[2])), type = "latex") 
 