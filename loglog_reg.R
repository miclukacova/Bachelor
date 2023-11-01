#################################--Indlæsning af pakker og data---###############################

leafs_log_train <- read.csv('Data/train_leafs_log.csv')
roots_log_train <- read.csv('Data/train_roots_log.csv')
wood_log_train<- read.csv('Data/train_wood_log.csv')

leafs_log_test <- read.csv('Data/test_leafs_log.csv')
roots_log_test <- read.csv('Data/test_roots_log.csv')
wood_log_test<- read.csv('Data/test_wood_log.csv')

leafs_log <- read.csv('Data/leafs_log.csv')
roots_log <- read.csv('Data/roots_log.csv')
wood_log<- read.csv('Data/wood_log.csv')

leafs_train <- read.csv('Data/train_leafs_log.csv')
roots_train <- read.csv('Data/train_roots_log.csv')
wood_train<- read.csv('Data/train_wood_log.csv')

leafs_test <- read.csv('Data/test_leafs.csv')
roots_test <- read.csv('Data/test_roots.csv')
wood_test <- read.csv('Data/test_wood.csv')

leafs <- read.csv('Data/leafs.csv')
roots <- read.csv('Data/roots.csv')
wood <- read.csv('Data/wood.csv')

library(tidyverse)
library(readr)
library(infer)
library(foreign)
library(xtable)
library(stargazer)
###################################################################################################

#Lineære modeller af log-log

lm_leafs_log <- lm(Kgp ~ Sc, data = leafs_log)
lm_wood_log <- lm(Kgp ~ Sc, data = wood_log)
lm_roots_log <- lm(Kgp ~ Sc, data = roots_log)


#Estimater

hat_beta <- c(lm_leafs_log$coefficients[[1]],
          lm_wood_log$coefficients[[1]],
           lm_roots_log$coefficients[[1]])

hat_alpha <- c(lm_leafs_log$coefficients[[2]],
          lm_wood_log$coefficients[[2]],
          lm_roots_log$coefficients[[2]])

var_hat <- c(var(lm_leafs_log$residuals),
             var(lm_wood_log$residuals),
             var(lm_roots_log$residuals))

xtable(tibble("Data" = c("Leafs", "Wood", "Roots"), "alpha" = hat_alpha, "var" = var_hat, "log(beta)" = hat_beta,
       "beta" =exp(hat_beta)), type = "latex")


#Y_hat estimater uden bias correction:

f_hat_leafs <- function(x) exp(hat_beta[1] + hat_alpha[1]*x)
f_hat_wood <- function(x) exp(hat_beta[2] + hat_alpha[2]*x)
f_hat_roots <- function(x) exp(hat_beta[3] + hat_alpha[3]*x)

#Y_hat estimater med bias correction: 
f_hat_leafs_adj <- function(x) exp(hat_beta[1] + hat_alpha[1]*x)*exp(var_hat[1]/2)
f_hat_wood_adj <- function(x) exp(hat_beta[2] + hat_alpha[2]*x)*exp(var_hat[2]/2)
f_hat_roots_adj <- function(x) exp(hat_beta[3] + hat_alpha[3]*x)*exp(var_hat[3]/2)

#Evaluering af de to modeller---------------------------------------------------

#MSE

l1 <- mean((f_hat_leafs(leafs_log$Sc)-exp(leafs_log$Kgp))^2)
l2 <- mean((f_hat_leafs_adj(leafs_log$Sc)-exp(leafs_log$Kgp))^2)

w1 <- mean((f_hat_wood(wood_log$Sc)-exp(wood_log$Kgp))^2)
w2 <- mean((f_hat_wood_adj(wood_log$Sc)-exp(wood_log$Kgp))^2)

r1 <- mean((f_hat_roots(roots_log$Sc)-exp(roots_log$Kgp))^2)
r2 <- mean((f_hat_roots_adj(roots_log$Sc)-exp(roots_log$Kgp))^2)

#Bias

l12 <- mean((f_hat_leafs(leafs_log$Sc)-exp(leafs_log$Kgp)))
l22 <- mean((f_hat_leafs_adj(leafs_log$Sc)-exp(leafs_log$Kgp)))

r12 <- mean((f_hat_roots(roots_log$Sc)-exp(roots_log$Kgp)))
r22 <- mean((f_hat_roots_adj(roots_log$Sc)-exp(roots_log$Kgp)))

w12 <- mean((f_hat_wood(wood_log$Sc)-exp(wood_log$Kgp)))
w22 <- mean((f_hat_wood_adj(wood_log$Sc)-exp(wood_log$Kgp)))

mse_bias_f <- tibble("Model" = c("Leafs", "Leafs bias adj.", "Wood", 
                            "Wood bias adj.", "Roots", "Roots bias adj."),
                "MSE" = c(l1, l2, w1, w2, r1, r2),
                "Bias" = c(l12, l22, w12, w22, r12, r22))

xtable(mse_bias_f, type = "latex")

#Y_hat estimater med bias correction: 
f_hat_leafs_log <- function(x) hat_beta[1] + x*hat_alpha[1]
f_hat_wood_log <- function(x) hat_beta[2] + x*hat_alpha[2]
f_hat_roots_log <- function(x) hat_beta[3] + x*hat_alpha[3]

#Plot på alt data log scale

cols <- c("darkolivegreen2","darkolivegreen")

ggplot(leafs_log, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'pink', fill = 'hotpink', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab(bquote('Crown area'~m^2/plant)) + 
  geom_function(fun = f_hat_leafs_log, color = "darkolivegreen")+
  ylab('Dry mass (kg/plant)')+
  labs(title = "Foliage")

ggplot(wood_log, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'pink', fill = 'hotpink', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab(bquote('Crown area'~m^2/plant)) + 
  ylab('log(Dry mass (kg/plant))')+
  geom_function(fun = f_hat_wood_log, color = "darkolivegreen")+
  labs(title = "Wood")

ggplot(roots_log, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'pink', fill = 'hotpink', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab(bquote(log('Crown area'~m^2/plant))) + 
  ylab('Dry mass (kg/plant)')+
  geom_function(fun = f_hat_roots_log, color = "darkolivegreen")+
  labs(title = "Roots")

#Plot på alt data real scale

cols <- c("darkolivegreen2","darkolivegreen")

ggplot(leafs, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'pink', fill = 'hotpink', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab(bquote('Crown area'~m^2/plant)) + 
  geom_function(fun = f_hat_leafs_real, aes(col = "f"))+
  geom_function(fun = f_hat_leafs_adj_real, aes(col = "f_adj"))+
  ylab('Dry mass (kg/plant)')+
  labs(title = "Foliage")+
  scale_colour_manual(values = cols)

ggplot(wood, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'pink', fill = 'hotpink', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab(bquote('Crown area'~m^2/plant)) + 
  ylab('log(Dry mass (kg/plant))')+
  geom_function(fun = f_hat_wood_real, aes(col = "f"))+
  geom_function(fun = f_hat_wood_adj_real, aes(col = "f_adj"))+
  labs(title = "Wood")+
  scale_colour_manual(values = cols)

ggplot(roots, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'pink', fill = 'hotpink', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab(bquote(log('Crown area'~m^2/plant))) + 
  ylab('Dry mass (kg/plant)')+
  geom_function(fun = f_hat_roots_real, aes(col = "f"))+
  geom_function(fun = f_hat_roots_adj_real, aes(col = "f_adj"))+
  labs(title = "Roots")+
  scale_colour_manual(values = cols)

# k-fold cv vurdering af de to modeller

cv <- function(data, k) {
  MSE <- c()
  MSE_adj <- c() 
  n <- nrow(data)
  group <- sample(rep(1:k, length.out = n))
  for (i in (1:k)){
    #Fit model
    lm_cv <- lm(Kgp ~ Sc, data = data[group != i, ])
    
    #MSE
    MSE[i] <- mean((exp(predict(lm_cv, newdata = data[group == i, ]))-exp(data[group == i, ]$Kgp))^2)
    MSE_adj[i] <- mean((exp(predict(lm_cv, newdata = data[group == i, ]))*
                         exp(var(lm_cv$residuals)/2)-exp(data[group == i, ]$Kgp))^2)
  }
  return(tibble("MSE" = MSE, "MSE Bias Corrected" = MSE_adj))
}

set.seed(1)
a <- cv(leafs_log, nrow(leafs_log))
b <- cv(wood_log, nrow(wood_log))
c <- cv(roots_log, nrow(roots_log))

cv_mse <- tibble("Model" = c("Leafs", "Leafs bias adj.", "Wood", "Wood bias adj.", "Roots", "Roots bias adj."),
       "Mean of CV-MSE" = c(mean(a$MSE), mean(a$`MSE Bias Corrected`), mean(b$MSE), mean(b$`MSE Bias Corrected`),
                            mean(c$MSE), mean(c$`MSE Bias Corrected`)))

xtable(cv_mse, type = latex)

#Bias

cv_bias <- function(data, k) {
  bias <- c()
  bias_adj <- c() 
  n <- nrow(data)
  group <- sample(rep(1:k, length.out = n))
  for (i in (1:k)){
    #Fit model
    lm_cv <- lm(Kgp ~ Sc, data = data[group != i, ])
    
    #Definere
    bias[i] <- mean((exp(predict(lm_cv, newdata = data[group == i, ]))-exp(data[group == i, ]$Kgp)))
    bias_adj[i] <- mean((exp(predict(lm_cv, newdata = data[group == i, ]))*
                          exp(var(lm_cv$residuals)/2)-exp(data[group == i, ]$Kgp)))
  }
  return(tibble("Bias" = bias, "Bias Bias Corrected" = bias_adj))
}

set.seed(1)
a <- cv_bias(leafs_log, nrow(leafs_log))
b <- cv_bias(wood_log, nrow(wood_log))
c <- cv_bias(roots_log, nrow(roots_log))

cv_bias <- tibble("Model" = c("Leafs", "Leafs bias adj.", "Wood", "Wood bias adj.", "Roots", "Roots bias adj."),
                 "Mean of CV-Bias" = c(mean(a$Bias), mean(a$`Bias Bias Corrected`), mean(b$Bias), mean(b$`Bias Bias Corrected`),
                                      mean(c$Bias), mean(c$`Bias Bias Corrected`)))

xtable(cv_bias, type = latex)


################################################################################
#-------------------------------- OLS på data ---------------------------------#
################################################################################

#Lineære modeller af log-log

lm_leafs <- lm(Kgp ~ Sc, data = leafs)
lm_wood <- lm(Kgp ~ Sc, data = wood)
lm_roots <- lm(Kgp ~ Sc, data = roots)

#Estimater

hat_beta <- c(lm_leafs$coefficients[[1]],
              lm_wood$coefficients[[1]],
              lm_roots$coefficients[[1]])

hat_alpha <- c(lm_leafs$coefficients[[2]],
               lm_wood$coefficients[[2]],
               lm_roots$coefficients[[2]])

var_hat <- c(var(lm_leafs$residuals),
             var(lm_wood$residuals),
             var(lm_roots$residuals))

xtable(tibble("Data" = c("Leafs", "Wood", "Roots"), "alpha" = hat_alpha, "var" = var_hat, "log(beta)" = hat_beta,
              "beta" =exp(hat_beta)), type = "latex")


#Y_hat estimater:

f_hat_leafs <- function(x) hat_beta[1] + hat_alpha[1]*x
f_hat_wood <- function(x) hat_beta[2] + hat_alpha[2]*x
f_hat_roots <- function(x) hat_beta[3] + hat_alpha[3]*x

#Evaluering af de to modeller---------------------------------------------------

#MSE

l1 <- mean((f_hat_leafs(leafs$Sc)-leafs$Kgp)^2)
w1 <- mean((f_hat_wood(wood$Sc)-wood$Kgp)^2)
r1 <- mean((f_hat_roots(roots$Sc)-roots$Kgp)^2)

#Bias


l12 <- mean((f_hat_leafs(leafs_log_test$Sc)-leafs_log_test$Kgp))
r12 <- mean((f_hat_roots(roots_log_test$Sc)-roots_log_test$Kgp))
w12 <- mean((f_hat_wood(wood_log_test$Sc)-wood_log_test$Kgp))

mse_bias_f <- tibble("Model" = c("Leafs", "Wood", "Roots"),
                 "MSE" = c(l1, w1, r1),
                "Bias" = c(l12, w12, r12))

xtable(mse_bias_f, type = "latex")

#Plot på alt data

ggplot(leafs, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'pink', fill = 'hotpink', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab(bquote('Crown area'~m^2/plant)) + 
  geom_function(fun = f_hat_leafs, color = "darkolivegreen")+
  ylab('Dry mass (kg/plant)')+
  labs(title = "Foliage")

ggplot(wood, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'pink', fill = 'hotpink', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab(bquote('Crown area'~m^2/plant)) + 
  ylab('log(Dry mass (kg/plant))')+
  geom_function(fun = f_hat_wood, , color = "darkolivegreen")+
  labs(title = "Wood")

ggplot(roots, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'pink', fill = 'hotpink', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab(bquote(log('Crown area'~m^2/plant))) + 
  ylab('Dry mass (kg/plant)')+
  geom_function(fun = f_hat_roots, , color = "darkolivegreen")+
  labs(title = "Roots")

# k-fold cv vurdering af de to modeller

cv <- function(data, k) {
  MSE <- c()
  bias <- c() 
  n <- nrow(data)
  group <- sample(rep(1:k, length.out = n))
  for (i in (1:k)){
    #Fit model
    lm_cv <- lm(Kgp ~ Sc, data = data[group != i, ])
    
    #MSE
    MSE[i] <- mean((predict(lm_cv, newdata = data[group == i, ])-data[group == i, ]$Kgp)^2)
    
    #Bias
    bias[i] <- mean(predict(lm_cv, newdata = data[group == i, ])-data[group == i, ]$Kgp)
  }
  return(tibble("MSE" = MSE, "Bias" = bias))
}

set.seed(1)
a <- cv(leafs, nrow(leafs))
b <- cv(wood, nrow(wood))
c <- cv(roots, nrow(roots))

cv_mse_bias <- tibble("Model" = c("Leafs", "Wood", "Roots"),
                 "Mean of LOOCV-MSE" = c(mean(a$MSE), mean(b$MSE), mean(c$MSE)),
                 "Mean of LOOCV-Bias" = c(mean(a$Bias), mean(b$Bias), mean(c$Bias)))

xtable(cv_mse_bias, type = latex)


