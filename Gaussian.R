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

#Lineære modeller af log-log

lm_leafs_log <- lm(Kgp ~ Sc, data = train_leafs_log)
lm_roots_log <- lm(Kgp ~ Sc, data = train_roots_log)
lm_wood_log <- lm(Kgp ~ Sc, data = train_wood_log)

#QQ-plot

residuals_leafs <- data.frame(residual = lm_leafs_log$residuals/sd(lm_leafs_log$residuals))
residuals_wood <- data.frame(residual = lm_wood_log$residuals/sd(lm_wood_log$residuals))
residuals_roots <- data.frame(residual = lm_roots_log$residuals/sd(lm_roots_log$residuals))

residuals_leafs %>%
  ggplot() +
  geom_histogram(aes(x = residual, y = ..density..), color = "white", fill = "darkolivegreen3") +
  geom_vline(xintercept = 0, color = "hotpink") +
  stat_function(fun = dnorm, color = "darkolivegreen")+
  theme_bw()+
  labs(title = "Foliage")

residuals_wood %>%
  ggplot() +
  geom_histogram(aes(x = residual, y = ..density..), color = "white", fill = "darkolivegreen3") +
  geom_vline(xintercept = 0, color = "hotpink") +
  theme_bw()+
  stat_function(fun = dnorm, color = "darkolivegreen")+
  labs(title = "Wood")

residuals_roots %>%
  ggplot() +
  geom_histogram(aes(x = residual, y = ..density..), color = "white", fill = "darkolivegreen3") +
  geom_vline(xintercept = 0, color = "hotpink") +
  stat_function(fun = dnorm, color = "darkolivegreen")+
  theme_bw()+
  labs(title = "Roots")

ggplot(data = residuals_leafs, aes(sample = residual)) +
           stat_qq() + stat_qq_line(color = "hotpink") + 
  geom_abline(intercept = 0, slope = 1, color = "darkolivegreen")+
  theme_bw()+
  labs(title = "Foliage")

ggplot(data = residuals_wood, aes(sample = residual)) +
  stat_qq() + stat_qq_line(color = "hotpink") + 
  geom_abline(intercept = 0, slope = 1, color = "darkolivegreen")+
  theme_bw()+
  labs(title = "Wood")

ggplot(data = residuals_roots, aes(sample = residual)) +
  stat_qq() + stat_qq_line(color = "hotpink") + 
  geom_abline(intercept = 0, slope = 1, color = "darkolivegreen")+
  theme_bw()+
  labs(title = "Roots")

#Estimater

hat_beta <- c(lm_leafs_log$coefficients[[1]],
              lm_roots_log$coefficients[[1]],
              lm_wood_log$coefficients[[1]])

hat_alpha <- c(lm_leafs_log$coefficients[[2]],
               lm_roots_log$coefficients[[2]],
               lm_wood_log$coefficients[[2]])

var_hat <- c(var(lm_leafs_log$residuals),
             var(lm_roots_log$residuals),
             var(lm_wood_log$residuals))

#log_hat estimater uden bias correction:

f_hat_leafs <- function(x) hat_beta[1] + hat_alpha[1]*x
f_hat_roots <- function(x) hat_beta[2] + hat_alpha[2]*x
f_hat_wood <- function(x) hat_beta[3] + hat_alpha[3]*x

#Y_hat estimater med bias correction: 
f_hat_leafs_exp_adj <- function(x) exp(hat_beta[1] + hat_alpha[1]*x)*exp(var_hat[1]/2)
f_hat_roots_exp_adj <- function(x) exp(hat_beta[2] + hat_alpha[2]*x)*exp(var_hat[2]/2)
f_hat_wood_exp_adj <- function(x) exp(hat_beta[3] + hat_alpha[3]*x)*exp(var_hat[3]/2)


#Prediction intervals log scale

alpha <- 0.1

upper_leafs <- function(x) {
  f_hat_leafs(x) - qt(alpha/2, nrow(train_leafs_log)-1)*sqrt(x^2/sum(train_leafs_log$Sc^2)+1)*sqrt(var_hat[1])
}

lower_leafs <- function(x) {
  f_hat_leafs(x) - qt(1-alpha/2, nrow(train_leafs_log)-1)*sqrt(x^2/sum(train_leafs_log$Sc^2)+1)*sqrt(var_hat[1])
}

upper_wood <- function(x) {
  f_hat_wood(x) - qt(alpha/2, nrow(train_wood_log)-1)*sqrt(x^2/sum(train_wood_log$Sc^2)+1)*sqrt(var_hat[3])
}

lower_wood <- function(x) {
  f_hat_wood(x) - qt(1-alpha/2, nrow(train_wood_log)-1)*sqrt(x^2/sum(train_wood_log$Sc^2)+1)*sqrt(var_hat[3])
}

upper_roots <- function(x) {
  f_hat_roots(x) - qt(alpha/2, nrow(train_roots_log)-1)*sqrt(x^2/sum(train_roots_log$Sc^2)+1)*sqrt(var_hat[3])
}

lower_roots <- function(x) {
  f_hat_roots(x) - qt(1-alpha/2, nrow(train_roots_log)-1)*sqrt(x^2/sum(train_roots_log$Sc^2)+1)*sqrt(var_hat[3])
}


#Plot with prediction intervals log scale

test_leafs_log_plot <- test_leafs_log %>%
  mutate(Indicator = if_else((lower_leafs(test_leafs_log$Sc) <= test_leafs_log$Kgp)&
                               (test_leafs_log$Kgp <= upper_leafs(test_leafs_log$Sc)),"in", "out"))

test_roots_log_plot <- test_roots_log %>%
  mutate(Indicator = if_else((lower_roots(test_roots_log$Sc) <= test_roots_log$Kgp)&
                               (test_roots_log$Kgp <= upper_roots(test_roots_log$Sc)),"in", "out"))

test_wood_log_plot <- test_wood_log %>%
  mutate(Indicator = if_else((lower_wood(test_wood_log$Sc) <= test_wood_log$Kgp)&
                               (test_wood_log$Kgp <= upper_wood(test_wood_log$Sc)),"in", "out"))

color <- c("in" = "darkolivegreen", "out" = "darkolivegreen3")

ggplot(test_leafs_log_plot, aes(x = Sc, y = Kgp)) + 
  geom_point(aes(color = Indicator)) + 
  theme_bw() +
  xlab('log(Sc)') + 
  ylab('log(Kgp)')+
  geom_function(fun = f_hat_leafs, colour = "hotpink1") +
  geom_function(fun = upper_leafs, colour = "hotpink4") +
  geom_function(fun = lower_leafs, colour = "hotpink4") +
  labs(title = "Leafs")+
  scale_color_manual(values = color)

ggplot(test_roots_log_plot, aes(x = Sc, y = Kgp)) + 
  geom_point(aes(color = Indicator)) + 
  theme_bw() +
  xlab('log(Sc)') + 
  ylab('log(Kgp)')+
  geom_function(fun = f_hat_roots, colour = "hotpink1") +
  geom_function(fun = upper_roots, colour = "hotpink4") +
  geom_function(fun = lower_roots, colour = "hotpink4") +
  labs(title = "roots")+
  scale_color_manual(values = color)

ggplot(test_wood_log_plot, aes(x = Sc, y = Kgp)) + 
  geom_point(aes(color = Indicator)) + 
  theme_bw() +
  xlab('log(Sc)') + 
  ylab('log(Kgp)')+
  geom_function(fun = f_hat_wood, colour = "hotpink1") +
  geom_function(fun = upper_wood, colour = "hotpink4") +
  geom_function(fun = lower_wood, colour = "hotpink4") +
  labs(title = "wood")+
  scale_color_manual(values = color)

coverage <- function(data, upper, lower){
  mean(lower(data$Sc) <= data$Kgp &upper(data$Sc) >= data$Kgp)
}

a <- coverage(test_leafs_log, upper_leafs, lower_leafs)
b <- coverage(test_wood_log, upper_wood, lower_wood)
c <- coverage(test_roots_log, upper_roots, lower_roots)

xtable(tibble("Data" = c("Leafs", "Wood", "Roots"), "Coverage" = c(a,b,c)), type = latex)

#Plot with prediction intervals real scale

#Y_hat estimater uden bias correction: 
f_hat_leafs_exp <- function(x) exp(hat_beta[1])*x^hat_alpha[1]
f_hat_roots_exp <- function(x) exp(hat_beta[2])*x^hat_alpha[2]
f_hat_wood_exp <- function(x) exp(hat_beta[3])*x^hat_alpha[3]

upper_leafs_exp <- function(x) exp(upper_leafs(log(x)))
lower_leafs_exp <- function(x) exp(lower_leafs(log(x)))
upper_wood_exp <- function(x) exp(upper_wood(log(x)))
lower_wood_exp <- function(x) exp(lower_wood(log(x)))
upper_roots_exp <- function(x) exp(upper_roots(log(x)))
lower_roots_exp <- function(x) exp(lower_roots(log(x)))

test_leafs_plot <- test_leafs %>%
  mutate(Indicator = if_else((lower_leafs_exp(Sc) <= Kgp)&
                               (Kgp <= upper_leafs_exp(Sc)),"in", "out"))

test_wood_plot <- test_wood %>%
  mutate(Indicator = if_else((lower_wood_exp(Sc) <= Kgp)&
                               (Kgp <= upper_wood_exp(Sc)),"in", "out"))

test_roots_plot <- test_roots %>%
  mutate(Indicator = if_else((lower_roots_exp(Sc) <= Kgp)&
                               (Kgp <= upper_roots_exp(Sc)),"in", "out"))


color <- c("in" = "darkolivegreen", "out" = "darkolivegreen3")

ggplot(test_leafs_plot, aes(x = Sc, y = Kgp)) + 
  geom_point(aes(color = Indicator)) + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  geom_function(fun = f_hat_leafs_exp, colour = "hotpink1") +
  geom_function(fun = upper_leafs_exp, colour = "hotpink4") +
  geom_function(fun = lower_leafs_exp, colour = "hotpink4") +
  labs(title = "Leafs")+
  scale_color_manual(values = color)

ggplot(test_roots_plot, aes(x = Sc, y = Kgp)) + 
  geom_point(aes(color = Indicator)) + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  geom_function(fun = f_hat_roots_exp, colour = "hotpink1") +
  geom_function(fun = upper_roots_exp, colour = "hotpink4") +
  geom_function(fun = lower_roots_exp, colour = "hotpink4") +
  labs(title = "Roots")+
  scale_color_manual(values = color)

ggplot(test_wood_plot, aes(x = Sc, y = Kgp)) + 
  geom_point(aes(color = Indicator)) + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  geom_function(fun = f_hat_wood_exp, colour = "hotpink1") +
  geom_function(fun = upper_wood_exp, colour = "hotpink4") +
  geom_function(fun = lower_wood_exp, colour = "hotpink4") +
  labs(title = "Wood")+
  scale_color_manual(values = color)

coverage <- function(data, upper, lower){
  mean(lower(data$Sc) <= exp(data$Kgp) & upper(data$Sc) >= exp(data$Kgp))
}

a <- coverage(test_leafs_log, upper_leafs_exp, lower_leafs_exp)
b <- coverage(test_wood_log, upper_wood_exp, lower_wood_exp)
c <- coverage(test_roots_log, upper_roots_exp, lower_roots_exp)

xtable(tibble("Data" = c("Leafs", "Wood", "Roots"), "Coverage" = c(a,b,c)), type = latex)

#Cv on coverage: 

cv_cov <- function(data, k, alpha) {
  cov <- c()
  n <- nrow(data)
  group <- sample(rep(1:k, length.out = n))
  for (i in (1:k)){
    #Fit model
    n_cv <- nrow(data[group != i, ])
    lm_cv <- lm(Kgp ~ Sc, data = data[group != i, ])
    sd_hat <- sqrt(sum(lm_cv$residuals^2)/(n_cv-1))
    f_hat <- function(x) lm_cv$coefficients[[2]]*x + lm_cv$coefficients[[1]] 
    
    #Quantiles
    upper <- function(x) {
      f_hat(x) - qt(alpha/2, n_cv-1)*sqrt(x^2/sum(data[group != i, ]$Sc^2)+1)*sd_hat
    }
    
    lower <- function(x) {
      f_hat(x) - qt(1-alpha/2, n_cv-1)*sqrt(x^2/sum(data[group != i, ]$Sc^2)+1)*sd_hat
    }
    
    #Definere
    cov[i] <- mean(lower(data[group == i, ]$Sc) <= data[group == i, ]$Kgp 
                   &upper(data[group == i, ]$Sc) >= data[group == i, ]$Kgp)
  }
  return(tibble("Coverage" = cov))
}

#Checking for correct coverage

set.seed(4)

a <- rbind(cv_cov(leafs_log, 10, 0.1), cv_cov(leafs_log, 10, 0.1), cv_cov(leafs_log, 10, 0.1))
b <- rbind(cv_cov(wood_log, 10, 0.1), cv_cov(wood_log, 10, 0.1), cv_cov(wood_log, 10, 0.1))
c <- rbind(cv_cov(roots_log, 10, 0.1), cv_cov(roots_log, 10, 0.1), cv_cov(roots_log, 10, 0.1))

a %>%
  ggplot() +
  geom_histogram(aes(x = Coverage, y = ..density..), color = "white", 
                 fill = "darkolivegreen3", binwidth = 0.012)+
  geom_vline(xintercept = 0.9, color = "hotpink") +
  theme_bw()+
  labs(title = "Foliage")

b %>%
  ggplot() +
  geom_histogram(aes(x = Coverage, y = ..density..), color = "white", 
                 fill = "darkolivegreen3", binwidth = 0.013)+
  geom_vline(xintercept = 0.9, color = "hotpink") +
  theme_bw()+
  labs(title = "Wood")

c %>%
  ggplot() +
  geom_histogram(aes(x = Coverage, y = ..density..), color = "white", 
                 fill = "darkolivegreen3", binwidth = 0.03)+
  geom_vline(xintercept = 0.9, color = "hotpink") +
  theme_bw()+
  labs(title = "Roots")

#Mean coverage:

xtable(tibble(Data = c("Leafs", "Wood", "Roots"), 
       "Mean coverage" =c(mean(a$Coverage), mean(b$Coverage), mean(c$Coverage))), type = latex)


#Checking for conditional coverage

bins <- sort(leafs_log_test$Sc)[seq(1,10)*floor(nrow(leafs_log_test)/10)]

indi <- c()

for (i in (1:nrow(leafs_log_test))){
  x <- test_leafs_log$Sc[i]
  print(x)
  if (x < bins[1]){
    indi[i] <- 1
  } else if (x < bins[2]){
    indi[i] <- 2
  } else if (x < bins[3]){
    indi[i] <- 3
  } else if (x < bins[4]){
    indi[i] <- 4
  } else if (x < bins[5]){
    indi[i] <- 5
  } else if (x < bins[6]){
    indi[i] <- 6
  } else if (x < bins[7]){
    indi[i] <- 7
  } else if (x < bins[8]){
    indi[i] <- 8
  } else if (x < bins[9]){
    indi[i] <- 9
  } else 
    indi[i] <- 10
}
}


test_leafs_log_cond <- test_leafs_log %>%
  mutate(Indicator = if_else((lower_leafs(test_leafs_log$Sc) <= test_leafs_log$Kgp)&
                               (test_leafs_log$Kgp <= upper_leafs(test_leafs_log$Sc)),"in", "out"))

       