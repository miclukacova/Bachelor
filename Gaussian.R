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


#Checking for conditional coverage----------------------------------------------

coverage <- function(data, upper, lower){
  mean(lower(data$Sc) <= data$Kgp & upper(data$Sc) >= data$Kgp)
}

cond_cov <- function(data, upper, lower, num_bins){
  #Bins
  bins <- sort(data$Sc)[seq(1,num_bins)*floor(nrow(data)/num_bins)]
  indi <- c()
  
  for (i in (1:nrow(data))){
    x <- data$Sc[i]
    j_0 <- - Inf
    for (j in (1:num_bins)){
      if (j_0 < x & x <= bins[j]){
        indi[i] <- j
        j_0 <- bins[j]
      }
    }
  }
  cond_cov_data <- cbind(data, indi = indi)
  
  #Conditional Coverage
  
  cond_cov_vec <- c()
  
  for (i in cond_cov_data$indi){
    zz <- cond_cov_data %>%
      filter(indi == i)
      
    cond_cov_vec[i] <- coverage(zz, upper, lower)
  }
  
  j_0 <- - Inf
  bins2 <- c()
  for (i in (1:num_bins)){
    bins2[i] <- paste("[", j_0, ",", round(bins[i],2), "]")
    j_0 <- round(bins[i],2)
  }
  
  return(tibble(Bin = bins2, "Conditional coverage" = cond_cov_vec))
}

a <- cond_cov(test_leafs_log, upper_leafs, lower_leafs, 5)
b <- cond_cov(test_wood_log, upper_wood, lower_wood, 5)

nrow(test_wood_log)

xtable(cbind(a,b), type = latex)
xtable(b, type = latex)

bins <- sort(test_leafs_log$Sc)[seq(1,5)*floor(nrow(test_leafs_log)/5)]

ggplot(test_leafs_log_plot, aes(x = Sc, y = Kgp)) + 
  geom_point(aes(color = Indicator)) + 
  theme_bw() +
  xlab('log(Sc)') + 
  ylab('log(Kgp)')+
  geom_function(fun = f_hat_leafs, colour = "hotpink1") +
  geom_function(fun = upper_leafs, colour = "hotpink4") +
  geom_function(fun = lower_leafs, colour = "hotpink4") +
  geom_vline(xintercept = bins[1], linetype = "dotted", linewidth = 0.3, color = "hotpink3")+
  geom_vline(xintercept = bins[2], linetype = "dotted", linewidth = 0.3, color = "hotpink3")+
  geom_vline(xintercept = bins[3], linetype = "dotted", linewidth = 0.3, color = "hotpink3")+
  geom_vline(xintercept = bins[4], linetype = "dotted", linewidth = 0.3, color = "hotpink3")+
  geom_vline(xintercept = bins[5], linetype = "dotted", linewidth = 0.3, color = "hotpink3")+
  labs(title = "Leafs")+
  scale_color_manual(values = color)

bins <- sort(test_wood_log$Sc)[seq(1,5)*floor(nrow(test_wood_log)/5)]

ggplot(test_wood_log_plot, aes(x = Sc, y = Kgp)) + 
  geom_point(aes(color = Indicator)) + 
  theme_bw() +
  xlab('log(Sc)') + 
  ylab('log(Kgp)')+
  geom_function(fun = f_hat_wood, colour = "hotpink1") +
  geom_function(fun = upper_wood, colour = "hotpink4") +
  geom_function(fun = lower_wood, colour = "hotpink4") +
  geom_vline(xintercept = bins[1], linetype = "dotted", linewidth = 0.3, color = "hotpink3")+
  geom_vline(xintercept = bins[2], linetype = "dotted", linewidth = 0.3, color = "hotpink3")+
  geom_vline(xintercept = bins[3], linetype = "dotted", linewidth = 0.3, color = "hotpink3")+
  geom_vline(xintercept = bins[4], linetype = "dotted", linewidth = 0.3, color = "hotpink3")+
  geom_vline(xintercept = bins[5], linetype = "dotted", linewidth = 0.3, color = "hotpink3")+
  labs(title = "Wood")+
  scale_color_manual(values = color)
