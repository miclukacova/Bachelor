library(readODS)
library(tidyverse)
library(ggplot2)
library(ggpubr)

leafs <- read.csv('Data/leafs.csv')
roots <- read.csv('Data/roots.csv')
wood <- read.csv('Data/wood.csv')
leafs_log <- read.csv('Data/leafs_log.csv')
roots_log <- read.csv('Data/roots_log.csv')
wood_log <- read.csv('Data/wood_log.csv')

#Histogrammer over fordelingen af biomasse

a <- ggplot(data = leafs)+
  geom_histogram(aes(x=Kgp, y=..density..), color = 'darkolivegreen',fill = 'darkolivegreen3', bins = 15)+
  theme_bw()+
  labs(title = "Leafs")

b <- ggplot(data = wood)+
  geom_histogram(aes(x=Kgp, y=..density..), color = 'darkolivegreen',fill = 'darkolivegreen3', bins = 15)+
  theme_bw()+
  labs(title = "Wood")

c <- ggplot(data = roots)+
  geom_histogram(aes(x=Kgp, y=..density..), color = 'darkolivegreen',fill = 'darkolivegreen3', bins = 15)+
  theme_bw()+
  labs(title = "Roots")

ggarrange(a,b,c, ncol = 3, nrow = 1)

#Histogrammer over fordelingen af biomasse efter log transformationen

a1 <- ggplot(data = leafs_log)+
  geom_histogram(aes(x=Kgp, y=..density..), color = 'darkolivegreen',fill = 'darkolivegreen3', bins = 15)+
  theme_bw()+
  labs(title = "Leafs")

b1 <- ggplot(data = wood_log)+
  geom_histogram(aes(x=Kgp, y=..density..), color = 'darkolivegreen',fill = 'darkolivegreen3', bins = 15)+
  theme_bw()+
  labs(title = "Wood")

c1 <- ggplot(data = roots_log)+
  geom_histogram(aes(x=Kgp, y=..density..), color = 'darkolivegreen',fill = 'darkolivegreen3', bins = 15)+
  theme_bw()+
  labs(title = "Roots")

ggarrange(a1,b1,c1, ncol = 3, nrow = 1)


#Uden log-log transformationer

leafs_sc_plot <- ggplot(leafs, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'darkolivegreen',fill = 'darkolivegreen3', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  geom_smooth(method = lm, se = FALSE, formula = y ~ x, color = 'hotpink')+
  labs(title = "Foliage")

roots_sc_plot <- ggplot(roots, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'darkolivegreen',fill = 'darkolivegreen4', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  geom_smooth(method = lm, se = FALSE, formula = y ~ x, color = 'hotpink')+
  labs(title = "Roots")

wood_sc_plot <- ggplot(wood, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'darkolivegreen',fill = 'darkolivegreen3', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  geom_smooth(method = lm, se = FALSE, formula = y ~ x, color = 'hotpink')+
  labs(title = "Wood")

ggarrange(leafs_sc_plot, roots_sc_plot, wood_sc_plot, ncol = 3, nrow = 1)

#Med log-log transformation

ggplot(leafs_log, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'darkolivegreen', fill = 'darkolivegreen3', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  geom_smooth(method = lm, se = FALSE, formula = y ~ x, color = 'hotpink')+
  labs(title = "Foliage")

ggplot(roots_log, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'darkolivegreen', fill = 'darkolivegreen4', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  geom_smooth(method = lm, se = FALSE, formula = y ~ x, color = 'hotpink')+
  labs(title = "Roots")

ggplot(wood_log, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'darkolivegreen',fill = 'darkolivegreen3', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  geom_smooth(method = lm, se = FALSE, formula = y ~ x, color = 'hotpink')+
  labs(title = "Wood")


#Residual plots

lm_leafs <- lm(Kgp ~ Sc, leafs)
lm_wood <- lm(Kgp ~ Sc, wood)
lm_roots <- lm(Kgp ~ Sc, roots)
lm_leafs_log <- lm(Kgp ~ Sc, leafs_log)
lm_wood_log<- lm(Kgp ~ Sc, wood_log)
lm_roots_log <- lm(Kgp ~ Sc, roots_log)

ggplot(lm_leafs, aes(x = lm_leafs$fitted.values, y = lm_leafs$residuals)) + 
  geom_point(color = 'darkolivegreen',fill = 'darkolivegreen3', alpha = 0.3, shape = 21)  +
  geom_smooth(method = lm, se = FALSE, formula = y ~ x, color = "hotpink")+
  theme_bw() +
  xlab('Fitted values') + 
  ylab('Residuals')+
  labs(title = "Leafs")

ggplot(lm_wood, aes(x = lm_wood$fitted.values, y = lm_wood$residuals)) +
  geom_point(color = 'darkolivegreen',fill = 'darkolivegreen3', alpha = 0.3, shape = 21)  +
  geom_smooth(method = lm, se = FALSE, formula = y ~ x, color = "hotpink")+
  theme_bw() +
  xlab('Fitted values') + 
  ylab('Residuals')+
  labs(title = "Wood")

ggplot(lm_roots, aes(x = lm_roots$fitted.values, y = lm_roots$residuals)) + 
  geom_point(color = 'darkolivegreen',fill = 'darkolivegreen3', alpha = 0.7, shape = 21)  + 
  geom_smooth(method = lm, se = FALSE, formula = y ~ x, color = "hotpink")+
  theme_bw() +
  xlab('Fitted values') + 
  ylab('Residuals')+
  labs(title = "Roots")


#Residual plots

ggplot(lm_leafs_log, aes(x = lm_leafs_log$fitted.values, y = lm_leafs_log$residuals)) + 
  geom_point(color = 'darkolivegreen', fill = 'darkolivegreen3', alpha = 0.6, shape = 21) +  
  theme_bw() +
  xlab('Fitted values') + 
  ylab('Residuals')+
  geom_smooth(method = lm, se = FALSE, formula = y ~ x)+
  labs(title = "Residual plot")
       
ggplot(lm_roots_log, aes(x = lm_roots_log$fitted.values, y = lm_roots_log$residuals)) + 
  geom_point(color = 'darkolivegreen', fill = 'darkolivegreen3', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab('Fitted values') + 
  ylab('Residuals')+
  labs(title = "Resdiual plot")

ggplot(lm_wood_log, aes(x = lm_wood_log$fitted.values, y = lm_wood_log$residuals)) + 
  geom_point(color = 'darkolivegreen', fill = 'darkolivegreen3', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab('Fitted values') + 
  ylab('Residuals')+
  labs(title = "Resdiual plot")
       
#Standardized residual plots       

ggplot(lm_leafs_log, aes(x = lm_leafs_log$fitted.values, y = rstandard(lm_leafs_log))) + 
  geom_point() + 
  theme_bw() +
  xlab('Fitted values') + 
  ylab('Residuals')+
  geom_smooth(method = lm, se = FALSE, formula = y ~ x)+
  labs(title = "Standardized Resdiual plot")

ggplot(lm_roots_log, aes(x = lm_roots_log$fitted.values, y = rstandard(lm_roots_log))) + 
  geom_point() + 
  theme_bw() +
  xlab('Fitted values') + 
  ylab('Standardized Residuals')+
  geom_smooth(method = lm, se = FALSE, formula = y ~ x)+
  labs(title = "Resdiual plot")

ggplot(lm_wood_log, aes(x = lm_wood_log$fitted.values, y = rstandard(lm_wood_log))) + 
  geom_point() + 
  theme_bw() +
  xlab('Fitted values') + 
  ylab('Residuals')+
  geom_smooth(method = lm, se = FALSE, formula = y ~ x)+
  labs(title = "Standardized Resdiual plot")

# Residuals against predictor 

ggplot(leafs_log, aes(x = Sc, y = rstandard(lm_leafs_log))) + 
  geom_point(color = 'darkolivegreen', fill = 'darkolivegreen3', alpha = 0.6, shape = 21) +  
  theme_bw() +
  xlab('Sc') + 
  ylab('Residuals')+
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x, color = "hotpink")+
  labs(title = "Leafs")

ggplot(wood_log, aes(x = Sc, y = rstandard(lm_wood_log))) + 
  geom_point(color = 'darkolivegreen', fill = 'darkolivegreen3', alpha = 0.6, shape = 21) +  
  theme_bw() +
  xlab('Sc') + 
  ylab('Residuals')+
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x, color = "hotpink")+
  labs(title = "Wood")

ggplot(roots_log, aes(x = Sc, y = rstandard(lm_roots_log))) + 
  geom_point(color = 'darkolivegreen', fill = 'darkolivegreen3', alpha = 0.6, shape = 21) +  
  theme_bw() +
  xlab('Sc') + 
  ylab('Residuals')+
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x, color = "hotpink")+
  labs(title = "Roots")

# Residuals against order

leafs_log_plot <- leafs_log %>% mutate(order = seq(1,nrow(leafs_log)))

ggplot(leafs_log_plot, aes(x = order, y = rstandard(lm_leafs_log))) + 
  geom_point(color = 'darkolivegreen', fill = 'darkolivegreen3', alpha = 0.6, shape = 21) +  
  theme_bw() +
  xlab('Order') + 
  ylab('Residuals')+
  geom_smooth(se = FALSE, formula = y ~ x, color = "hotpink")+
  labs(title = "Leafs")

wood_log_plot <- wood_log %>% mutate(order = seq(1,nrow(wood_log)))

ggplot(wood_log_plot, aes(x = order, y = rstandard(lm_wood_log))) + 
  geom_point(color = 'darkolivegreen', fill = 'darkolivegreen3', alpha = 0.6, shape = 21) +  
  theme_bw() +
  xlab('Order') + 
  ylab('Residuals')+
  geom_smooth(se = FALSE, formula = y ~ x, color = "hotpink")+
  labs(title = "Wood")

roots_log_plot <- roots_log %>% mutate(order = seq(1,nrow(roots_log)))

ggplot(roots_log_plot, aes(x = order, y = rstandard(lm_roots_log))) + 
  geom_point(color = 'darkolivegreen', fill = 'darkolivegreen3', alpha = 0.6, shape = 21) +  
  theme_bw() +
  xlab('Order') + 
  ylab('Residuals')+
  geom_smooth( se = FALSE, formula = y ~ x, color = "hotpink")+
  labs(title = "Roots")




##########################--------Fitting y_t from y_t-1--------################

#############################-----------Leafs-----------########################

#Creating a dataframe
#Log vs ikke log, giver kun en forskel i skala. Hvorfor mon?

leafs <- read.csv('Data/leafs.csv')
leafs_lag <- tibble("y_t" = leafs$Kgp[2:nrow(leafs)], "y_t_1" = leafs$Kgp[1:nrow(leafs)-1])

#Plotting

ggplot(leafs_lag, aes(x = y_t_1, y = y_t)) + 
  geom_point(color = 'darkolivegreen', fill = 'darkolivegreen3', alpha = 0.6, shape = 21) +  
  theme_bw() +
  xlab('y_t-1') + 
  ylab('y_t')+
  labs(title = "Leafs")

#Fitting model

lm_leafs_lag <- lm(y_t ~ y_t_1, data = leafs_lag)
lm_func_leafs <- function(x) lm_leafs_lag$coefficients[1] + lm_leafs_lag$coefficients[2]*x

#Plotting

ggplot(leafs_lag, aes(x = y_t_1, y = y_t)) + 
  geom_point(color = 'darkolivegreen', fill = 'darkolivegreen3', alpha = 0.6, shape = 21) +  
  theme_bw() +
  xlab('y_t') + 
  ylab('y_t-1')+
  geom_function(fun = lm_func_leafs, color = "hotpink")+
  labs(title = "Leafs")

#Summary

summary(lm_leafs_lag)

#############################-----------Wood-----------########################

#Creating a dataframe

wood_lag <- tibble("y_t" = wood$Kgp[2:nrow(wood)], "y_t_1" = wood$Kgp[1:nrow(wood)-1])

#Plotting

ggplot(wood_lag, aes(x = y_t_1, y = y_t)) + 
  geom_point(color = 'darkolivegreen', fill = 'darkolivegreen3', alpha = 0.6, shape = 21) +  
  theme_bw() +
  xlab('y_t-1') + 
  ylab('y_t')+
  labs(title = "Wood")

#Fitting model

lm_wood_lag <- lm(y_t ~ y_t_1, data = wood_lag)
lm_func_wood <- function(x) lm_wood_lag$coefficients[1] + lm_wood_lag$coefficients[2]*x

#Plotting

ggplot(wood_lag, aes(x = y_t_1, y = y_t)) + 
  geom_point(color = 'darkolivegreen', fill = 'darkolivegreen3', alpha = 0.6, shape = 21) +  
  theme_bw() +
  xlab('y_t') + 
  ylab('y_t-1')+
  geom_function(fun = lm_func_wood, color = "hotpink")+
  labs(title = "wood")

#Summary

summary(lm_wood_lag)


#############################-----------Roots-----------########################

#Creating a dataframe

roots_lag <- tibble("y_t" = roots$Kgp[2:nrow(roots)], "y_t_1" = roots$Kgp[1:nrow(roots)-1])

#Plotting

ggplot(roots_lag, aes(x = y_t_1, y = y_t)) + 
  geom_point(color = 'darkolivegreen', fill = 'darkolivegreen3', alpha = 0.6, shape = 21) +  
  theme_bw() +
  xlab('y_t-1') + 
  ylab('y_t')+
  labs(title = "roots")

#Fitting model

lm_roots_lag <- lm(y_t ~ y_t_1, data = roots_lag)
lm_func_roots <- function(x) lm_roots_lag$coefficients[1] + lm_roots_lag$coefficients[2]*x

#Plotting

ggplot(roots_lag, aes(x = y_t_1, y = y_t)) + 
  geom_point(color = 'darkolivegreen', fill = 'darkolivegreen3', alpha = 0.6, shape = 21) +  
  theme_bw() +
  xlab('y_t') + 
  ylab('y_t-1')+
  geom_function(fun = lm_func_roots, color = "hotpink")+
  labs(title = "roots")

#Summary

summary(lm_roots_lag)


#Permuteret
leafs_perm <- sample(leafs$Kgp, size = nrow(leafs), replace = FALSE)
leafs_lag_perm <- tibble("y_t" = leafs_perm[2:length(leafs_perm)], "y_t_1" = leafs_perm[1:length(leafs_perm)-1])
ggplot(leafs_lag_perm, aes(x = y_t_1, y = y_t)) + 
  geom_point(color = 'darkolivegreen', fill = 'darkolivegreen3', alpha = 0.6, shape = 21) +  
  theme_bw() +
  xlab('y_t-1') + 
  ylab('y_t')+
  labs(title = "Leafs")

summary(lm(y_t ~ y_t_1, data = leafs_lag_perm))




##############################--------QQ-plot--------###########################

residuals_leafs <- data.frame(residual = lm_leafs_log$residuals/sd(lm_leafs_log$residuals))
residuals_wood <- data.frame(residual = lm_wood_log$residuals/sd(lm_wood_log$residuals))
residuals_roots <- data.frame(residual = lm_roots_log$residuals/sd(lm_roots_log$residuals))

residuals_leafs %>%
  ggplot() +
  geom_histogram(aes(x = residual, y = ..density..), color = "white", fill = "darkolivegreen3", bins = 40) +
  geom_vline(xintercept = 0, color = "hotpink", size = 1) +
  stat_function(fun = dnorm, color = "darkolivegreen", size = 1)+
  theme_bw()+
  labs(title = "Foliage")
?geom_vline

residuals_wood %>%
  ggplot() +
  geom_histogram(aes(x = residual, y = ..density..), color = "white", fill = "darkolivegreen3", bins = 40) +
  geom_vline(xintercept = 0, color = "hotpink", size = 1) +
  stat_function(fun = dnorm, color = "darkolivegreen", size = 1)+
  theme_bw()+
  labs(title = "Wood")

residuals_roots %>%
  ggplot() +
  geom_histogram(aes(x = residual, y = ..density..), color = "white", fill = "darkolivegreen3") +
  geom_vline(xintercept = 0, color = "hotpink", size = 1) +
  stat_function(fun = dnorm, color = "darkolivegreen", size = 1)+
  theme_bw()+
  labs(title = "Roots")

ggplot(data = residuals_leafs, aes(sample = residual)) +
  stat_qq() + stat_qq_line(color = "hotpink", size = 0.9) + 
  geom_abline(intercept = 0, slope = 1, color = "darkolivegreen", size = 0.9)+
  theme_bw()+
  labs(title = "Foliage")

ggplot(data = residuals_wood, aes(sample = residual)) +
  stat_qq() + stat_qq_line(color = "hotpink", size = 0.9) + 
  geom_abline(intercept = 0, slope = 1, color = "darkolivegreen", size = 0.9)+
  theme_bw()+
  labs(title = "Wood")

ggplot(data = residuals_roots, aes(sample = residual)) +
  stat_qq() + stat_qq_line(color = "hotpink", size = 0.9) + 
  geom_abline(intercept = 0, slope = 1, color = "darkolivegreen", size = 0.9)+
  theme_bw()+
  labs(title = "Roots")



##------------QQ-PLots on the regular scale:
residuals_leafs <- data.frame(residual = lm_leafs$residuals/sd(lm_leafs$residuals))
residuals_wood <- data.frame(residual = lm_wood$residuals/sd(lm_wood$residuals))
residuals_roots <- data.frame(residual = lm_roots$residuals/sd(lm_roots$residuals))

residuals_leafs %>%
  ggplot() +
  geom_histogram(aes(x = residual, y = ..density..), color = "white", fill = "darkolivegreen3", bins = 40) +
  geom_vline(xintercept = 0, color = "hotpink", size = 1) +
  stat_function(fun = dnorm, color = "darkolivegreen", size = 1)+
  theme_bw()+
  labs(title = "Foliage")


residuals_wood %>%
  ggplot() +
  geom_histogram(aes(x = residual, y = ..density..), color = "white", fill = "darkolivegreen3", bins = 40) +
  geom_vline(xintercept = 0, color = "hotpink", size = 1) +
  stat_function(fun = dnorm, color = "darkolivegreen", size = 1)+
  theme_bw()+
  labs(title = "Wood")

residuals_roots %>%
  ggplot() +
  geom_histogram(aes(x = residual, y = ..density..), color = "white", fill = "darkolivegreen3") +
  geom_vline(xintercept = 0, color = "hotpink", size = 1) +
  stat_function(fun = dnorm, color = "darkolivegreen", size = 1)+
  theme_bw()+
  labs(title = "Roots")

ggplot(data = residuals_leafs, aes(sample = residual)) +
  stat_qq() + stat_qq_line(color = "hotpink", size = 0.9) + 
  geom_abline(intercept = 0, slope = 1, color = "darkolivegreen", size = 0.9)+
  theme_bw()+
  labs(title = "Foliage")

ggplot(data = residuals_wood, aes(sample = residual)) +
  stat_qq() + stat_qq_line(color = "hotpink", size = 0.9) + 
  geom_abline(intercept = 0, slope = 1, color = "darkolivegreen", size = 0.9)+
  theme_bw()+
  labs(title = "Wood")

ggplot(data = residuals_roots, aes(sample = residual)) +
  stat_qq() + stat_qq_line(color = "hotpink", size = 0.9) + 
  geom_abline(intercept = 0, slope = 1, color = "darkolivegreen", size = 0.9)+
  theme_bw()+
  labs(title = "Roots")

