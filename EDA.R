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

#Uden log-log transformationer

ggplot(leafs, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'darkolivegreen',fill = 'darkolivegreen3', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab('Crown Size')+
  ylab('Biomass')+
  geom_smooth(method = lm, se = FALSE, formula = y ~ x, color = 'hotpink')+
  labs(title = "Leafs")+
  theme(text = element_text(family = "serif"),legend.position = "none", plot.title = element_text(size = 19),
        axis.title = element_text(size = 15), axis.text = element_text(size = 13))

ggplot(roots, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'darkolivegreen',fill = 'darkolivegreen4', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab('Crown Size')+
  ylab('Biomass')+
  geom_smooth(method = lm, se = FALSE, formula = y ~ x, color = 'hotpink')+
  labs(title = "Roots")+
  theme(text = element_text(family = "serif"),legend.position = "none", plot.title = element_text(size = 19),
        axis.title = element_text(size = 15), axis.text = element_text(size = 13))

ggplot(wood, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'darkolivegreen',fill = 'darkolivegreen3', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab('Crown Size')+
  ylab('Biomass')+
  geom_smooth(method = lm, se = FALSE, formula = y ~ x, color = 'hotpink')+
  labs(title = "Wood")+ 
  theme(text = element_text(family = "serif"),legend.position = "none", plot.title = element_text(size = 19),
                              axis.title = element_text(size = 15), axis.text = element_text(size = 13))

#Med log-log transformation

ggplot(leafs_log, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'darkolivegreen', fill = 'darkolivegreen3', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab('log(Crown Size)')+
  ylab('log(Biomass)')+
  geom_smooth(method = lm, se = FALSE, formula = y ~ x, color = 'hotpink')+
  labs(title = "Leafs")+
  theme(text = element_text(family = "serif"),legend.position = "none", plot.title = element_text(size = 19),
        axis.title = element_text(size = 15), axis.text = element_text(size = 13))

ggplot(roots_log, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'darkolivegreen', fill = 'darkolivegreen4', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab('log(Crown Size)')+
  ylab('log(Biomass)')+
  geom_smooth(method = lm, se = FALSE, formula = y ~ x, color = 'hotpink')+
  labs(title = "Roots")+
  theme(text = element_text(family = "serif"),legend.position = "none", plot.title = element_text(size = 19),
        axis.title = element_text(size = 15), axis.text = element_text(size = 13))

ggplot(wood_log, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'darkolivegreen',fill = 'darkolivegreen3', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab('log(Crown Size)')+
  ylab('log(Biomass)')+
  geom_smooth(method = lm, se = FALSE, formula = y ~ x, color = 'hotpink')+
  labs(title = "Wood")+  
  theme(text = element_text(family = "serif"),legend.position = "none", plot.title = element_text(size = 19),
                               axis.title = element_text(size = 15), axis.text = element_text(size = 13))


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
#First we get the unpermuted data:
leafs_unperm <- read.csv('Data/leafs_unperm.csv')
roots_unperm <- read.csv('Data/roots_unperm.csv')
wood_unperm <- read.csv('Data/wood_unperm.csv')

lm_leafs_log <- lm(log(Kgp) ~ log(Sc), leafs_unperm)
lm_wood_log<- lm(log(Kgp) ~ log(Sc), wood_unperm)
lm_roots_log <- lm(log(Kgp) ~ log(Sc), roots_unperm)


leafs_log_plot <- log(leafs_unperm) %>% mutate(order = seq(1,nrow(leafs_log)))

ggplot(leafs_log_plot, aes(x = order, y = rstandard(lm_leafs_log))) + 
  geom_point(color = 'darkolivegreen', fill = 'darkolivegreen3', alpha = 0.6, shape = 21) +  
  theme_bw() +
  xlab('Order') + 
  ylab('Residuals')+
  geom_smooth(se = FALSE, formula = y ~ x, color = "hotpink")+
  labs(title = "Leafs")+
  theme(text = element_text(family = "serif"),legend.position = "none", plot.title = element_text(size = 19),
      axis.title = element_text(size = 15), axis.text = element_text(size = 13))

wood_log_plot <- wood_log %>% mutate(order = seq(1,nrow(wood_log)))

ggplot(wood_log_plot, aes(x = order, y = rstandard(lm_wood_log))) + 
  geom_point(color = 'darkolivegreen', fill = 'darkolivegreen3', alpha = 0.6, shape = 21) +  
  theme_bw() +
  xlab('Order') + 
  ylab('Residuals')+
  geom_smooth(se = FALSE, formula = y ~ x, color = "hotpink")+
  labs(title = "Wood")+
  theme(text = element_text(family = "serif"),legend.position = "none", plot.title = element_text(size = 19),
        axis.title = element_text(size = 15), axis.text = element_text(size = 13))

roots_log_plot <- roots_log %>% mutate(order = seq(1,nrow(roots_log)))

ggplot(roots_log_plot, aes(x = order, y = rstandard(lm_roots_log))) + 
  geom_point(color = 'darkolivegreen', fill = 'darkolivegreen3', alpha = 0.6, shape = 21) +  
  theme_bw() +
  xlab('Order') + 
  ylab('Residuals')+
  geom_smooth( se = FALSE, formula = y ~ x, color = "hotpink")+
  labs(title = "Roots")+  
  theme(text = element_text(family = "serif"),legend.position = "none", plot.title = element_text(size = 19),
                                axis.title = element_text(size = 15), axis.text = element_text(size = 13))




##########################--------Fitting y_t from y_t-1--------################

#############################-----------Leafs-----------########################

#Creating a dataframe
#Log vs ikke log, giver kun en forskel i skala. Hvorfor mon?

leafs_lag <- tibble("y_t" = leafs_unperm$Kgp[2:nrow(leafs)], "y_t_1" = leafs_unperm$Kgp[1:nrow(leafs)-1])

#Plotting

ggplot(leafs_lag, aes(x = y_t_1, y = y_t)) + 
  geom_point(color = 'darkolivegreen', fill = 'darkolivegreen3', alpha = 0.6, shape = 21) +  
  theme_bw() +
  xlab('y_t-1') + 
  ylab('y_t')+
  labs(title = "Leafs")+
  theme(text = element_text(family = "serif"),legend.position = "none", plot.title = element_text(size = 19),
        axis.title = element_text(size = 15), axis.text = element_text(size = 13))

#############################-----------Wood-----------########################

#Creating a dataframe

wood_lag <- tibble("y_t" = wood_unperm$Kgp[2:nrow(wood)], "y_t_1" = wood_unperm$Kgp[1:nrow(wood)-1])

#Plotting

ggplot(wood_lag, aes(x = y_t_1, y = y_t)) + 
  geom_point(color = 'darkolivegreen', fill = 'darkolivegreen3', alpha = 0.6, shape = 21) +  
  theme_bw() +
  xlab('y_t-1') + 
  ylab('y_t')+
  labs(title = "Wood")+
  theme(text = element_text(family = "serif"),legend.position = "none", plot.title = element_text(size = 19),
        axis.title = element_text(size = 15), axis.text = element_text(size = 13))



#############################-----------Roots-----------########################

#Creating a dataframe

roots_lag <- tibble("y_t" = roots_unperm$Kgp[2:nrow(roots)], "y_t_1" = roots_unperm$Kgp[1:nrow(roots)-1])

#Plotting

ggplot(roots_lag, aes(x = y_t_1, y = y_t)) + 
  geom_point(color = 'darkolivegreen', fill = 'darkolivegreen3', alpha = 0.6, shape = 21) +  
  theme_bw() +
  xlab('y_t-1') + 
  ylab('y_t')+
  labs(title = "Roots")+
  theme(text = element_text(family = "serif"),legend.position = "none", plot.title = element_text(size = 19),
        axis.title = element_text(size = 15), axis.text = element_text(size = 13))




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


#Model diagnostics


res_vs_fit <- function(data, bias = TRUE) {
  #Model fit
  lm_log <- lm(log(Kgp) ~ log(Sc), data)
  if (bias == TRUE){
    f_hat <- function(x) {
      exp(lm_log$coefficients[[1]])*x^(lm_log$coefficients[[2]])*exp(var(lm_log$residuals)/2)}
  }
  else {
    f_hat <- function(x) exp(lm_log$coefficients[[1]])*x^(lm_log$coefficients[[2]])
  }
  
  #Residuals
  res_fit <- tibble(res = f_hat(data$Sc) - data$Kgp, fit = f_hat(data$Sc))
  
  #Plot
  ggplot(res_fit, aes(x = fit, y = res)) + 
    geom_point(color = 'darkolivegreen', fill = 'darkolivegreen3', alpha = 0.6, shape = 21) +  
    theme_bw() +
    xlab('Fitted values') + 
    ylab('Residuals')+
    labs(title = "Residual plot")
}

res_vs_fit(leafs)
res_vs_fit(leafs, bias = FALSE)
res_vs_fit(wood, bias = FALSE)
res_vs_fit(wood)
res_vs_fit(roots)
res_vs_fit(roots, bias = FALSE)


