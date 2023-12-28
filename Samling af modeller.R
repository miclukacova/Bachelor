#################################--Indlæsning af pakker og data---###############################

leafs_log <- read.csv('Data/leafs_log.csv')
roots_log <- read.csv('Data/roots_log.csv')
wood_log<- read.csv('Data/wood_log.csv')

leafs <- read.csv('Data/leafs.csv')
roots <- read.csv('Data/roots.csv')
wood <- read.csv('Data/wood.csv')

library(tidyverse)
library(readr)
library(infer)
library(foreign)
library(xtable)
library(stargazer)
library(randomForest)
###################################################################################################

#Lineære modeller af log-log

lm_log_l <- lm(Kgp ~ Sc, data = leafs_log)
lm_log_w <- lm(Kgp ~ Sc, data = wood_log)
lm_log_r <- lm(Kgp ~ Sc, data = roots_log)

#Lineære modeller

lm_l <- lm(Kgp ~ Sc, data = leafs)
lm_w <- lm(Kgp ~ Sc, data = wood)
lm_r <- lm(Kgp ~ Sc, data = roots)

#Log log OLS 

ols_log_l <- function(x) exp(lm_log_l$coefficients[[1]] + lm_log_l$coefficients[[2]]*log(x))
ols_log_w <- function(x) exp(lm_log_w$coefficients[[1]] + lm_log_w$coefficients[[2]]*log(x))
ols_log_r <- function(x) exp(lm_log_r$coefficients[[1]] + lm_log_r$coefficients[[2]]*log(x))

ols_log_adj_l <- function(x) exp(lm_log_l$coefficients[[1]] + 
                                     lm_log_l$coefficients[[2]]*log(x))*exp(var(lm_log_l$residuals)/2)
ols_log_adj_w <- function(x) exp(lm_log_w$coefficients[[1]] +
                                  lm_log_w$coefficients[[2]]*log(x))*exp(var(lm_log_w$residuals)/2)
ols_log_adj_r <- function(x) exp(lm_log_r$coefficients[[1]] + 
                                     lm_log_r$coefficients[[2]]*log(x))*exp(var(lm_log_r$residuals)/2)

#OLS 

ols_l <- function(x) lm_l$coefficients[[1]] + lm_l$coefficients[[2]]*x
ols_w <- function(x) lm_w$coefficients[[1]] + lm_w$coefficients[[2]]*x
ols_r <- function(x) lm_r$coefficients[[1]] + lm_r$coefficients[[2]]*x

#NLR

nlr_l <- function(x) 0.5687030*x^0.7160185
nlr_w <- function(x) 6.9505933*x^0.9842457
nlr_r <- function(x) 0.1206275*x^1.7372176


###################################################################################################


#Plot på alt data real scale

cols <- c("darkolivegreen","black", "hotpink2", "hotpink4")

ggplot(leafs, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'black', fill = 'darkolivegreen2', alpha = 0.6, size = 1.5, shape = 21) + 
  theme_bw() +
  xlab('Sc') + 
  geom_function(fun = ols_l, aes(col = "OLS"), size = 1)+
  geom_function(fun = nlr_l, aes(col = "NLR"), size = 1)+
  geom_function(fun = ols_log_l, aes(col = "OLS log"), size = 1)+
  geom_function(fun = ols_log_adj_l, aes(col = "OLS log Bias adj."), size = 1)+
  ylab('Kgp')+
  labs(title = "Leafs")+
  scale_colour_manual(values = cols)+
  theme(legend.title = element_blank(),
               legend.position = c(0.165, 0.82), legend.background = element_rect(linetype = 'solid', color = 'black'),
               plot.title = element_text(size = 17),
               axis.title = element_text(size = 13))+
  ylim(c(0,35))


ggplot(wood, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'black', fill = 'darkolivegreen2', alpha = 0.6, size = 1.5, shape = 21) + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  geom_function(fun = ols_w, aes(col = "OLS"), size = 1)+
  geom_function(fun = nlr_w, aes(col = "NLR"), size = 0.7)+
  geom_function(fun = ols_log_w, aes(col = "OLS log"), size = 0.7)+
  geom_function(fun = ols_log_adj_w, aes(col = "OLS log Bias adj."), size = 1)+
  labs(title = "Wood")+
  scale_colour_manual(values = cols)+
  theme( legend.title = element_blank(),
               legend.position = c(0.165, 0.82), legend.background = element_rect(linetype = 'solid', color = 'black'),
               plot.title = element_text(size = 17),
               axis.title = element_text(size = 13))+
  ylim(c(0,1000))

ggplot(roots, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'black', fill = 'darkolivegreen', alpha = 0.6, size = 2., shape = 21) + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  geom_function(fun = ols_r, aes(col = "OLS"), size = 0.7)+
  geom_function(fun = nlr_r, aes(col = "NLR"), size = 0.7)+
  geom_function(fun = ols_log_r, aes(col = "OLS log"), size = 0.7)+
  geom_function(fun = ols_log_adj_r, aes(col = "OLS log Bias adj."), size = 0.7)+
  ggtitle( "Roots")+
  scale_colour_manual(values = cols)+
  theme( legend.title = element_blank(),
               legend.position = c(0.165, 0.82), legend.background = element_rect(linetype = 'solid', color = 'black'),
               plot.title = element_text(size = 17),
               axis.title = element_text(size = 13))

