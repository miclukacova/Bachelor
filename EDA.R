library(readODS)
library(tidyverse)
library(ggplot2)

leafs <- read.csv('Data/leafs.csv')
roots <- read.csv('Data/roots.csv')
wood <- read.csv('Data/wood.csv')
leafs_log <- read.csv('Data/leafs_log.csv')
roots_log <- read.csv('Data/roots_log.csv')
wood_log <- read.csv('Data/wood_log.csv')

#De første par rækker

head(leafs)
head(roots)
head(wood)
head(leafs)
head(roots)
head(wood)

#Uden log-log transformationer

ggplot(leafs, aes(x = Sc, y = Bfkg)) + 
  geom_point() + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Bfkg')+
  geom_smooth(method = lm, se = FALSE, formula = y ~ x)+
  labs(title = "Bfkg as function of Sc")

ggplot(roots, aes(x = Sc, y = mract)) + 
  geom_point() + 
  theme_bw() +
  xlab('Sc') + 
  ylab('mract')+
  geom_smooth(method = lm, se = FALSE, formula = y ~ x)+
  labs(title = "mract as function of Sc")

ggplot(wood, aes(x = Sc, y = mbt)) + 
  geom_point() + 
  theme_bw() +
  xlab('Sc') + 
  ylab('mbt')+
  geom_smooth(method = lm, se = FALSE, formula = y ~ x)+
  labs(title = "mbt as function of Sc")

#Med log-log transformation


ggplot(leafs_log, aes(x = Sc, y = Bfkg)) + 
  geom_point() + 
  theme_bw() +
  xlab('log(Sc)') + 
  ylab('log(Bfkg)')+
  geom_smooth(method = lm, se = FALSE, formula = y ~ x)+
  labs(title = "Bfkg as function of Sc")

ggplot(roots_log, aes(x = Sc, y = mract)) + 
  geom_point() + 
  theme_bw() +
  xlab('log(Sc)') + 
  ylab('log(mract)')+
  geom_smooth(method = lm, se = FALSE, formula = y ~ x)+
  labs(title = "mract as function of Sc")

ggplot(wood_log, aes(x = Sc, y = mbt)) + 
  geom_point() + 
  theme_bw() +
  xlab('log(Sc)') + 
  ylab('log(mbt)')+
  geom_smooth(method = lm, se = FALSE, formula = y ~ x)+
  labs(title = "mbt as function of Sc")

#Lineære modeller 

lm_leafs <- lm(Bfkg ~ Sc, data = leafs)
lm_roots <- lm(mract ~ Sc, data = roots)
lm_wood <- lm(mbt ~ Sc, data = wood)


#Lineære modeller af log-log

lm_leafs_log <- lm(Bfkg ~ Sc, data = leafs_log)
lm_roots_log <- lm(mract ~ Sc, data = roots_log)
lm_wood_log <- lm(mbt ~ Sc, data = wood_log)

#Lineære modeller af log-log uden intercept

lm_leafs_log <- lm(Bfkg ~ Sc - 1, data = leafs_log)
lm_roots_log <- lm(mract ~ Sc - 1, data = roots_log)
lm_wood_log <- lm(mbt ~ Sc - 1, data = wood_log)

#Residual plots

ggplot(lm_leafs, aes(x = lm_leafs$fitted.values, y = lm_leafs$residuals)) + 
  geom_point() + 
  theme_bw() +
  xlab('Fitted values') + 
  ylab('Residuals')+
  labs(title = "Residual plot")

ggplot(lm_roots, aes(x = lm_roots$fitted.values, y = lm_roots$residuals)) + 
  geom_point() + 
  theme_bw() +
  xlab('Fitted values') + 
  ylab('Residuals')+
  labs(title = "Resdiual plot")

ggplot(lm_wood, aes(x = lm_wood$fitted.values, y = lm_wood$residuals)) + 
  geom_point() + 
  theme_bw() +
  xlab('Fitted values') + 
  ylab('Residuals')+
  labs(title = "Resdiual plot")


#Residual plots

ggplot(lm_leafs_log, aes(x = lm_leafs_log$fitted.values, y = lm_leafs_log$residuals)) + 
  geom_point() + 
  theme_bw() +
  xlab('Fitted values') + 
  ylab('Residuals')+
  geom_smooth(method = lm, se = FALSE, formula = y ~ x)+
  labs(title = "Residual plot")
       
ggplot(lm_roots_log, aes(x = lm_roots_log$fitted.values, y = lm_roots_log$residuals)) + 
  geom_point() + 
  theme_bw() +
  xlab('Fitted values') + 
  ylab('Residuals')+
  labs(title = "Resdiual plot")

ggplot(lm_wood_log, aes(x = lm_wood_log$fitted.values, y = lm_wood_log$residuals)) + 
  geom_point() + 
  theme_bw() +
  xlab('Fitted values') + 
  ylab('Residuals')+
  labs(title = "Resdiual plot")
       
#Standardized residual plots       

ggplot(lm_leafs_log, aes(x = lm_leafs_log$fitted.values, y = rstandard(lm_leafs_log)) + 
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

# Histogram over fordelingen af residualerne

#x <- residuals(lm_koen)/sd(residuals(lm_koen))


#ggplot(data = tibble(x=x))+
#  geom_histogram(aes(x=x, y=..density..), color = "white", bins = 8)+
#  labs(title = "Histogram over fordelingen af residualerne")+
#  geom_function(fun = dnorm, color = "red")









