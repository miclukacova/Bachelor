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
  geom_histogram(aes(x=Kgp, y=..density..), color = "white", bins = 10)+
  theme_bw()+
  labs(title = "Distribution of leafs biomass")

b <- ggplot(data = roots)+
  geom_histogram(aes(x=Kgp, y=..density..), color = "white", bins = 10)+
  theme_bw()+
  labs(title = "Distribution of root biomass")

c <- ggplot(data = wood)+
  geom_histogram(aes(x=Kgp, y=..density..), color = "white", bins = 10)+
  theme_bw()+
  labs(title = "Distribution of wood biomass")

ggarrange(a,b,c, ncol = 3, nrow = 1)

#Histogrammer over fordelingen af biomasse efter log transformationen

a1 <- ggplot(data = leafs_log)+
  geom_histogram(aes(x=Kgp, y=..density..), color = "white", bins = 10)+
  theme_bw()+
  labs(title = "Distribution of log leafs biomass")

b1 <- ggplot(data = roots_log)+
  geom_histogram(aes(x=Kgp, y=..density..), color = "white", bins = 10)+
  theme_bw()+
  labs(title = "Distribution of log root biomass")

c1 <- ggplot(data = wood_log)+
  geom_histogram(aes(x=Kgp, y=..density..), color = 'darkolivegreen',fill = 'darkolivegreen3', bins = 15)+
  theme_bw()+
  labs(title = "Distribution of log wood biomass")

ggarrange(a1,b1,c1, ncol = 3, nrow = 1)


#Uden log-log transformationer

leafs_sc_plot <- ggplot(leafs, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'darkolivegreen',fill = 'darkolivegreen3', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab(bquote('Crown area'~(m^2/plant))) + 
  ylab('Foliage dry mass (kg/plant)')+
  geom_smooth(method = lm, se = FALSE, formula = y ~ x, color = 'hotpink')+
  labs(title = "Foliage")

roots_sc_plot <- ggplot(roots, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'darkolivegreen',fill = 'darkolivegreen4', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab(bquote('Crown area'~(m^2/plant))) + 
  ylab('Root dry mass (kg/plant)')+
  geom_smooth(method = lm, se = FALSE, formula = y ~ x, color = 'hotpink')+
  labs(title = "Roots")

wood_sc_plot <- ggplot(wood, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'darkolivegreen',fill = 'darkolivegreen3', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab(bquote('Crown area'~(m^2/plant))) + 
  ylab('Wood dry mass (kg/plant)')+
  geom_smooth(method = lm, se = FALSE, formula = y ~ x, color = 'hotpink')+
  labs(title = "Wood")

ggarrange(leafs_sc_plot, roots_sc_plot, wood_sc_plot, ncol = 3, nrow = 1)

#Med log-log transformation


ggplot(leafs_log, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'darkolivegreen', fill = 'darkolivegreen3', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab(bquote(log('Crown area'~m^2/plant))) + 
  ylab('log(Dry mass (kg/plant))')+
  geom_smooth(method = lm, se = FALSE, formula = y ~ x, color = 'hotpink')+
  labs(title = "Foliage")

ggplot(roots_log, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'darkolivegreen', fill = 'darkolivegreen4', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab(bquote(log('Crown area'~m^2/plant))) + 
  ylab('log(Dry mass (kg/plant))')+
  geom_smooth(method = lm, se = FALSE, formula = y ~ x, color = 'hotpink')+
  labs(title = "Roots")

ggplot(wood_log, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'darkolivegreen',fill = 'darkolivegreen3', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab(bquote(log('Crown area'~m^2/plant))) + 
  ylab('log(Dry mass (kg/plant))')+
  geom_smooth(method = lm, se = FALSE, formula = y ~ x, color = 'hotpink')+
  labs(title = "Wood")

#Lineære modeller 

lm_leafs <- lm(Kgp ~ Sc, data = leafs)
lm_roots <- lm(Kgp ~ Sc, data = roots)
lm_wood <- lm(Kgp ~ Sc, data = wood)


#Lineære modeller af log-log

lm_leafs_log <- lm(Kgp ~ Sc, data = leafs_log)
lm_roots_log <- lm(Kgp ~ Sc, data = roots_log)
lm_wood_log <- lm(Kgp ~ Sc, data = wood_log)

#Lineære modeller af log-log uden intercept

lm_leafs_log <- lm(Kgp ~ Sc - 1, data = leafs_log)
lm_roots_log <- lm(Kgp ~ Sc - 1, data = roots_log)
lm_wood_log <- lm(Kgp ~ Sc - 1, data = wood_log)

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









