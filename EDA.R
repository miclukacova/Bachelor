library(readODS)
library(tidyverse)
library(ggplot2)

leafs = read_ods('Data/leafs.ods')
roots = read_ods('Data/roots.ods')
wood = read_ods('Data/wood.ods')

colnames(roots)[1] = c("Sc")

head(leafs)
head(roots)
head(wood)

leafs_log = leafs %>% mutate(Sc = log(Sc), Bfkg = log(Bfkg))
roots_log = roots %>% mutate(Sc = log(Sc), mract = log(mract))
wood_log = wood %>% mutate(Sc = log(Sc), mbt = log(mbt))


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

#Lineære modeller af log-log

lm_leafs_log <- lm(Bfkg ~ Sc, data = leafs_log)
lm_roots_log <- lm(mract ~ Sc, data = roots_log)
lm_wood_log <- lm(mbt ~ Sc, data = wood_log)

#Lineære modeller af log-log uden intercept

lm_leafs_log <- lm(Bfkg ~ Sc - 1, data = leafs_log)
lm_roots_log <- lm(mract ~ Sc - 1, data = roots_log)
lm_wood_log <- lm(mbt ~ Sc - 1, data = wood_log)

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
  geom_smooth(method = lm, se = FALSE, formula = y ~ x)+
  labs(title = "Resdiual plot")

ggplot(lm_wood_log, aes(x = lm_wood_log$fitted.values, y = lm_wood_log$residuals)) + 
  geom_point() + 
  theme_bw() +
  xlab('Fitted values') + 
  ylab('Residuals')+
  geom_smooth(method = lm, se = FALSE, formula = y ~ x)+
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








