#########################################################################################################3
#An attempt at fitting a model with splines

leafs <- read.csv('Data/leafs.csv')
wood <- read.csv('Data/wood.csv')
roots <- read.csv('Data/roots.csv')

library(splines)
library(ggplot2)

#Fitting a simple lm but with a spline:
slm_leafs <- lm(Kgp ~ ns(Sc, df = 2), data = leafs)
slm_wood <- lm(Kgp ~ ns(Sc, df = 2), data = wood)
slm_roots <- lm(Kgp ~ ns(Sc, df = 2), data = roots)


cols <- c("darkolivegreen2","darkolivegreen")

ggplot(leafs, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'pink', fill = 'hotpink', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab(bquote('Crown area'~m^2/plant)) + 
  geom_line(aes(y=predict(slm_leafs)), color = "darkolivegreen")+
  ylab('Dry mass (kg/plant)')+
  labs(title = "Foliage")

ggplot(wood, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'pink', fill = 'hotpink', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab(bquote('Crown area'~m^2/plant)) + 
  ylab('log(Dry mass (kg/plant))')+
  geom_point(aes(y=predict(slm_wood)), color = "darkolivegreen")+
  labs(title = "Wood")

ggplot(roots, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'pink', fill = 'hotpink', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab(bquote(log('Crown area'~m^2/plant))) + 
  ylab('Dry mass (kg/plant)')+
  geom_(aes(y=predict(slm_roots)), color = "darkolivegreen")+
  labs(title = "Roots")
