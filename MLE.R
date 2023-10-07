library(tidyverse)
library(readr)
library(infer)
library(foreign)
library(xtable)
library(stargazer)
leafs_train <- read.csv('Data/train_leafs.csv')

logxi <- log(leafs_train$Sc)
logyi <- log(leafs_train$Kgp)
n <- nrow(leafs_train)


alpha1 <- sum(logxi*logyi-1/n*sum(logyi)*logxi)/sum(logxi^2-1/(n)*sum(logxi)*logxi)
log_beta <- exp(1/n * sum(logyi-alpha1*logxi))

f_hat <- function(x) exp(log_beta)*x^(alpha1)

ggplot(leafs_train, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'pink', fill = 'hotpink', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab(bquote('Crown area'~m^2/plant)) + 
  ylab('Dry mass (kg/plant)')+
  geom_function(fun = f_hat, colour = "darkolivegreen")+
  labs(title = "Wood")

