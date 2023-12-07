##############################################################################
###Indlæsning af pakker og data:
library(randomForest)
library(quantregForest)
library(tidyverse)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(xtable)

set.seed(777)
leafs <- read.csv('Data/leafs.csv')
wood<- read.csv('Data/wood.csv')
roots <- read.csv('Data/roots.csv')
##############################################################################


#----------------------Creating Bootstrap function---------------------------

#Model skal spytte et alpha og et beta estimat ud.
  
boot_leafs <- bootstrap_loo(model_logolsB, leafs, 100, alpha = 0.2)
boot_wood <- bootstrap_loo(model_logolsB, wood, 100, alpha = 0.2)
boot_roots <- bootstrap_loo(model_logolsB, roots, 100, alpha = 0.2) 

plot_maker(boot_leafs, "Leafs")
plot_maker(boot_wood, "Wood")
plot_maker(boot_roots, "Roots")

coverage(boot_leafs)
coverage(boot_roots)

#Distribution of coverage

boot_leafs_rs <- rs_cov_boot(data = leafs, k = 10, alpha = 0.2, pred_int_maker = bootstrap, model = model_logolsB)
boot_wood_rs <- rs_cov(leafs, 10, 0.2, bootstrap(model_logolsB, wood, 100, alpha = 0.2))
boot_roots_rs <- rs_cov(leafs, 10, 0.2, bootstrap(model_logolsB, roots, 100, alpha = 0.2))

rs_plot_maker(boot_leafs_rs, "Leafs", alpha = 0.2)
rs_plot_maker(boot_wood_rs, "Wood", alpha = 0.2)
rs_plot_maker(boot_roots_rs, "Roots", alpha = 0.2)

#Conditional coverage

roll_cov(boot_leafs, alpha = 0.2, bin_size = 50, "Leafs")
roll_cov(boot_leafs, alpha = 0.2, bin_size = 50, "Leafs")




