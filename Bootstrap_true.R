##############################################################################
###Indlæsning af pakker og data:
library(randomForest)
library(quantregForest)
library(tidyverse)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(xtable)
library(infer)


set.seed(777)
leafs <- read.csv('Data/leafs.csv')
wood<- read.csv('Data/wood.csv')
roots <- read.csv('Data/roots.csv')


##############################################################################


#----------------------logOLS---------------------------
  
set.seed(4)

#boot_leafs <- bootstrap_loo(model_logols, leafs, 1000, alpha = 0.2)
#boot_wood <- bootstrap_loo(model_logols, wood, 1000, alpha = 0.2)
#boot_roots <- bootstrap_loo(model_logols, roots, 1000, alpha = 0.2) 

#write.csv(boot_leafs, "/Users/michaelalukacova/Bachelor1/Data/boot_leafs_logols.csv", row.names=F)
#write.csv(boot_wood, "/Users/michaelalukacova/Bachelor1/Data/boot_wood_logols.csv", row.names=F)
#write.csv(boot_roots, "/Users/michaelalukacova/Bachelor1/Data/boot_roots_logols.csv", row.names=F)

boot_leafs <- read.csv('Data/boot_leafs_logols.csv')
boot_wood <- read.csv('Data/boot_wood_logols.csv')
boot_roots <- read.csv('Data/boot_roots_logols.csv')

plot_maker(boot_leafs, "Leafs")
plot_maker(boot_wood, "Wood")
plot_maker(boot_roots, "Roots")

coverage(boot_leafs)
coverage(boot_wood)
coverage(boot_roots)

#Distribution of coverage

set.seed(4)

boot_leafs_rs <- rs_cov_boot(data = leafs, k = 50, alpha = 0.2, model = model_logols)
boot_wood_rs <- rs_cov_boot(data = wood, k = 50, alpha = 0.2, model = model_logols)
boot_roots_rs <- rs_cov_boot(data = roots, k = 50, alpha = 0.2, model = model_logols)

rs_plot_maker(boot_leafs_rs, "Leafs", alpha = 0.2)
rs_plot_maker(boot_wood_rs, "Wood", alpha = 0.2)
rs_plot_maker(boot_roots_rs, "Roots", alpha = 0.2)

#Conditional coverage

roll_cov_boot(boot_leafs, alpha = 0.2, bin_size = 50, "Leafs")
roll_cov_boot(boot_wood, alpha = 0.2, bin_size = 50, "Wood")
roll_cov_boot(boot_roots, alpha = 0.2, bin_size = 5, "Roots")

#Diff alphas

set.seed(4)
diff_alohas_boot(data = leafs, model = model_logols)
diff_alohas_boot(data = wood, model = model_logols)
diff_alohas_boot(data = roots, model = model_logols)

#----------------------NLR---------------------------

set.seed(4)

starting_point_leafs <- c(0.61,0.81)
starting_point_wood <- c(5.51,0.73)
starting_point_roots <- c(3.66,0.1)

model_NLR_leafs <- function(data) model_NLR(data, starting_point_leafs)
model_NLR_wood <- function(data) model_NLR(data, starting_point_wood)
model_NLR_roots <- function(data) model_NLR(data, starting_point_roots)

boot_leafs <- bootstrap_loo(model_NLR_leafs, leafs, 1000, alpha = 0.2)
boot_wood <- bootstrap_loo(model_NLR_wood, wood, 1000, alpha = 0.2)
boot_roots <- bootstrap_loo(model_NLR_roots, roots, 1000, alpha = 0.2)


write.csv(boot_leafs, "/Users/michaelalukacova/Bachelor1/Data/boot_leafs_NLR.csv", row.names=F)
write.csv(boot_wood, "/Users/michaelalukacova/Bachelor1/Data/boot_wood_NLR.csv", row.names=F)
write.csv(boot_roots, "/Users/michaelalukacova/Bachelor1/Data/boot_roots_NLR.csv", row.names=F)

boot_leafs <- read.csv('Data/boot_leafs_NLR.csv')
boot_wood <- read.csv('Data/boot_wood_NLR.csv')
boot_roots <- read.csv('Data/boot_roots_NLR.csv')

plot_maker(boot_leafs, "Leafs")
plot_maker(boot_wood, "Wood")
plot_maker(boot_roots, "Roots")

coverage(boot_leafs)
coverage(boot_wood)
coverage(boot_roots)

#Distribution of coverage

set.seed(4)

boot_leafs_rs <- rs_cov_boot(data = leafs, k = 50, alpha = 0.2, model = model_NLR_leafs)
boot_wood_rs <- rs_cov_boot(data = wood, k = 50, alpha = 0.2, model = model_NLR_wood)
boot_roots_rs <- rs_cov_boot(data = roots, k = 50, alpha = 0.2, model = model_NLR_roots)

rs_plot_maker(boot_leafs_rs, "Leafs", alpha = 0.2)
rs_plot_maker(boot_wood_rs, "Wood", alpha = 0.2)
rs_plot_maker(boot_roots_rs, "Roots", alpha = 0.2)

mean(boot_leafs_rs$Coverage)
mean(boot_wood_rs$Coverage)
mean(boot_roots_rs$Coverage)

#Conditional coverage

roll_cov_boot(boot_leafs, alpha = 0.2, bin_size = 50, "Leafs")
roll_cov_boot(boot_wood, alpha = 0.2, bin_size = 50, "Wood")
roll_cov_boot(boot_roots, alpha = 0.2, bin_size = 5, "Roots")


#Different alphas with k-fold cv instead og loocv:
set.seed(4)
cov_alpha_l <- diff_alohas_boot(data = leafs, model = model_NLR_leafs, B = 300, k = 5)
cov_alpha_w <- diff_alohas_boot(data = wood, model = model_NLR_wood, B = 300, k = 5)
cov_alpha_r <- diff_alohas_boot(data = roots, model = model_NLR_roots, B = 300, k = 5)


xtable(tibble("Signif. level" = alphas, "Leafs" = cov_alpha_l, 
              "Wood" = cov_alpha_w, "Roots" = cov_alpha_r))
