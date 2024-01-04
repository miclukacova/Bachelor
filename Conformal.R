library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(infer)
library(foreign)
library(xtable)
library(stargazer)
library(randomForest)
library(quantregForest)

leafs <- read.csv('Data/leafs.csv')
roots <- read.csv('Data/roots.csv')
wood <- read.csv('Data/wood.csv')


#Log-log ols--------------------------------------------------------

#Leafs

#Score 2
set.seed(4)
loo_adj <- loo_pred_int(leafs, alpha = 0.2, s2_logolsb_conf) 
loo <- loo_pred_int(leafs, alpha = 0.2, s2_logols_conf)

plot_maker(loo_adj[[1]], "Leafs - logOLSB", ols_log_adj_l)
plot_maker(loo[[1]], "Leafs")

#Score 1
set.seed(4)
loo2_adj <- loo_pred_int(leafs, alpha = 0.2, s1_logolsb_conf) 
plot_maker(loo2_adj[[1]], "Leafs", ols_log_adj_l)

#Wood

#Score 2
set.seed(4)
loo_adj_w <- loo_pred_int(wood, alpha = 0.2, s2_logolsb_conf) 
loo_w <- loo_pred_int(wood, alpha = 0.2, s2_logols_conf)

plot_maker(loo_adj_w[[1]], "Wood - logOLSB", ols_log_adj_w)
plot_maker(loo_w[[1]], "Wood", ols_log_w)

#Score 1
set.seed(4)
loo2_adj_w <- loo_pred_int(wood, alpha = 0.2, s1_logolsb_conf) 
plot_maker(loo2_adj_w[[1]], "Wood", ols_log_adj_w)


#Roots

#Score 2
set.seed(4)
loo_adj_r <- loo_pred_int(roots, alpha = 0.2, s2_logolsb_conf) 
loo_r <- loo_pred_int(roots, alpha = 0.2, s2_logols_conf)

plot_maker(loo_adj_r[[1]], "Roots", ols_log_adj_r)
plot_maker(loo_r[[1]], "Roots", ols_log_r) 
head(loo_r)

#Score 1
set.seed(4)
loo2_adj_r <- loo_pred_int(roots, alpha = 0.2, s1_logolsb_conf) 
plot_maker(loo2_adj_r[[1]], "Roots", ols_log_adj_r)


#----------------------------Checking coverage for different alphas-----------------------------------------
alphas <- c(0.05, 0.1, 0.2, 0.3)

set.seed(4)
cov_alpha_l <- diff_alohas(leafs, s2_logolsb_conf, k = 5)
cov_alpha_w <- diff_alohas(wood, s2_logolsb_conf, k = 5)
cov_alpha_r <- diff_alohas(roots, s2_logolsb_conf, k = 5)

xtable(tibble("Signif. level" = alphas, "Leafs" = cov_alpha_l, 
              "Wood" = cov_alpha_w, "Roots" = cov_alpha_r))


#----------------------------Distribution of coverage by resampling-----------------------------------------

#Checking for correct coverage

set.seed(4)
a <- rs_cov(data = leafs, k = 50, alpha = 0.2, pred_int_maker = s2_logolsb_conf)
b <- rs_cov(data = wood, k = 50, alpha = 0.2, pred_int_maker = s2_logolsb_conf)
c <- rs_cov(data = roots, k = 50, alpha = 0.2, pred_int_maker = s2_logolsb_conf)

#Mean coverage:
mean_a <- mean(a$Coverage)
mean_b <- mean(b$Coverage)
mean_c <- mean(c$Coverage)

median(b$Coverage)

xtable(tibble(Data = c("Leafs", "Wood", "Roots"), 
              "Mean coverage" =c(mean_a, mean_b, mean_c)), type = latex)

rs_plot_maker(a, "Leafs - logOLSB", 0.2, conformal = TRUE, n = nrow(leafs))
rs_plot_maker(b, "Wood - logOLSB", 0.2, conformal = TRUE, n = nrow(wood))
rs_plot_maker(c, "Roots", 0.2, conformal = TRUE, n = nrow(roots))


#----------------------------Rolling coverage---------------------------------------------

set.seed(4)

leafs_pred_int <- loo_pred_int(leafs, alpha = 0.2, s2_logolsb_conf)
wood_pred_int <- loo_pred_int(wood, alpha = 0.2, s2_logolsb_conf)
roots_pred_int <- loo_pred_int(roots, alpha = 0.2, s2_logolsb_conf)

roll_cov(pred_int = leafs_pred_int, title = "Leafs - logOLSB")
roll_cov(pred_int = wood_pred_int, title = "Wood - logOLSB")
roll_cov(pred_int = roots_pred_int, title = "Roots", bin_size = 5)

####################################################################
#Diagnostics for logOLSB WITH SD-score:
###############################################################3####

#Checking coverage:
loo2_adj[[2]]
loo2_adj_r[[2]]
loo2_adj_w[[2]]


#----------------------------Checking coverage for different alphas-----------------------------------------
alphas <- c(0.05, 0.1, 0.3, 0.4)

set.seed(4)
cov_alpha_l <- diff_alohas(leafs, s1_logolsb_conf, k = 5)
cov_alpha_w <- diff_alohas(wood, s1_logolsb_conf, k = 5)
cov_alpha_r <- diff_alohas(roots, s1_logolsb_conf, k = 5)

xtable(tibble("Signif. level" = alphas, "Leafs" = cov_alpha_l, 
              "Wood" = cov_alpha_w, "Roots" = cov_alpha_r))


#----------------------------Distribution of coverage by resampling-----------------------------------------

#Checking for correct coverage

set.seed(4)
a <- rs_cov(data = leafs, k = 50, alpha = 0.2, pred_int_maker = s1_logolsb_conf)
b <- rs_cov(data = wood, k = 50, alpha = 0.2, pred_int_maker = s1_logolsb_conf)
c <- rs_cov(data = roots, k = 50, alpha = 0.2, pred_int_maker = s1_logolsb_conf)

#Mean coverage:
mean_a <- mean(a$Coverage)
mean_b <- mean(b$Coverage)
mean_c <- mean(c$Coverage)

median(b$Coverage)

xtable(tibble(Data = c("Leafs", "Wood", "Roots"), 
              "Mean coverage" =c(mean_a, mean_b, mean_c)), type = latex)

rs_plot_maker(a, "Leafs", 0.2, conformal = TRUE, n = nrow(leafs))
rs_plot_maker(b, "Wood", 0.2, conformal = TRUE, n = nrow(wood))
rs_plot_maker(c, "Roots", 0.2, conformal = TRUE, n = nrow(roots))


#----------------------------Rolling coverage---------------------------------------------

set.seed(4)

leafs_pred_int <- loo_pred_int(leafs, alpha = 0.2, s1_logolsb_conf)
wood_pred_int <- loo_pred_int(wood, alpha = 0.2, s1_logolsb_conf)
roots_pred_int <- loo_pred_int(roots, alpha = 0.2, s1_logolsb_conf)

roll_cov(pred_int = leafs_pred_int, title = "Leafs")
roll_cov(pred_int = wood_pred_int, title = "Wood")
roll_cov(pred_int = roots_pred_int, title = "Roots", bin_size = 5)







#NLR----------------------------------------------------------------------------

#----------------------------Pred intervaller###################################

set.seed(4)

loo_l_NLR <- loo_pred_int(leafs, alpha = 0.2, pred_int_nlr_l) 
loo_w_NLR <- loo_pred_int(wood, alpha = 0.2, pred_int_nlr_w)
loo_r_NLR <- loo_pred_int(roots, alpha = 0.2, pred_int_nlr_r)


plot_maker(loo_l_NLR[[1]], "Leafs - NLR", nlr_l)
plot_maker(loo_w_NLR[[1]], "Woods - NLR", nlr_w)
plot_maker(loo_r_NLR[[1]], "Roots", nlr_r)


#----------------------------Distribution of coverage by resampling-----------------------------------------

#Checking for correct coverage

set.seed(4)
a <- rs_cov(data = leafs, k = 50, alpha = 0.2, pred_int_maker = pred_int_nlr_l)
b <- rs_cov(data = wood, k = 50, alpha = 0.2, pred_int_maker = pred_int_nlr_w)
c <- rs_cov(data = roots, k = 50, alpha = 0.2, pred_int_maker = pred_int_nlr_r)

#Mean coverage:
mean_a <- mean(a$Coverage)
mean_b <- mean(b$Coverage)
mean_c <- mean(c$Coverage)

median(b$Coverage)

xtable(tibble(Data = c("Leafs", "Wood", "Roots"), 
              "Mean coverage" =c(mean_a, mean_b, mean_c)), type = latex)

rs_plot_maker(a, "Leafs - NLR", 0.2, conformal = TRUE, n = nrow(leafs))
rs_plot_maker(b, "Wood - NLR", 0.2, conformal = TRUE, n = nrow(wood))
rs_plot_maker(c, "Roots", 0.2, conformal = TRUE, n = nrow(roots))

#For different alphas:
alphas <- c(0.05, 0.1, 0.2,0.3)

set.seed(4)
cov_alpha_l <- diff_alohas(leafs, pred_int_nlr_l, k = 5)
cov_alpha_w <- diff_alohas(wood, pred_int_nlr_w, k = 5)
cov_alpha_r <- diff_alohas(roots, pred_int_nlr_r, k = 5)

xtable(tibble("Signif. level" = alphas, "Leafs" = cov_alpha_l, 
              "Wood" = cov_alpha_w, "Roots" = cov_alpha_r))


#----------------------------Rolling coverage---------------------------------------------

set.seed(4)

leafs_pred_int <- loo_pred_int(leafs, alpha = 0.2, pred_int_nlr_l)
wood_pred_int <- loo_pred_int(wood, alpha = 0.2, pred_int_nlr_w)
roots_pred_int <- loo_pred_int(roots, alpha = 0.2, pred_int_nlr_r)

roll_cov(pred_int = leafs_pred_int, title = "Leafs - NLR")
roll_cov(pred_int = wood_pred_int, title = "Wood - NLR")
roll_cov(pred_int = roots_pred_int, title = "Roots - NLR", bin_size = 5)




#Regression forest--------------------------------------------------------------
#----------------------------Pred intervaller-----------------------------------

set.seed(4)
loo_l <- loo_pred_int(leafs, pred_int = pred_int_rf_l)
loo_w <- loo_pred_int(wood, pred_int = pred_int_rf_w)
loo_r <- loo_pred_int(roots, pred_int = pred_int_rf_r)

xtable(tibble(" " = c("Leafs", "Wood", "Roots"), 
              "Covergae" = c(loo_l[[2]], loo_w[[2]], loo_r[[2]])), type = latex)

plot_maker(loo_l[[1]],"Leafs - RF")
plot_maker(loo_w[[1]],"Wood - RF")
plot_maker(loo_r[[1]],"Roots")

#----------------------------Checking coverage for different alphas-----------------------------------------
alphas <- c(0.05, 0.1, 0.2, 0.3)

set.seed(4)
cov_alpha_l <- diff_alohas(leafs, pred_int_rf_l, k = 5)
cov_alpha_w <- diff_alohas(wood, pred_int_rf_w, k = 5)
cov_alpha_r <- diff_alohas(wood, pred_int_rf_r, k = 5)

xtable(tibble("Signif. level" = alphas, "Leafs" = cov_alpha_l, 
              "Wood" = cov_alpha_w))


#----------------------------Distribution of coverage by resampling-----------------------------------------

#Checking for correct coverage

set.seed(4)
a <- rs_cov(data = leafs, k = 50, alpha = 0.2, pred_int_maker = pred_int_rf_l)
b <- rs_cov(data = wood, k = 50, alpha = 0.2, pred_int_maker = pred_int_rf_w)

#Mean coverage:
mean_a <- mean(a$Coverage)
mean_b <- mean(b$Coverage)

median(b$Coverage)

xtable(tibble(Data = c("Leafs", "Wood"), 
              "Mean coverage" =c(mean_a, mean_b)), type = latex)

rs_plot_maker(a, "Leafs - RF", 0.2, conformal = TRUE, n = nrow(leafs)) 
rs_plot_maker(b, "Wood - RF", 0.2, conformal = TRUE, n = nrow(wood)) 
 

#----------------------------Rolling coverage---------------------------------------------

roll_cov(pred_int = loo_l, title = "Leafs - RF")
roll_cov(pred_int = loo_w, title = "Wood - RF")
roll_cov(pred_int = loo_r, title = "Roots", bin_size = 5)

####################################################################
#Conformalised quantile regression forest---------------------------------------
####################################################################

set.seed(4)
#loo_l <- loo_pred_int(leafs, pred_int = pred_int_qrf_l)
#loo_w <- loo_pred_int(wood, pred_int = pred_int_qrf_w)
#Den her mangler at køre
#loo_r <- loo_pred_int(roots, pred_int = pred_int_qrf_r)


#write.csv(loo_l, "/Users/michaelalukacova/Bachelor1/Data/loo_l_conf_qr.csv", row.names=F)
#write.csv(loo_w, "/Users/michaelalukacova/Bachelor1/Data/loo_w_conf_qr.csv", row.names=F)
#write.csv(loo_r, "/Users/michaelalukacova/Bachelor1/Data/loo_r_conf_qr.csv", row.names=F)

loo_l <- read.csv('Data//loo_l_conf.csv')
loo_w <- read.csv('Data/loo_w_conf.csv')
#loo_r <- read.csv('Data/loo_r_conf.csv')


xtable(tibble(" " = c("Leafs", "Wood"), 
              "Covergae" = c(loo_l[[2]], loo_w[[1]])), type = latex)

plot_maker(loo_l,"Leafs")
mean(loo_l$Low <= loo_l$Kgp & loo_l$High >= loo_l$Kgp)
plot_maker(loo_w[[1]], "Wood")

#----------------------------Checking coverage for different alphas-----------------------------------------
alphas <- c(0.05, 0.1, 0.2, 0.3)

set.seed(2)
cov_alpha_l <- diff_alohas(leafs, pred_int_qrf_l)
cov_alpha_w <- diff_alohas(wood, pred_int_qrf_w)

xtable(tibble("Signif. level" = alphas, "Leafs" = cov_alpha_l, 
              "Wood" = cov_alpha_w))


#----------------------------Distribution of coverage by resampling-----------------------------------------

#Checking for correct coverage

set.seed(4)
a <- rs_cov(data = leafs, k = 50, alpha = 0.2, pred_int_maker = pred_int_qrf_l)
b <- rs_cov(data = wood, k = 50, alpha = 0.2, pred_int_maker = pred_int_qrf_w)

#Mean coverage:
mean_a <- mean(a$Coverage)
mean_b <- mean(b$Coverage)

median(b$Coverage)

xtable(tibble(Data = c("Leafs", "Wood"), 
              "Mean coverage" =c(mean_a, mean_b)), type = latex)

rs_plot_maker(a, "Leafs", 0.2, conformal = TRUE, n = nrow(leafs))
rs_plot_maker(b, "Wood", 0.2, conformal = TRUE, n = nrow(wood))
rs_plot_maker(c, "Roots", 0.2)

#----------------------------Rolling coverage---------------------------------------------

roll_cov(pred_int = loo_l, title = "Leafs")
roll_cov(pred_int = loo_w, title = "Wood")

