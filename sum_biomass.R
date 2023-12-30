#################################--Indlæsning af pakker og data---###############################

leafs <- read.csv('Data/leafs.csv')
roots <- read.csv('Data/roots.csv')
wood <- read.csv('Data/wood.csv')

library(tidyverse)
library(readr)
library(infer)
library(foreign)
library(xtable)
library(stargazer)


################################################################################
## ----------------------- Skabelse af sum data ----------------------------- ##
################################################################################


part_func <- function(data) {
  group <- sample(rep(1:150, length.out = nrow(data)))
  for (i in (1:49)){
    if (i == 1){
      new_data <- tibble(Group = 1, Sc = sum(data[group == 1, 1]), Kgp = sum(data[group == 1, 2]))
    }
    else {
      new_data <- new_data %>% add_row(Group = i,Sc = sum(data[group == i, 1]), 
                                       Kgp = sum(data[group == i, 2]))
    }
  }
  for (i in (50:69)){
    if (i %% 2 == 0){
      new_data <- new_data %>% add_row(Group = i,Sc = sum(data[(group == i | group == i+1), 1]), 
                                       Kgp = sum(data[(group == i | group == i+1), 2]))
    }
  }
  for (i in (70:100)){
    if (i %% 3 == 0){
      new_data <- new_data %>% add_row(Group = i,Sc = sum(data[(group == i | group == i-1 | group == i-2), 1]), 
                                       Kgp = sum(data[(group == i | group == i-1 | group == i-2), 2]))
    }
  }
}

leafs 



#Ide

#Sæt tal på alle obs, fra 1 til 150. Læg Alle 1'ere sammen, alle 2'ere osv.
# 50+51, 52+54, ...
#Læg derefter 70, 71 og 72 sammen, 73, 74, og 75, osv...
# Læg 100-104 sammen, 105-109 sammen, 110-114, 115-124, 125-134,135-150.  


################################################################################
## ------------------------------ sum model OLS ----------------------------- ##
################################################################################

lm_leafs <- lm(Kgp ~ Sc, data = leafs)
lm_wood <- lm(Kgp ~ Sc, data = wood)
lm_roots <- lm(Kgp ~ Sc, data = roots)

hat_beta <- c(lm_leafs$coefficients[[1]],
              lm_wood$coefficients[[1]],
              lm_roots$coefficients[[1]])

hat_alpha <- c(lm_leafs$coefficients[[2]],
               lm_wood$coefficients[[2]],
               lm_roots$coefficients[[2]])

var_hat <- c(var(lm_leafs$residuals),
               var(lm_wood$residuals),
               var(lm_roots$residuals))



sum_leafs <- function(x) hat_alpha[1] * sum(x) + length(x)*hat_beta[1]
sum_wood <- function(x) hat_alpha[2] * sum(x) + length(x)*hat_beta[2]
sum_roots <- function(x) hat_alpha[3] * sum(x) + length(x)*hat_beta[3]

sum_biomass <- function(x,y,z) sum_leafs(x) + sum_wood(y) + sum_roots(z)

#Plot predikteret ifht. faktisk


# Tag evt. kvantiler af fordelingen ???


#
