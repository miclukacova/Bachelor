#Bootstrap

library(readODS)
library(tidyverse)
library(readr)
library(infer)

##Indlæsning af data

leafs_log <- read.csv('Data/leafs_log.csv')
roots_log <- read.csv('Data/roots_log.csv')
wood_log <- read.csv('Data/wood_log.csv')

##Bootstrap

boot_leafs <- leafs_log %>% rep_sample_n(replace = TRUE, reps = 100, size = nrow(leafs_log))
boot_roots <- roots_log %>% rep_sample_n(replace = TRUE, reps = 100, size = nrow(roots_log))
boot_wood <- wood_log %>% rep_sample_n(replace = TRUE, reps = 100, size = nrow(wood_log))

## Beregening af parametre ved OLS

lm_leafs_coefs <- boot_leafs %>%
  group_by(replicate) %>%
  summarize(a = lm(Bfkg ~ Sc)$coefficients[1], b = lm(Bfkg ~ Sc)$coefficients[2])

lm_roots_coefs <- boot_roots %>%
  group_by(replicate) %>%
  summarize(a = lm(mract ~ Sc)$coefficients[1], b = lm(mract ~ Sc)$coefficients[2])

lm_wood_coefs <- boot_wood %>%
  group_by(replicate) %>%
  summarize(a = lm(mbt ~ Sc)$coefficients[1], b = lm(mbt ~ Sc)$coefficients[2])

##Konfidens intervaller

#Undersøg lige de forskellige måder at opstille konfidensintervaller på: 

tibble(
  Data = c("Leafs", "Roots", "Wood"), 
  Lower = c(quantile(lm_leafs_coefs$a, 0.025), quantile(lm_roots_coefs$a, 0.025), quantile(lm_wood_coefs$a, 0.025)),
  Upper = c(quantile(lm_leafs_coefs$a, 0.975), quantile(lm_roots_coefs$a, 0.975), quantile(lm_wood_coefs$a, 0.975)),
)

tibble(
  Data = c("Leafs", "Roots", "Wood"), 
  Lower = c(quantile(lm_leafs_coefs$b, 0.025), quantile(lm_roots_coefs$b, 0.025), quantile(lm_wood_coefs$b, 0.025)),
  Upper = c(quantile(lm_leafs_coefs$b, 0.975), quantile(lm_roots_coefs$b, 0.975), quantile(lm_wood_coefs$b, 0.975)),
)


##Det giver rigtig god mening baseret på antallet af observerede data punkter:

nrow(roots)
nrow(wood)
nrow(leafs)



