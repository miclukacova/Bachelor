#Bootstrap

library(tidyverse)
library(readr)
library(infer)
library(ggplot2)

##Indlæsning af data

leafs_log <- read.csv('Data/leafs_log.csv')
roots_log <- read.csv('Data/roots_log.csv')
wood_log <- read.csv('Data/wood_log.csv')

##Bootstrap

B <- 1000

boot_leafs <- leafs_log %>% rep_sample_n(replace = TRUE, reps = B, size = nrow(leafs_log))
boot_roots <- roots_log %>% rep_sample_n(replace = TRUE, reps = B, size = nrow(roots_log))
boot_wood <- wood_log %>% rep_sample_n(replace = TRUE, reps = B, size = nrow(wood_log))

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

##Fordeling af estimater

ggplot(data = lm_wood_coefs)+
  geom_histogram(aes(x=a, y=..density..), color = "white")+
  labs(title = "Histogram over the a coefficient")

ggplot(data = lm_wood_coefs)+
  geom_histogram(aes(x=b, y=..density..), color = "white")+
  labs(title = "Histogram over the b coefficient")

##Konfidens intervaller - percentil:

#a

tibble(
  Data = c("Leafs", "Roots", "Wood"), 
  Lower = c(quantile(lm_leafs_coefs$a, 0.025), quantile(lm_roots_coefs$a, 0.025), quantile(lm_wood_coefs$a, 0.025)),
  Upper = c(quantile(lm_leafs_coefs$a, 0.975), quantile(lm_roots_coefs$a, 0.975), quantile(lm_wood_coefs$a, 0.975)),
)


#b

tibble(
  Data = c("Leafs", "Roots", "Wood"), 
  Lower = c(quantile(lm_leafs_coefs$b, 0.025), quantile(lm_roots_coefs$b, 0.025), quantile(lm_wood_coefs$b, 0.025)),
  Upper = c(quantile(lm_leafs_coefs$b, 0.975), quantile(lm_roots_coefs$b, 0.975), quantile(lm_wood_coefs$b, 0.975)),
)

##Det giver rigtig god mening baseret på antallet af observerede data punkter:

nrow(roots_log)
nrow(wood_log)
nrow(leafs_log)

##Konfidens intervaller - standard error (bare for leafs):

theta_hat_a <- c(lm(Bfkg ~ Sc, leafs_log)$coefficients[1],
                 lm(mract ~ Sc, roots_log)$coefficients[1],
                 lm(mbt ~ Sc, wood_log)$coefficients[1])


theta_hat_b <-  c(lm(Bfkg ~ Sc, leafs_log)$coefficients[2],
                  lm(mract ~ Sc, roots_log)$coefficients[2],
                  lm(mbt ~ Sc, wood_log)$coefficients[2])

se_b_a <- c(sqrt(1/(B-1)*(sum(theta_hat_a[1] - lm_leafs_coefs$a)^2)),
            sqrt(1/(B-1)*(sum(theta_hat_a[2] - lm_roots_coefs$a)^2)),
            sqrt(1/(B-1)*(sum(theta_hat_a[3] - lm_wood_coefs$a)^2)))


se_b_b <- c(sqrt(1/(B-1)*(sum(theta_hat_b[1] - lm_leafs_coefs$b)^2)),
            sqrt(1/(B-1)*(sum(theta_hat_b[2] - lm_roots_coefs$b)^2)),
            sqrt(1/(B-1)*(sum(theta_hat_b[3] - lm_wood_coefs$b)^2)))

upper_a <- theta_hat_a-qt(0.025, nrow(leafs_log)-1)*se_b_a
upper_b <- theta_hat_b-qt(0.025, nrow(leafs_log)-1)*se_b_b

lower_a <- theta_hat_a+qt(0.025, nrow(leafs_log)-1)*se_b_a
lower_b <- theta_hat_b+qt(0.025, nrow(leafs_log)-1)*se_b_b

#a

tibble(
  Data = c("Leafs", "Roots", "Wood"), 
  Lower = c(lower_a[1], lower_a[2], lower_a[3]),
  Upper = c(upper_a[1], upper_a[2], upper_a[3]),
)

#b

tibble(
  Data = c("Leafs", "Roots", "Wood"), 
  Lower = c(lower_b[1], lower_b[2], lower_b[3]),
  Upper = c(upper_b[1], upper_b[2], upper_b[3]))



