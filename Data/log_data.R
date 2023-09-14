library(readODS)
library(tidyverse)
library(ggplot2)

leafs = read_ods('Data/leafs.ods')
roots = read_ods('Data/roots.ods')
wood = read_ods('Data/wood.ods')

colnames(roots)[1] = c("Sc")
colnames(leafs)[2] = c("Kgp")
colnames(roots)[2] = c("Kgp")
colnames(wood)[2] = c("Kgp")

leafs_log = leafs %>% mutate(Sc = log(Sc), Kgp = log(Kgp))
roots_log = roots %>% mutate(Sc = log(Sc), Kgp = log(Kgp))
wood_log = wood %>% mutate(Sc = log(Sc), Kgp = log(Kgp))

write.csv(leafs, "/Users/michaelalukacova/Bachelor1/Data/leafs.csv", row.names=FALSE)
write.csv(roots, "/Users/michaelalukacova/Bachelor1/Data/roots.csv", row.names=FALSE)
write.csv(wood, "/Users/michaelalukacova/Bachelor1/Data/wood.csv", row.names=FALSE)
write.csv(leafs_log, "/Users/michaelalukacova/Bachelor1/Data/leafs_log.csv", row.names=FALSE)
write.csv(roots_log, "/Users/michaelalukacova/Bachelor1/Data/roots_log.csv", row.names=FALSE)
write.csv(wood_log, "/Users/michaelalukacova/Bachelor1/Data/wood_log.csv", row.names=FALSE)