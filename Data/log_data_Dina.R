library(readODS)
library(tidyverse)

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

# Split Data 
sample_size_leafs = floor(0.8*nrow(leafs_log))
sample_size_roots = floor(0.8*nrow(roots_log))
sample_size_wood = floor(0.8*nrow(wood_log))

# Randomly split data
set.seed(777)
picked_leafs = sample(seq_len(nrow(leafs_log)),size = sample_size_leafs)
picked_roots = sample(seq_len(nrow(roots_log)),size = sample_size_roots)
picked_wood = sample(seq_len(nrow(wood_log)),size = sample_size_wood)


train_leafs_log = leafs_log[picked_leafs,]
test_leafs_log = leafs_log[-picked_leafs,]
train_roots_log = roots_log[picked_roots,]
test_roots_log = roots_log[-picked_roots,]
train_wood_log = wood_log[picked_wood,]
test_wood_log = wood_log[-picked_wood,]

train_leafs = leafs[picked_leafs,]
test_leafs = leafs[-picked_leafs,]
train_roots = roots[picked_roots,]
test_roots = roots[-picked_roots,]
train_wood = wood[picked_wood,]
test_wood = wood[-picked_wood,]

write.csv(leafs, "C:/Users/Bruger/OneDrive - University of Copenhagen/4. år/Bachelor/Bachelor1/Data/leafs.csv", row.names=FALSE)
write.csv(roots, "C:/Users/Bruger/OneDrive - University of Copenhagen/4. år/Bachelor/Bachelor1/Data/roots.csv", row.names=FALSE)
write.csv(wood, "C:/Users/Bruger/OneDrive - University of Copenhagen/4. år/Bachelor/Bachelor1/wood.csv", row.names=FALSE)
write.csv(leafs_log, "C:/Users/Bruger/OneDrive - University of Copenhagen/4. år/Bachelor/Bachelor1/Data/leafs_log.csv", row.names=FALSE)
write.csv(roots_log, "C:/Users/Bruger/OneDrive - University of Copenhagen/4. år/Bachelor/Bachelor1/Data/roots_log.csv", row.names=FALSE)
write.csv(wood_log, "C:/Users/Bruger/OneDrive - University of Copenhagen/4. år/Bachelor/Bachelor1/Data/wood_log.csv", row.names=FALSE)

write.csv(train_leafs_log, "C:/Users/Bruger/OneDrive - University of Copenhagen/4. år/Bachelor/Bachelor1/Data/train_leafs_log.csv", row.names=FALSE)
write.csv(test_leafs_log, "C:/Users/Bruger/OneDrive - University of Copenhagen/4. år/Bachelor/Bachelor1/Data/test_leafs_log.csv", row.names=FALSE)
write.csv(train_wood_log, "C:/Users/Bruger/OneDrive - University of Copenhagen/4. år/Bachelor/Bachelor1/Data/train_wood_log.csv", row.names=FALSE)
write.csv(test_wood_log, "C:/Users/Bruger/OneDrive - University of Copenhagen/4. år/Bachelor/Bachelor1/Data/test_wood_log.csv", row.names=FALSE)
write.csv(test_roots_log, "C:/Users/Bruger/OneDrive - University of Copenhagen/4. år/Bachelor/Bachelor1/Data/test_roots_log.csv", row.names=FALSE)
write.csv(train_roots_log, "C:/Users/Bruger/OneDrive - University of Copenhagen/4. år/Bachelor/Bachelor1/Data/train_roots_log.csv", row.names=FALSE)

write.csv(train_leafs, "C:/Users/Bruger/OneDrive - University of Copenhagen/4. år/Bachelor/Bachelor1/Data/train_leafs.csv", row.names=FALSE)
write.csv(test_leafs, "C:/Users/Bruger/OneDrive - University of Copenhagen/4. år/Bachelor/Bachelor1/Data/test_leafs.csv", row.names=FALSE)
write.csv(train_wood, "C:/Users/Bruger/OneDrive - University of Copenhagen/4. år/Bachelor/Bachelor1/Data/train_wood.csv", row.names=FALSE)
write.csv(test_wood,"C:/Users/Bruger/OneDrive - University of Copenhagen/4. år/Bachelor/Bachelor1/Data/test_wood.csv", row.names=FALSE)
write.csv(test_roots, "C:/Users/Bruger/OneDrive - University of Copenhagen/4. år/Bachelor/Bachelor1/Data/test_roots.csv", row.names=FALSE)
write.csv(train_roots, "C:/Users/Bruger/OneDrive - University of Copenhagen/4. år/Bachelor/Bachelor1/Data/train_roots.csv", row.names=FALSE)

