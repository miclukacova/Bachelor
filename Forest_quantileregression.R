leafs_log <- read.csv('Data/leafs_log.csv')
roots_log <- read.csv('Data/roots_log.csv')
wood_log <- read.csv('Data/wood_log.csv')

library(tidyverse)
library(readr)
library(infer)
library(randomForest)
library(RColorBrewer)
library(quantregForest)

x <- data.frame(leafs_log[1])
y <- data.frame(leafs_log[2])

quantregForest(x, y, keep.inbag = FALSE)

?randomForest


