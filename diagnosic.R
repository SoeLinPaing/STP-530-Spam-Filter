rm(list=ls()) # Clean up the workspace for the new analysis
options(max.print = 10000)

library(dplyr)
library(car)
library(caret)
library(binomTools)

setwd("C:/Users/slinp/Desktop/spambase")

spamdata <- read.csv("spambase.data", header = F)

attach(spamdata)