library(readr)
library(sna)
library(tidyverse)

setwd("C:/Users/kevin/Downloads/Assignment1/Assignment1/Lintner")

w1 <- as.matrix(read.csv("10_W1.csv", header = FALSE, sep = ";"))
w2 <- as.matrix(read.csv("10_W2.csv", header = FALSE, sep = ";"))
attr <- read.csv("attr.csv", sep = ";")

attr10 <- attr %>% filter(classroomID == 10)

complete_idx <- which(complete.cases(attr10))
attr10 <- attr10[complete_idx, ]
w1 <- w1[complete_idx, complete_idx]
w2 <- w2[complete_idx, complete_idx]

model1 <- netlm(w2, w1, nullhyp = "qap", reps = 5000)

summary(model1)