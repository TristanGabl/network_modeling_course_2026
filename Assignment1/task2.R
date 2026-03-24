#task 2
#Group 30: Tristan Gabl, Hristo Georgiev, Kevin Halter, Jakob Teetz

library(readr)
library(sna)
library(tidyverse)
library(dplyr)
library(gridExtra)
library(grid)

#setwd("./Lintner")

#Task 2.1
attr <- read.csv("attr.csv", sep = ";")

w1 <- as.matrix(read.csv("10_W1.csv", header = FALSE, sep = ";"))
w2 <- as.matrix(read.csv("10_W2.csv", header = FALSE, sep = ";"))

attr10 <- attr %>% filter(classroomID == 10)

complete_idx <- which(complete.cases(attr10))
attr10 <- attr10[complete_idx, ]
w1 <- w1[complete_idx, complete_idx]
w2 <- w2[complete_idx, complete_idx]

model1 <- netlm(w2, w1, nullhyp = "qap", reps = 5000)

summary(model1)


#Task 2.2 / 2.3:

literacy_matrix <- outer(rep(1, nrow(attr10)), attr10$literacy_end)
gender_matrix <- outer(attr10$gender, attr10$gender, FUN = "==") * 1
HISEI_matrix <- outer(attr10$HISEI, rep(1, nrow(attr10)))

model2 <- netlm(
  w2,
  list(w1, literacy_matrix, gender_matrix, HISEI_matrix),
  nullhyp = "qap",
  reps = 5000
)

summary(model2)


#Task 2.4 / 2.5

classrooms <- c("01", "02", "03", "04", "05", "10", "12")
results <- list()

for (class in classrooms) {
  w1 <- as.matrix(read.csv(paste0(class, "_W1.csv"), header = FALSE, sep = ";"))
  w2 <- as.matrix(read.csv(paste0(class, "_W2.csv"), header = FALSE, sep = ";"))
  attr <- read.csv("attr.csv", sep = ";")
  
  attr_class <- attr %>% filter(classroomID == as.numeric(class))
  
  complete_idx <- which(complete.cases(attr_class))
  attr_class <- attr_class[complete_idx, ]
  w1 <- w1[complete_idx, complete_idx]
  w2 <- w2[complete_idx, complete_idx]
  
  literacy_matrix <- outer(rep(1, nrow(attr_class)), attr_class$literacy_end)
  gender_matrix <- outer(attr_class$gender, attr_class$gender, FUN = "==") * 1
  HISEI_matrix <- outer(attr_class$HISEI, rep(1, nrow(attr_class)))
  
  model <- netlm(
    w2,
    list(w1, literacy_matrix, gender_matrix, HISEI_matrix),
    nullhyp = "qap",
    reps = 5000
  )
  
  results[[class]] <- summary(model)$coefficients
}

results


#Task 2.5:

summary_df <- bind_rows(
  lapply(names(results), function(class) {
    data.frame(
      classroom = class,
      intercept = results[[class]][1],
      w1 = results[[class]][2],
      literacy = results[[class]][3],
      gender = results[[class]][4],
      HISEI = results[[class]][5]
    )
  })
)

summary_df

summary_df_round <- summary_df
summary_df_round[-1] <- lapply(summary_df_round[-1], function(x) round(x, 4))

png("../task2_table.png", width = 1200, height = 400, res = 150)
grid.table(summary_df_round)
dev.off()
