#Import package
library(tidyverse)

# Import csv's of cleaned data for each content subdomain (Mass and Kinetics)
# Create all variables as factors
mass <- read_csv("mass_uncleaned.csv", 
                 col_types = cols(year = col_factor(levels = c("2019", "2020")),
                                  task = col_factor(levels = c("Lecture", "InClassProblems", "Homework", "PBL1", "PBL2")),
                                  level = col_factor(levels = c("Remember", "Understand", "Apply", "Analyze", "Evaluate", "Create")),
                                  task_binary_pbl = col_factor(levels = c("nonPBL","PBL")),
                                  task_binary_hw = col_factor(levels = c("nonHW","HW")),
                                  task_binary_icp = col_factor(levels = c("nonICP","ICP")),
                                  task_binary_lec = col_factor(levels = c("nonLEC","LEC"))))

kinetics <- read_csv("kinetics_uncleaned.csv",
                     col_types = cols(year = col_factor(levels = c("2019", "2020")),
                                      task = col_factor(levels = c("Lecture", "InClassProblems", "Homework", "PBL2")),
                                      level = col_factor(levels = c("Remember", "Understand", "Apply", "Analyze", "Evaluate", "Create")),
                                      task_binary_pbl = col_factor(levels = c("nonPBL","PBL")),
                                      task_binary_hw = col_factor(levels = c("nonHW","HW")),
                                      task_binary_icp = col_factor(levels = c("nonICP","ICP")),
                                      task_binary_lec = col_factor(levels = c("nonLEC","LEC"))))

# Create a linear regression for each Task * Bloom's in Mass
fit11 <- lm(score ~ level_numeric * task_binary_lec + year, data = mass)
fit12 <- lm(score ~ level_numeric * task_binary_icp + year, data = mass)
fit13 <- lm(score ~ level_numeric * task_binary_hw + year, data = mass)
fit14 <- lm(score ~ level_numeric * task_binary_pbl + year, data = mass)

# Store these new models and their coef/SE/t-val/p-val in a df
df_mlevels <- data.frame(tidy(fit11),tidy(fit12),tidy(fit13),tidy(fit14))

# Create an interaction model for each Task * Bloom's in Kinetics
fit21 <- lm(score ~ level_numeric * task_binary_lec + year, data = kinetics)
fit22 <- lm(score ~ level_numeric * task_binary_icp + year, data = kinetics)
fit23 <- lm(score ~ level_numeric * task_binary_hw + year, data = kinetics)
fit24 <- lm(score ~ level_numeric * task_binary_pbl + year, data = kinetics)

# Store these new models and their coef/SE/t-val/p-val in a df
df_klevels <- data.frame(tidy(fit21),tidy(fit22),tidy(fit23),tidy(fit24))