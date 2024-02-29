# Regression results across 5 quantiles + 10 quantiles
# Hypothesis testing for 10 quantiles

library(dplyr)
library(gplots)
library(car)
library(stargazer)
library(plm)
library(jtools)
library("ggplot2")
library(sparklyr)
library("fabricatr")
library(data.table)
library(naniar)
library(textreg)
library(reshape2)
library(arsenal)
suppressPackageStartupMessages(library("tidyverse"))

# Use the merged and cleaned data file "soep_final.csv"
soep <- read.csv()

# Rename columns for regression 
soep <- soep %>%
  rename(
    SYR = syear, 
    BIRTHYR = birth_year,
    FEMALE = female,
    WEST89 = west89,
    LS = life_satisfaction, 
    MARRIED = married, 
    PT = part_time, 
    FT = full_time,
    APPREN = apprenticeship,
    MINIJOB = mini_job,
    HHINC = hh_income,
    AGE = age,
    AGE_SQ = age_sq,
  )

# Year Dummies
soep$SYR <- factor(soep$SYR)

# Five Cohorts
num_quantile <- 5 
soep$birthyear_quantile <- split_quantile(soep$BIRTHYR, type = num_quantile)

soep_quantile_data <- list()
reg_quantile <- list()
hypothesis_results <- list()

for (i in 1:num_quantile) {
  soep_quantile_data[[i]] <- subset(soep, birthyear_quantile == as.character(i))
  reg_quantile[[i]] <- lm(LS ~ WEST89:SYR + AGE + AGE_SQ + FEMALE + MARRIED + FT + PT + APPREN + MINIJOB + HHINC + SYR + 0, data = soep_quantile_data[[i]])
}

# See regression results for each quantile
reg_quantile

# Hypothesis testing WEST89:SYR1991 = WEST89:SYR2019 for each quantile
for (i in 1:length(reg_quantile)) {
  reg <- reg_quantile[[i]]  # Extract the regression object
  R <- matrix(0, nrow = 1, ncol = length(coef(reg)))
  colnames(R) <- names(coef(reg))
  R[1, "WEST89:SYR1991"] <- 1
  R[1, "WEST89:SYR2019"] <- -1
  hypothesis_results[[i]] <- linearHypothesis(reg, R, rhs = c(0))
}

hypothesis_results



# Regression analysis for 10 cohorts 
num_quantile <- 10
soep$birthyear_quantile <- split_quantile(soep$BIRTHYR, type = num_quantile)

soep_q1_10 <- subset(soep, birthyear_quantile == "1")
soep_q2_10 <- subset(soep, birthyear_quantile == "2")
soep_q3_10 <- subset(soep, birthyear_quantile == "3")
soep_q4_10 <- subset(soep, birthyear_quantile == "4")
soep_q5_10 <- subset(soep, birthyear_quantile == "5")
soep_q6_10 <- subset(soep, birthyear_quantile == "6")
soep_q7_10 <- subset(soep, birthyear_quantile == "7")
soep_q8_10 <- subset(soep, birthyear_quantile == "8")
soep_q9_10 <- subset(soep, birthyear_quantile == "9")
soep_q10_10 <- subset(soep, birthyear_quantile == "10")

reg_fcq1_10 <- lm(LS ~ WEST89:SYR + AGE + AGE_SQ + FEMALE + MARRIED + FT + PT + MINIJOB + HHINC + SYR + 0, data=soep_q1_10) # variable APPREN is dropped as it contains only zeros for Q1.
reg_fcq2_10 <- lm(LS ~ WEST89:SYR + AGE + AGE_SQ + FEMALE + MARRIED + FT + PT + APPREN + MINIJOB + HHINC + SYR + 0, data=soep_q2_10)
reg_fcq3_10 <- lm(LS ~ WEST89:SYR + AGE + AGE_SQ + FEMALE + MARRIED + FT + PT + APPREN + MINIJOB + HHINC + SYR + 0, data=soep_q3_10)
reg_fcq4_10 <- lm(LS ~ WEST89:SYR + AGE + AGE_SQ + FEMALE + MARRIED + FT + PT + APPREN + MINIJOB + HHINC + SYR + 0, data=soep_q4_10)
reg_fcq5_10 <- lm(LS ~ WEST89:SYR + AGE + AGE_SQ + FEMALE + MARRIED + FT + PT + APPREN + MINIJOB + HHINC + SYR + 0, data=soep_q5_10)
reg_fcq6_10 <- lm(LS ~ WEST89:SYR + AGE + AGE_SQ + FEMALE + MARRIED + FT + PT + APPREN + MINIJOB + HHINC + SYR + 0, data=soep_q6_10)
reg_fcq7_10 <- lm(LS ~ WEST89:SYR + AGE + AGE_SQ + FEMALE + MARRIED + FT + PT + APPREN + MINIJOB + HHINC + SYR + 0, data=soep_q7_10)
reg_fcq8_10 <- lm(LS ~ WEST89:SYR + AGE + AGE_SQ + FEMALE + MARRIED + FT + PT + APPREN + MINIJOB + HHINC + SYR + 0, data=soep_q8_10)
reg_fcq9_10 <- lm(LS ~ WEST89:SYR + AGE + AGE_SQ + FEMALE + MARRIED + FT + PT + APPREN + MINIJOB + HHINC + SYR + 0, data=soep_q9_10)
reg_fcq10_10 <- lm(LS ~ WEST89:SYR + AGE + AGE_SQ + FEMALE + MARRIED + FT + PT + APPREN + MINIJOB + HHINC + SYR + 0, data=soep_q10_10)

linearHypothesis(reg_fcq1_10, "WEST89:SYR1991 = WEST89:SYR2019")
linearHypothesis(reg_fcq2_10, "WEST89:SYR1991 = WEST89:SYR2019")
linearHypothesis(reg_fcq3_10, "WEST89:SYR1991 = WEST89:SYR2019")
linearHypothesis(reg_fcq4_10, "WEST89:SYR1991 = WEST89:SYR2019")
linearHypothesis(reg_fcq5_10, "WEST89:SYR1991 = WEST89:SYR2019")
linearHypothesis(reg_fcq6_10, "WEST89:SYR1991 = WEST89:SYR2019")
linearHypothesis(reg_fcq7_10, "WEST89:SYR1991 = WEST89:SYR2019")
linearHypothesis(reg_fcq8_10, "WEST89:SYR1991 = WEST89:SYR2019")
linearHypothesis(reg_fcq9_10, "WEST89:SYR1991 = WEST89:SYR2019")

# For Q10, data available from 1997 onwards only 
linearHypothesis(reg_fcq10_10, "WEST89:SYR1997 = WEST89:SYR2019")



