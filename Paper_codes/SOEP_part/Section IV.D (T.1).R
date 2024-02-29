# Regression table entire sample and 5 quantiles
# Hypothesis testing for entire sample and 5 quantiles

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

## Entire Sample
# Regression - Entire Sample 
reg_1 <- lm(LS ~ WEST89:SYR + AGE + AGE_SQ + FEMALE + MARRIED + FT + PT + APPREN + MINIJOB + HHINC + 0 + SYR, data=soep)
summary(reg_1)


# Hypothesis testing for the entire sample
linearHypothesis(reg_1, "WEST89:SYR1991 = WEST89:SYR2019")




## Regression analysis by cohort 
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



# 2005-2015 vs. 1990-2005, the rate of convergence is higher on average.
coeff_1995_2005 <- matrix(coef(reg_1)[45:55])
coeff_1995_2005 <- data_frame(coeff_1995_2005)
coeff_1995_2005 <- coeff_1995_2005 %>% rename(coeff = coeff_1995_2005)
coeff_1995_2005$SYR <- c(1995:2005)
mean_coeff_1995_2005 <- mean(coeff_1995_2005$coeff)
reg_1995_2005 <- lm(coeff ~ SYR, data=coeff_1995_2005)
norm_1995_2005 <- coef(reg_1995_2005)[2] / mean_coeff_1995_2005 # -0.00140471 -> -0.14% on average per year
coeff_1995_2005$PRED <- predict(reg_1995_2005)

coeff_2005_2015 <- matrix(coef(reg_1)[55:65])
coeff_2005_2015 <- data_frame(coeff_2005_2015)
coeff_2005_2015 <- coeff_2005_2015 %>% rename(coeff = coeff_2005_2015)
coeff_2005_2015$SYR <- c(2005:2015)
mean_coeff_2005_2015 <- mean(coeff_2005_2015$coeff)
reg_2005_2015 <- lm(coeff ~ SYR, data=coeff_2005_2015)
norm_2005_2015 <- coef(reg_2005_2015)[2] / mean_coeff_2005_2015 # - 0.08508016 -> -8.51% on average per year
coeff_2005_2015$PRED <- predict(reg_2005_2015)
#coeff_2005_2015$PRED2 <- coeff_2005_2015$PRED * (1+norm_2005_2015) # to confirm if on average, we are losing 8% each year. 

coeff_all <- rbind(coeff_1995_2005, coeff_2005_2015)
coeff_all$PRED <- c(predict(reg_1995_2005), predict(reg_2005_2015))

plot(y=coeff_all$coeff, x=coeff_all$SYR, 
     main = "SYR x DIFF",
     xlab = "Year", 
     ylab = "DIFF")

plot(y=coeff_all$PRED, x=coeff_all$SYR, 
     main = "SYR x PRED",
     xlab = "Year", 
     ylab = "PRED")