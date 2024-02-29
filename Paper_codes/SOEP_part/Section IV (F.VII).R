# Entire sample mean life satisfaction plot 

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

# Use the merged and cleaned data file "soep_final.csv" from python
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


## Create new dataframe with the mean values
LS_MEAN_SYR <- aggregate(LS ~ SYR, data = soep, FUN = mean)
LS_MEAN_SYR_WEST89 <- aggregate(LS ~ SYR + WEST89, data = soep, FUN = mean)

# Plot LS mean against Year 
common_ylim <- range(5.5, 9.5)
colors <- ifelse(LS_MEAN_SYR_WEST89$WEST89 == 0, "black", "red")
plot(y=LS_MEAN_SYR_WEST89$LS, x=LS_MEAN_SYR_WEST89$SYR, 
     pch = ifelse(LS_MEAN_SYR_WEST89$WEST89 == 0, 1, 2),
     col = colors,
     font.main = 1,
     main = "Entire Sample",
     xlab = "Year", 
     ylab = "Mean Life Satisfaction",
     ylim = common_ylim)
legend("bottomright", legend = c("East Germany", "West Germany"), 
       col = c("black", "red"), pch = c(1, 2), title = "Location in 1989", cex = 0.7,bty = "n")