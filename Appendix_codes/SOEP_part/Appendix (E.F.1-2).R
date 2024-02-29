# Mean life satisfaction plot (five / ten quantiles)

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

num_quantile <- 5 # 10 
soep$birthyear_quantile <- split_quantile(soep$BIRTHYR, type = num_quantile)

# Find out the birth year range for each quantile
soep %>%
  group_by(birthyear_quantile) %>%
  summarise(First_Birth_Year = min(BIRTHYR),
            Last_Birth_Year = max(BIRTHYR),
            Count = n()) %>%
  ungroup() 

# Loop through quantiles for mean LS 
# par(mfrow = c(2, 2)) # to go back to original setting: par()
quantiles <- seq(1, num_quantile)
common_xlim <- range(soep$SYR)
for (q in quantiles) {
  subset_data <- subset(soep, birthyear_quantile == q)
  
  # Aggregate data for the current quantile
  agg_data <- aggregate(LS ~ SYR + WEST89, 
                        data = subset_data, FUN = mean)
  
  birth_year_range <- summarise(subset_data, 
                                First_Birth_Year = min(BIRTHYR),
                                Last_Birth_Year = max(BIRTHYR))
  
  colors <- ifelse(agg_data$WEST89 == 0, "black", "red")
  
  common_ylim <- range(5.5, 9.5)
  
  plot(y = agg_data$LS, 
       x = agg_data$SYR, 
       pch = ifelse(agg_data$WEST89 == 0, 1, 2),
       col = colors,
       font.main = 1,
       main = sprintf("Q%d: %d - %d", q, birth_year_range$First_Birth_Year, birth_year_range$Last_Birth_Year),
       xlab = "Year", 
       ylab = "Mean Life Satisfaction",
       xlim = common_xlim,
       ylim = common_ylim)
  
  legend("bottomright", title = "Location in 1989",
         legend = c("East Germany", "West Germany"),
         bty = "n", # Removes the legend box
         col = c("black", "red"),
         pch = c(1, 2),
         cex = 0.7)
}
