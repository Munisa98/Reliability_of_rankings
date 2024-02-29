# Descriptive statistics

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

## Summary statistics table
stargazer(subset(soep, soep$WEST89 == 1), title="Descriptive Statistics for West Germany Location in 1989", omit.summary.stat = c("N", "min", "max"))
stargazer(subset(soep, soep$WEST89 == 0), title="Descriptive Statistics for East Germany Location in 1989", omit.summary.stat = c("N", "min", "max"))
