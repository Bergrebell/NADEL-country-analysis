# set console lang & wd
Sys.setenv(LANG = "en")
setwd("~/Dropbox (Personal)/NADEL ETHZ/Policy Evaluation and Applied Statistics/Country Analysis")

# import packages
library(readr)
library(naniar) # to replace NA 
library(AER)
library(stargazer)
library(PerformanceAnalytics)
library(reshape2)
library(corrplot)
library(corrgram)
library(dplyr)
library(sandwich)
library(tidyr)

# inspect data - source: https://www.littlemissdata.com/blog/inspectdf
library(devtools)
library(inspectdf)
library(tidyverse)

# read and attach Tanzania dataset
tanzania <- read_csv("Tanzani.csv")
attach(tanzania)

# per year data
t05 <- subset(tanzania, year==2005)
t10 <- subset(tanzania, year==2010)
t15 <- subset(tanzania, year==2015)


# select only certain columns from dataframe
sub05 <- select(t05,
                'stunting',
                'c_age', 
                'water_improved_total', 
                'sani_improved_total',
                'mo_noedu',
                'mo_care',
                'mo_assistance',
                'Y_his',
                'log_y')

# first visual hint on correlation between variables
source("http://www.sthda.com/upload/rquery_cormat.r")
rquery.cormat(sub05)

# correlation matrix with p-values and historgram
chart.Correlation(sub05)



