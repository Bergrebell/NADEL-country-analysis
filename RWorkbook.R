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

# read and attach Tanzania dataset
tanzania <- read_csv("Tanzani.csv")
attach(tanzania)
#ut <- unique(tanzania) not needed anymore

# get some general info + datatypes
#str(dataset)

t05 <- subset(tanzania, year==2005)
t10 <- subset(tanzania, year==2010)
t15 <- subset(tanzania, year==2015)

# check for correlations
corrplot(corrgram(t05), type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)