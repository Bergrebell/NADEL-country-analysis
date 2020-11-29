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
nepal <- read_csv("Nepal.csv")
attach(nepal)
length(unique(hh_id))


# get some general info + datatypes
str(dataset)

# check for correlations
# corrplot(corrgram(dataset), type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

