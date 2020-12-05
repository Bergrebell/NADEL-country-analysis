# set console lang & wd
Sys.setenv(LANG = "en")
setwd("~/Dropbox (Personal)/NADEL ETHZ/Policy Evaluation and Applied Statistics/Country Analysis")

# import packages
library(corrgram)
library(corrplot)

# read and attach Tanzania dataset
tanzania <- read_csv("Tanzani.csv")
attach(tanzania)

# check for correlations
#corrplot(corrgram(t05), type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
corrplot(corrgram(t05), type = "full", method="pie")

