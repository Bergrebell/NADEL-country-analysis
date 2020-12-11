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

# inspect data - source: https://www.littlemissdata.com/blog/inspectdf
library(devtools)
library(inspectdf)
library(tidyverse)

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

hist(t05$Y_his)
hist(t10$Y_his)
hist(t15$Y_his)

# select only certain columns from dataframe
sub05 <- select(t05, 'year')

# omit NA values
# tanzaniaNA05 <- na.omit(t05)

cat05 <- inspect_cat(t05) 
show_plot(cat05)

x <- inspect_imb(t05)
show_plot(x)

# warum log für corr? -> nicht linearität -> mehr nutzen von zusätzlichem geld wenn man nur wenig hat
# subset mit childId==1 => immer noch erste kind anschauen, damit familien mit vielen kinder keinen überproportionalen einfluss haben

model_dead5 = lm(dead5~log_y)
summary(model_dead5)
# interpretation: ein anstieg von 1% einkommen -> eine reduktion von 0.008% stunting (interpret siehe paper von ken)


# Model 2 - mit time effects
model_dead5_2 = lm(dead5~log_y + as.factor(year))
summary(model_dead5_2)

# Stargazer output erstellen
rob_se <- list(sqrt(diag(vcovHC(model_dead5, type = "HC1"))),
               sqrt(diag(vcovHC(model_dead5_2, type = "HC1"))))
stargazer(model_dead5, model_dead5_2,
          title = "Regressions Using Demographic and Health Surveys",
          type = "text",
          out="myfile",
          out.header=FALSE,
          align=TRUE,
          digits = 5,
          header = TRUE,
          omit="year",
          se = rob_se,
          object.names = TRUE,
          model.numbers = FALSE,
          column.labels = c("(I)", "(II)", "(III)", "(IV)", "(V)", "(VI)"))

corr=cor(tanzania[as.numeric(which(sapply(tanzania,class)=="numeric"))])
chart.Correlation(corr)

tanzania %>%
  group_by(year) %>%
  summarise_at("Y_his", funs(mean(., na.rm=TRUE)))

tanzania %>%
  group_by(year) %>%
  summarise_at("Y_his", funs(perce(., na.rm=TRUE)))

stargazer(t05, 
          type = 'text', min.max=TRUE, mean.sd = TRUE, 
          nobs = FALSE, median = FALSE, iqr = FALSE,
          digits=1, align=T,
          title = "Summary Statistics",
          out = 'tab.txt')



a <- c(t05$year, t05$stunting)
stargazer(a, type = "text")

