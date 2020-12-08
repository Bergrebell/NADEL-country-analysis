# corrplot source: http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software

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
require(ggplot2)

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
summary(t05$survey_weight)

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


#p <- ggplot(data = sub05, aes(x=variable, y=value)) + 
  geom_boxplot(aes(fill=Label))

model1 <- lm(t05$stunting ~ t05$Y_his)
plot(model1)

# why we must use fixed time effect: stunting and dead5 are sinking over time
ggplot(tanzania, 
       aes(x = factor(year,
                      levels = c("2005", "2010", 
                                 "2015")),
           fill = factor(stunting==1))) + 
  geom_bar(position = "fill") +
  labs(y = "stunting") +
  labs(x = "year")

ggplot(tanzania, 
       aes(x = factor(year,
                      levels = c("2005", "2010", 
                                 "2015")),
           fill = factor(dead5==1))) + 
  geom_bar(position = "fill") +
  labs(y = "dead5") +
  labs(x = "year")






# PLAYGROUND
scatterplot(stunting~year|region, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=tanzania)

ggplot(tanzania, aes(x=log_y, y=year, colour=factor(region))) +
  geom_point() +
  xlab("Hours worked") +
  ylab("Weekly salary") + 
  scale_colour_discrete(name="Female")

ggplot(tanzania, 
       aes(x = year, 
           fill = stunting == 1)) + 
  geom_bar(position = "dodge")

ggplot(tanzania, 
       aes(x = year, 
           fill = dead5 == 1)) + 
  geom_bar(position = "dodge")

ggplot(tanzania, 
       aes(x = year, 
           fill = stunting==1)) + 
  geom_bar(position = "fill") +
  labs(y = "stunting")

ggplot(tanzania, 
       aes(x = year, 
           fill = dead5==1)) + 
  geom_bar(position = "fill") +
  labs(y = "dead5")


ggplot(tanzania, 
       aes(x = factor(year,
                      levels = c("2005", "2010", 
                                 "2015")),
           fill = factor(stunting==1, 
                         levels = c("Buu", "TRUE", "FALSE"),
                         labels = c("NA", "TRUE", "FALSE")))) + 
  geom_bar(position = "fill") +
  labs(y = "stunting")

qplot(x = factor(year), y = Y_his,
      geom = "boxplot",
      data = tanzania)

tanzania %>%
  ggplot() +
  aes(x = year, y = log_y) + geom_jitter(alpha = .1) + geom_smooth(method = "lm")


# share of stunting, non-stunting & NA
table(tanzania$stunting, useNA="always")
x = 7936
n = 7936 + 13283

library(binom)
binom.confint(7936, 24198)

sum(stunting=="1")

library(Hmisc)
binconf(7936, 24198,alpha = 0.05)
