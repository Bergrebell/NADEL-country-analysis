# set console lang & wd
Sys.setenv(LANG = "en")
setwd("~/Dropbox (Personal)/NADEL ETHZ/Policy Evaluation and Applied Statistics/Country Analysis")

# import packages
library(readr)
library(sandwich)
library(stargazer)
library(tidyr)
require(ggplot2)
library(data.table)
library(tidyr)

# read and attach Tanzania dataset
tanzania <- read_csv("Tanzani.csv")
attach(tanzania)

# per year data
t05 <- subset(tanzania, year==2005)
t10 <- subset(tanzania, year==2010)
t15 <- subset(tanzania, year==2015)
summary(t05$survey_weight)

# unique number of households
distinct_households <- unique(hh_id)
length(distinct_households)

# show how income has changed over time
qplot(x = factor(year), y = Y_his,
      geom = "boxplot",
      data = tanzania)

# show how income has changed over time + rural vs urban
qplot(x = factor(year), y = Y_his, colour=factor(urban),
geom = "boxplot",
data = tanzania)

# show how income differs per region and time (for ANNEX)
qplot(x = region, y = Y_his, colour=factor(year),
      geom = "boxplot",
      data = tanzania)

# descriptive summary table - V1 (kein latex)
library(table1)
library(rvest)

table1::label(tanzania$stunting) <- "Life Expectancy"
table1::label(tanzania$dead5) <- "Population"
table1::label(tanzania$Y_his) <- "Gdp Per Capita"
table1::table1(~stunting + dead5 + Y_his | year, data = tanzania)

# stunting ratio over time
ggplot(subset(tanzania, !is.na(tanzania$stunting)), aes(x=factor(year,levels=2005:2015))) +
  geom_bar(aes(fill = stunting == 1), position = "fill", width = 0.4) + theme_classic() +
  labs(y = "proportion", fill ="stunting") +
  scale_x_discrete('year', breaks=factor(2005:2015), drop=TRUE)

# dead5 ratio over time
ggplot(subset(tanzania, !is.na(tanzania$dead5)), aes(x=factor(year,levels=2005:2015))) +
  geom_bar(aes(fill = dead5 == 1), position = "fill", width = 0.4) + theme_classic() +
  labs(y = "proportion", fill ="dead5") +
  scale_x_discrete('year', breaks=factor(2005:2015), drop=TRUE)

# water_improved_total ratio over time
ggplot(subset(tanzania, !is.na(tanzania$water_improved_total)), aes(x=factor(year,levels=2005:2015))) +
  geom_bar(aes(fill = water_improved_total == 1), position = "fill", width = 0.4) + theme_classic() +
  labs(y = "proportion", fill ="water_improved_total") +
  scale_x_discrete('year', breaks=factor(2005:2015), drop=TRUE)
  
# sanitation_improved_total ratio over time
ggplot(subset(tanzania, !is.na(tanzania$sani_improved_total)), aes(x=factor(year,levels=2005:2015))) +
  geom_bar(aes(fill = sani_improved_total == 1), position = "fill", width = 0.4) + theme_classic() +
  labs(y = "proportion", fill ="sani_improved_total") +
  scale_x_discrete('year', breaks=factor(2005:2015), drop=TRUE)


# education ratio over time
ggplot(subset(tanzania, !is.na(tanzania$education)), aes(x=factor(year,levels=2005:2015))) +
  geom_bar(aes(fill = education), position = "fill", width = 0.4) + theme_classic() +
  labs(y = "proportion", fill ="education") +
  scale_x_discrete('year', breaks=factor(2005:2015), drop=TRUE)


tanzania$agg_education <- ifelse(tanzania$mo_noedu == 1,"noedu",
                         ifelse(tanzania$mo_secondary == 1, 'secondary',
                                ifelse(tanzania$mo_primary ==1, 'primary', 'invalid')))





















# STUFF

# Flavio example:
stuntingratio_vs_years = c(mean(t05$stunting,na.rm=TRUE),mean(t10$stunting,na.rm=TRUE),mean(t15$stunting,na.rm=TRUE))
dead5ratio_vs_years = c(mean(t05$dead5),mean(t10$dead5),mean(t15$dead5))

my.df <- data.frame(c(1,2,3),
                    100*dead5ratio_vs_years,
                    100*stuntingratio_vs_years)

# stunting ratio over time => Flavios ergebnisse stimmen
mean(t05$stunting, na.rm = TRUE) # => 0.5694675
mean(tanzania$dead5, na.rm = TRUE) # => 0.5694675


length(which(t05$stunting == 0)) # => 3775
length(which(t05$stunting == 1)) # => 2854
3775 / sum(2854, 3775) # => 0.5694675
2854 / sum(2854, 3775) # => 0.4305325



stuntingratio_vs_years = c(mean(t05$stunting,na.rm=TRUE),mean(t10$stunting,na.rm=TRUE),mean(t15$stunting,na.rm=TRUE))
dead5ratio_vs_years = c(mean(t05$dead5,na.rm=TRUE),mean(t10$dead5,na.rm=TRUE),mean(t15$dead5,na.rm=TRUE))
average_income = c(mean(t05$Y_his),mean(t10$Y_his),mean(t15$Y_his))

graph_data <- data.frame(c("2005","2010","2015"),
                         100*dead5ratio_vs_years,
                         100*stuntingratio_vs_years,
                         average_income
)
colnames(graph_data) <- c("year","dead_percent","stunting_percent","income_avg")

ggplot(data=graph_data, 
       aes(x=year, y=dead_percent, group=1)) +
  geom_line()+
  geom_point()

ggplot(data=graph_data, 
       aes(x=year, y=stunting_percent, group=1)) +
  geom_line()+
  geom_point()

ggplot(data=subset(tanzania, !is.na(tanzania$stunting)), 
       aes(x = year, 
           fill = stunting == 1)) + 
  geom_bar(position = "stack")

ggplot(tanzania, 
       aes(x = year, 
           fill = dead5 == 1)) + 
  geom_bar(position = "dodge")

ggplot(tanzania, aes(x = year)) +
  geom_bar(aes(fill = dead5 == 1), position = position_stack(reverse = TRUE)) +
  theme(legend.position = "top")










