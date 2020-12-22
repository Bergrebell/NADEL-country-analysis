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

tanzania$agg_education_indoubt_edu <- ifelse(tanzania$mo_secondary == 1, 'secondary',
                                      ifelse(tanzania$mo_primary ==1, 'primary',
                                      ifelse(tanzania$mo_noedu == 1,"noedu", "invalid")))

as.data.frame(table(tanzania$agg_education))
as.data.frame(table(tanzania$agg_education_indoubt_edu))


# education over time
ggplot(tanzania, aes(x=factor(year,levels=2005:2015))) +
  geom_bar(aes(fill = agg_education), position = "fill", width = 0.4) + theme_classic() +
  labs(y = "proportion", fill ="agg_education") +
  scale_x_discrete('year', breaks=factor(2005:2015), drop=TRUE)

ggplot(tanzania, aes(x=factor(year,levels=2005:2015))) +
  geom_bar(aes(fill = agg_education_indoubt_edu), position = "fill", width = 0.4) + theme_classic() +
  labs(y = "proportion", fill ="agg_education_indoubt_edu") +
  scale_x_discrete('year', breaks=factor(2005:2015), drop=TRUE)

tanzania$agg_education_indoubt_edu <- ifelse(tanzania$mo_secondary == 1, 'secondary',
                                             ifelse(tanzania$mo_primary ==1, 'primary',
                                                    ifelse(tanzania$mo_noedu == 1,"noedu", "invalid")))

# descriptive summary table - V1 (kein latex)
library(table1)
library(rvest)
library(boot)

tanzania$dead5 <- factor(tanzania$dead5)
tanzania$dead1 <- factor(tanzania$dead1)
tanzania$dead0 <- factor(tanzania$dead0)
tanzania$stunting <- factor(tanzania$stunting)
tanzania$mo_breastfeeeding <- factor(tanzania$mo_breastfeeeding)
tanzania$c_sex <- factor(tanzania$c_sex)
tanzania$water_improved_total <- factor(tanzania$water_improved_total)
tanzania$sani_improved_total <- factor(tanzania$sani_improved_total)
tanzania$mo_care <- factor(tanzania$mo_care)
tanzania$mo_assistance <- factor(tanzania$mo_assistance)


# most important
table1::label(tanzania$stunting) <- "Stunting status"
table1::label(tanzania$dead5) <- "Under-five mortality status"
table1::label(tanzania$Y_his) <- "Household income"
table1::label(tanzania$urban) <- "Place of residence"
# mother
table1::label(tanzania$mo_age) <- "Mothers age (years)"
table1::label(tanzania$mo_totchild) <- "Mothers total children"
table1::label(tanzania$mo_age_first_birth) <- "Mothers age (years) at first birth"
table1::label(tanzania$mo_breastfeeeding) <- "Mothers breastfeeding status"
table1::label(tanzania$agg_education_indoubt_edu) <- "Mother education"
table1::label(tanzania$mo_care) <- "Mother received prenatal care"
table1::label(tanzania$mo_assistance) <- "Mother received medical assistance"

# child
table1::label(tanzania$c_age) <- "Childs age (months)"
table1::label(tanzania$c_sex) <- "Childs sex (1=male; 2=female)"
# general
table1::label(tanzania$water_improved_total) <- "Water: improved"
table1::label(tanzania$sani_improved_total) <- "Sanitation: improved"
table1::label(tanzania$dead1) <- "Under-one year mortality status"
table1::label(tanzania$dead0) <- "Under-one month mortality status"

table1::table1(~stunting + 
                 dead5 + 
                 Y_his + 
                 urban + 
                 mo_age + 
                 mo_age_first_birth +
                 mo_totchild + 
                 mo_breastfeeeding +
                 mo_care +
                 mo_assistance +
                 c_age +
                 c_sex +
                 water_improved_total +
                 sani_improved_total +
                 agg_education_indoubt_edu +
                 dead1 +
                 dead0
                 
               | year, data = tanzania)









# STUFF












