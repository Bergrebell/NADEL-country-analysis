# set console lang & wd
Sys.setenv(LANG = "en")
setwd("~/Dropbox (Personal)/NADEL ETHZ/Policy Evaluation and Applied Statistics/Country Analysis")

# import packages
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

# read and attach Tanzania dataset
tanzania <- read_csv("Tanzani.csv")
attach(tanzania)

# per year data
t05 <- subset(tanzania, year==2005)
t10 <- subset(tanzania, year==2010)
t15 <- subset(tanzania, year==2015)
summary(t05$survey_weight)

tanzania$agg_education_indoubt_edu <- ifelse(tanzania$mo_secondary == 1, 'secondary',
                                             ifelse(tanzania$mo_primary ==1, 'primary',
                                                    ifelse(tanzania$mo_noedu == 1,"noedu", "invalid")))

# Corrmatrix Stunting
# select only certain columns from dataframe
sub_stunting_with_factors <- select(tanzania,
                       'stunting',
                       'urban',
                       'c_age', 
                       'c_sex',
                       'mo_age_birth',
                       'mo_breastfeeeding',
                       'mo_primary',
                       'mo_secondary',
                       'log_y',
                       'water_improved_total', 
                       'sani_improved_total'
)
sub_stunting_with_factors$mo_breastfeeeding <- as.factor(sub_stunting_with_factors$mo_breastfeeeding)
sub_stunting_with_factors$urban <- as.factor(sub_stunting_with_factors$urban)

library(tidyr)
sub_stunting_with_factors_clean <- na.omit(sub_stunting_with_factors)
sub_stunting_with_factors_clean$mo_breastfeeeding <- ifelse(sub_stunting_with_factors_clean$mo_breastfeeeding == 'yes', 1, 0)
sub_stunting_with_factors_clean$urban <- ifelse(sub_stunting_with_factors_clean$urban == 'urban', 1, 0)
head(sub_stunting_with_factors_clean)

# first visual hint on correlation between variables
require(corrplot)
corrplot(corrgram(sub_stunting_with_factors_clean), type = "upper", tl.col = "black", tl.srt = 45, sig.level = 0.05, insig = "blank")

# Corrmatrix Dead5
# select only certain columns from dataframe
sub_dead5_with_factors <- select(tanzania,
                                    'dead5',
                                    'c_sex',
                                    'c_first',
                                    'mo_assistance',
                                    'mo_care',
                                    'mo_tetanus',
                                    'mo_breastfeeeding',
                                    'mo_age_birth',
                                    'mo_primary',
                                    'mo_secondary',
                                    'log_y',
                                    'urban',
                                    'water_improved_total', 
                                    'sani_improved_total'
                                    
)
sub_dead5_with_factors$mo_breastfeeeding <- as.factor(sub_dead5_with_factors$mo_breastfeeeding)
sub_dead5_with_factors$urban <- as.factor(sub_dead5_with_factors$urban)

sub_dead5_with_factors_clean <- na.omit(sub_dead5_with_factors)
sub_dead5_with_factors_clean$mo_breastfeeeding <- ifelse(sub_dead5_with_factors_clean$mo_breastfeeeding == 'yes', 1, 0)
sub_dead5_with_factors_clean$urban <- ifelse(sub_dead5_with_factors_clean$urban == 'urban', 1, 0)
head(sub_dead5_with_factors_clean)

# first visual hint on correlation between variables
require(corrplot)
corrplot(corrgram(sub_dead5_with_factors_clean), type = "upper", tl.col = "black", tl.srt = 45, sig.level = 0.05, insig = "blank")


# Corrmatrix Both --------------------------

sub_all_with_factors <- select(tanzania,
                                 'dead5',
                                 'c_sex',
                                 'c_first',
                                 'mo_assistance',
                                 'mo_care',
                                 'mo_tetanus',
                                 'mo_breastfeeeding',
                                 'mo_age_birth',
                                 'mo_primary',
                                 'mo_secondary',
                                 'log_y',
                                 'urban',
                                 'water_improved_total', 
                                 'sani_improved_total'
)
sub_all_with_factors$mo_breastfeeeding <- as.factor(sub_all_with_factors$mo_breastfeeeding)
sub_all_with_factors$urban <- as.factor(sub_all_with_factors$urban)
sub_all_with_factors_clean <- na.omit(sub_all_with_factors)
sub_all_with_factors_clean$mo_breastfeeeding <- ifelse(sub_all_with_factors_clean$mo_breastfeeeding == 'yes', 1, 0)
sub_all_with_factors_clean$urban <- ifelse(sub_all_with_factors_clean$urban == 'urban', 1, 0)
head(sub_all_with_factors_clean)

corrplot(corrgram(sub_all_with_factors_clean), type = "upper", tl.col = "black", tl.srt = 45, sig.level = 0.05, insig = "blank")



# STUFF
# Other correlation matrix tool
source("http://www.sthda.com/upload/rquery_cormat.r")
rquery.cormat(sub_dead5_with_factors_clean)