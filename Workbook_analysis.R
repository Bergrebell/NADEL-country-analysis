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

# Regression models ---------------------------------------------------------
# Stunting - Model 1
model_stunting_1 = lm(stunting~log_y)
summary(model_stunting_1)

# Stunting - Model 2
model_stunting_2_tfe = lm(stunting~log_y + as.factor(year))
summary(model_stunting_2_tfe)

# Stunting - Model 3
model_stunting_3_tfe = lm(stunting~
                            log_y + 
                            as.factor(year) + 
                            as.factor(urban) + 
                            c_age + 
                            c_sex + 
                            mo_age_birth +
                            as.factor(mo_breastfeeeding) +
                            mo_noedu +
                            mo_primary +
                            mo_secondary +
                            water_improved_total +
                            sani_improved_total
)
summary(model_stunting_3_tfe)

# Dead5 - Model 1
model_dead5_1 = lm(dead5~log_y)
summary(model_dead5_1)

# Dead5 - Model 2
model_dead5_2_tfe = lm(dead5~log_y + as.factor(year))
summary(model_dead5_2_tfe)

# Dead5 - Model 3
model_dead5_3_tfe = lm(dead5~
                            log_y + 
                            as.factor(year) + 
                            as.factor(urban) + 
                            c_sex + 
                            c_first + 
                            mo_assistance +
                            mo_breastfeeeding +
                            mo_age_birth +
                            mo_primary +
                            mo_secondary +
                            water_improved_total +
                            sani_improved_total
)
summary(model_dead5_3_tfe)

# Stargazer S.errors ---------------------------------------------------------
rob_se <- list(
               sqrt(diag(vcovHC(model_stunting_1, type = "HC1"))),
               sqrt(diag(vcovHC(model_stunting_2_tfe, type = "HC1"))),
               sqrt(diag(vcovHC(model_stunting_3_tfe, type = "HC1"))),
               sqrt(diag(vcovHC(model_dead5_1, type = "HC1"))),
               sqrt(diag(vcovHC(model_dead5_2_tfe, type = "HC1"))),
               sqrt(diag(vcovHC(model_dead5_3_tfe, type = "HC1")))
               )

# Stargazer table ---------------------------------------------------------
stargazer(model_stunting_1, model_stunting_2_tfe, model_stunting_3_tfe, model_dead5_1, model_dead5_2_tfe, model_dead5_3_tfe,
          title = "Regressions Using Demographic and Health Surveys",
          type = "latex",
          out="myfile",
          out.header=FALSE,
          align=TRUE,
          float.env = "sidewaystable",
          digits = 5,
          header = TRUE,
          omit="year",
          omit.stat=c("LL","ser","f"), 
          no.space=TRUE,
          se = rob_se,
          object.names = TRUE,
          model.numbers = FALSE,
          column.labels = c("(I)", "(II)", "(III)", "(IV)", "(V)", "(VI)"))


