library(vctrs)
library(survival)
library(boot)
library(haven)
library(dplyr)
library(ggplot2)
library(survminer)
library(epiDisplay)
library(tableone)
library(here)
here::i_am("suicidality/analysis/Prediction_12wks.R")

source(here("suicidality", "analysis", "common.R"))

#ITT 12wks follow-up
data <- filter_complete_cases(read_rds_file("main_12wks_28.rds"))

#FU time
data$t_end = ceiling((data$fu_end_itt - data$fu_start)/7)

# Some reformatting and data cleaning for the modeling
treatdat <- data

treatdat<-treatdat[!(treatdat$edufam_cat==99),]
treatdat<-treatdat[!(treatdat$inc_cat==99),]
treatdat<-treatdat[!(treatdat$source=="M"),]

treatdat$edu <- recode(treatdat$edufam_cat, "0" = 0, "1" = 0, "2" = 1)
treatdat$inc <- recode(treatdat$inc_cat, "1" = 0, "2" = 0, "3" = 1)
treatdat$hospital <- recode(treatdat$hosp, "0" = 0, "1" = 1, "2" = 1, "3" = 1)

# Create combined variables from Table S3 diagnoses
treatdat$behav <- case_when(
  treatdat$diag_personality_cluster_b | treatdat$diag_conduct ~ "1",
  TRUE ~ "0")

treatdat$psychosis <- case_when(
  treatdat$diag_psychotic | treatdat$med_antipsychotic ~ "1",
  TRUE ~ "0")

# Note: diag_sud already includes alcohol (F10-F19 excl F17)
treatdat$suds <- as.character(treatdat$diag_sud)

# Note: diag_overdose already includes all poisoning (T36-T51, X40-X49)
treatdat$pois <- as.character(treatdat$diag_overdose)

# Combined anxiety (phobic + panic/GAD + OCD + stress)
treatdat$anxiety_combined <- case_when(
  treatdat$diag_phobic | treatdat$diag_anxiety_other | treatdat$diag_ocd | treatdat$diag_stress ~ "1",
  TRUE ~ "0")

# Combined eating disorders (anorexia + bulimia)
treatdat$eat_combined <- case_when(
  treatdat$diag_anorexia | treatdat$diag_bulimia ~ "1",
  TRUE ~ "0")

treatdat$sedative <- case_when(
  treatdat$med_hypnotic | treatdat$med_benzodiazepine ~ "1",
  TRUE ~ "0")

#Fit cox model with predictors (without cc)
cox_pred <- coxph(Surv(t_end, sb12_itt) ~
                    female + age + year + as.factor(edu) + as.factor(hospital) + relevel(as.factor(source), ref="O") +
                    as.factor(inc) + as.factor(fh_suicidal) + as.factor(fh_depr) + anxiety_combined + eat_combined +
                    med_antipsychotic + suds + behav + diag_adhd + diag_intellectual_disability + diag_autism + pois + diag_suicidal +
                    sedative + med_antiepileptic + med_stimulant,
                    data=treatdat)

summary(cox_pred)

#Fit cox model with predictors (with cc)
cox_pred_cc <- coxph(Surv(t_end, sb12_itt) ~
                    cc + female + age + year + as.factor(edu) + as.factor(hospital) + relevel(as.factor(source), ref="O") +
                    as.factor(inc) + as.factor(fh_suicidal) + as.factor(fh_depr) + anxiety_combined + eat_combined +
                    med_antipsychotic + suds + behav + diag_adhd + diag_intellectual_disability + diag_autism + pois + diag_suicidal +
                    sedative + med_antiepileptic + med_stimulant,
                    data=treatdat)

summary(cox_pred_cc)

###################################
#frequencies for predictors

#reformat factor variables
varsToFactor <- c("edu","inc","source","hospital","fh_suicidal","fh_depr", "female",
                  "anxiety_combined", "eat_combined", "med_antipsychotic", "suds", "behav",
                  "diag_adhd", "diag_intellectual_disability", "diag_autism",
                  "pois", "diag_suicidal", "sedative",
                  "med_antiepileptic", "med_stimulant")
treatdat[varsToFactor] <- lapply(treatdat[varsToFactor], factor)

covariates <- c("age", "year", "edu","inc","source","hospital","fh_suicidal","fh_depr", "female",
                "anxiety_combined", "eat_combined", "med_antipsychotic", "suds", "behav",
                "diag_adhd", "diag_intellectual_disability", "diag_autism",
                "pois", "diag_suicidal", "sedative",
                "med_antiepileptic", "med_stimulant")

#raw data descriptive stats
Unadjusted <- CreateTableOne(vars = covariates, data=treatdat)
Unadjusted
