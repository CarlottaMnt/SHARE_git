setwd("C:/Users/carlotta/Desktop/Dropbox/SHARE_new/R")
setwd("//crc/Team_work/Caralpvk/SHARE_new/R")

#PACKAGES

library(caret)
library(tidyverse)
library(dplyr)
library(fastDummies)
library(naniar)
######################################################################################################
##################################### gl.RData #######################################################
######################################################################################################
load("Dataset_constructed/sharelife_new.RData")
#Work environement
 #Working environment
#frequency tables

work_env <- sharelife_new %>%
dplyr::select(
	mergeid,
	yrbirth,
	country,
	int_year,
	sl_wq002_,#PHYSICALLY DEMANDING 
	sl_wq003_,#UNCOMFORTABLE 
	sl_wq004_,#Under constant time pressure
	sl_wq005_,#EMOTIONALLY DEMANDING 
	sl_wq006_,#INVOLVED CONFLICTS 
	sl_wq007_,#LITTLE FREEDOM TO DECIDE 
	sl_wq008_,#ALLOWED DEVELOPMENT OF SKILLS 
	sl_wq009_,#GAVE RECOGNITION 
	sl_wq010_,#ADEQUATE SALARY 
	sl_wq011_,#ADEQUATE SUPPORT 
	sl_wq012_,#good atmosphere 
	sl_wq013_,#EMPLOYEES TREATED FAIR 
	sl_wq014_#state HEALTH RISK REDUCED
	) %>%
	mutate(
	physical_demanding_work = case_when(
		grepl("Strongly agree",sl_wq002_)~ 1,
		grepl("Strongly disagree",sl_wq002_)~ 4,
		grepl("Disagree",sl_wq002_)~ 3,
		grepl("Agree",sl_wq002_)~ 2,
		grepl("Don't know",sl_wq002_)~ -2,
		grepl("Refusal",sl_wq002_)~ -1,
		TRUE ~ NA_real_),
	uncomfort_work = case_when(
		grepl("Strongly agree",sl_wq003_)~ 1,
		grepl("Strongly disagree",sl_wq003_)~ 4,
		grepl("Disagree",sl_wq003_)~ 3,
		grepl("Agree",sl_wq003_)~ 2,
		grepl("Don't know",sl_wq003_)~ -2,
		grepl("Refusal",sl_wq003_)~ -1,
		TRUE ~ NA_real_),
	time_pressure_at_work = case_when(
		grepl("Strongly agree",sl_wq004_)~ 1,
		grepl("Strongly disagree",sl_wq004_)~ 4,
		grepl("Disagree",sl_wq004_)~ 3,
		grepl("Agree",sl_wq004_)~ 2,
		grepl("Don't know",sl_wq004_)~ -2,
		grepl("Refusal",sl_wq004_)~ -1,
		TRUE ~ NA_real_),
	emotion_demanding_work = case_when(
		grepl("Strongly agree",  sl_wq005_)~ 1,
		grepl("Strongly disagree",  sl_wq005_)~ 4,
		grepl("Disagree",  sl_wq005_)~ 3,
		grepl("Agree",  sl_wq005_)~ 2,
		grepl("Don't know",  sl_wq005_)~ -2,
		grepl("Refusal",  sl_wq005_)~ -1,
		TRUE ~ NA_real_),
    work_with_conflict = case_when(
		grepl("Strongly agree",  sl_wq006_)~ 1,
		grepl("Strongly disagree",  sl_wq006_)~ 4,
		grepl("Disagree",  sl_wq006_)~ 3,
		grepl("Agree",  sl_wq006_)~ 2,
		grepl("Don't know",  sl_wq006_)~ -2,
		grepl("Refusal",  sl_wq006_)~ -1,
		TRUE ~ NA_real_),
	little_freedom_work = case_when(
		grepl("Strongly agree",  sl_wq007_)~ 1,
		grepl("Strongly disagree",  sl_wq007_)~ 4,
		grepl("Disagree",  sl_wq007_)~ 3,
		grepl("Agree",  sl_wq007_)~ 2,
		grepl("Don't know",  sl_wq007_)~ -2,
		grepl("Refusal",  sl_wq007_)~ -1,
		TRUE ~ NA_real_),
	develop_skill_work = case_when(
		grepl("Strongly agree",  sl_wq008_)~ 1,
		grepl("Strongly disagree",  sl_wq008_)~ 4,
		grepl("Disagree",  sl_wq008_)~ 3,
		grepl("Agree",  sl_wq008_)~ 2,
		grepl("Don't know",  sl_wq008_)~ -2,
		grepl("Refusal",  sl_wq008_)~ -1,
		TRUE ~ NA_real_),
	gave_recognition_work = case_when(
		grepl("Strongly agree",  sl_wq009_)~ 1,
		grepl("Strongly disagree",  sl_wq009_)~ 4,
		grepl("Disagree",  sl_wq009_)~ 3,
		grepl("Agree",  sl_wq009_)~ 2,
		grepl("Don't know",  sl_wq009_)~ -2,
		grepl("Refusal",  sl_wq009_)~ -1,
		TRUE ~ NA_real_),
	adequate_salary = case_when(
		grepl("Strongly agree",  sl_wq010_)~ 1,
		grepl("Strongly disagree",  sl_wq010_)~ 4,
		grepl("Disagree",  sl_wq010_)~ 3,
		grepl("Agree",  sl_wq010_)~ 2,
		grepl("Don't know",  sl_wq010_)~ -2,
		grepl("Refusal",  sl_wq010_)~ -1,
		TRUE ~ NA_real_),
	adequate_support_work = case_when(
		grepl("Strongly agree",  sl_wq011_)~ 1,
		grepl("Strongly disagree",  sl_wq011_)~ 4,
		grepl("Disagree",  sl_wq011_)~ 3,
		grepl("Agree",  sl_wq011_)~ 2,
		grepl("Don't know",  sl_wq011_)~ -2,
		grepl("Refusal",  sl_wq011_)~ -1,
		TRUE ~ NA_real_),
	good_atmosphere_work = case_when(
		grepl("Strongly agree",  sl_wq012_)~ 1,
		grepl("Strongly disagree",  sl_wq012_)~ 4,
		grepl("Disagree",  sl_wq012_)~ 3,
		grepl("Agree",  sl_wq012_)~ 2,
		grepl("Don't know",  sl_wq012_)~ -2,
		grepl("Refusal",  sl_wq012_)~ -1,
		TRUE ~ NA_real_),
	treated_fair_at_work = case_when(
		grepl("Strongly agree",  sl_wq013_)~ 1,
		grepl("Strongly disagree",  sl_wq013_)~ 4,
		grepl("Disagree",  sl_wq013_)~ 3,
		grepl("Agree",  sl_wq013_)~ 2,
		grepl("Don't know",  sl_wq013_)~ -2,
		grepl("Refusal",  sl_wq013_)~ -1,
		TRUE ~ NA_real_),
	state_protect_health_at_work = case_when(
		grepl("Strongly agree",  sl_wq014_)~ 1,
		grepl("Strongly disagree",  sl_wq014_)~ 4,
		grepl("Disagree",  sl_wq014_)~ 3,
		grepl("Agree",  sl_wq014_)~ 2,
		grepl("Don't know",  sl_wq014_)~ -2,
		grepl("Refusal",  sl_wq014_)~ -1,
		TRUE ~ NA_real_)
		)%>%
		dplyr::select(-c(sl_wq002_:sl_wq014_))
save(work_env,file = "Dataset_constructed/work_env.RData")

gg_miss_var(work_env, facet =int_year, show_pct = TRUE)

#Frequency tables
graphdf <- work_env %>%
 gather(Variable,Value,-c(mergeid,yrbirth,country,int_year))
graphdf$Value <- factor(graphdf$Value ,
levels = c(-1,-2,1,2,3,4),
labels = c("Refusal","Don't know","Strongly agree", "Agree", "Disagree","Strongly disagree"))
 
ggplot(graphdf,aes((Value)))+ geom_bar()+facet_wrap(~Variable)+theme(axis.text.x = element_text(angle = 90))