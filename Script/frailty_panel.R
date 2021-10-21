setwd("C:/Users/carlotta/Desktop/Dropbox/SHARE_new/R")
setwd("//crc/Team_work/Caralpvk/SHARE_new/R")
#IMPORT THE REQUIRED PACKAGES-----------------------------------------------------------------
#install.packages("knitr")# This is used for creatinkg latex tables
#install.packages("bestNormalize")# This may be used for normalizing variables
#install.packages("xtable")# Other package for latex
#install.packages("foreign") ##Package for importing dta files
#install.packages("here")
#install.packages("tidyverse")
#install.packages("foreign")
#install.packages("gridExtra")
#install.packages("knitr")
#install.packages("plyr")
#install.packages("MCMCpack")
#install.packages("corrr")
install.packages("qwraps2")
# install.packages("psych")
# install.packages("surveysummary")
# install.packages("stargazer")
# install.packages("car")
# install.packages("tables")
# install.packages("Blackmore")
# install.packages("fastDummies")
# install.packages("tmaptools")
# install.packages("tmap") 
# install.packages("sf")
#Libraries-------------------------------------------------------------------------
#Data analysis
library(tidyverse)
library(plyr)
library(fastDummies) 
library(foreign)
#Standardize 
library(bestNormalize)
library(car)
#Tables
library(xtable)
library(gtable)
library(stargazer)
library(tables)
library(knitr)
library(data.table)
#Scatter and correlation plot
library(psych)
library(corrplot)
#Missing data 
library(naniar)
library(VIM)
#Plots
library(grid)
library(gridExtra)
library(surveysummary)
#Maps
library(sf)
library(tmap)
library(tmaptools)
################################################################################
################################################################################
############################ frailty ###########################################
################################################################################
################################################################################
rm(list=ls())
#Import the required dataset from the folder "~/Dataset_constructed/." and "~/panel/."

#Dataset with sharelife respondents (N = 57714) , excluding Ireland and proxy respondents.
#This is created in the script "sharelife_merge".

load("Dataset_constructed/sharelife_new.RData") 

#This is created in the script "~/Old_script/phsicaldfdf".
load("panel/physicaldf.RData")

#This is created in the script "~/Old_script/gvhealth".
load("panel/healthdf.Rdata")

#This is created in the script "~/Old_script/cvdf". We will use use to gather info on age and gender of the respondent
load("panel/CVdf.Rdata")

#Create the dataframe with only the variable required for constructing the frailty index.
#The selected deficit are taken from Romero paper "The frailty index in Europeans: association with age and mortality" supplementary material.

ph_panel <- physicaldf %>%
	dplyr::select(
		mergeid,
		country,
		wave,
		ph049d1:ph049d13,
		ph048d1:ph048d9,
		ph004_,
		ph005_,
		ph006d1:ph006d6,
		ph006d10:ph006d14
	) %>%
	mutate(

#Difficulties:

#Dressing, including putting on shoes and socks 
		ph049d1 = case_when(
		  grepl("Refusal",ph049d1) ~ as.numeric(-2),
		  grepl("Don't know",ph049d1) ~ as.numeric(-1),
		  grepl("Selected",ph049d1) ~ as.numeric(1),
		  grepl("Not selected",ph049d1) ~ as.numeric(0),
		  TRUE ~ NA_real_),

# Walking across a room 
		ph049d2 = case_when(
		  grepl("Refusal",ph049d2) ~ as.numeric(-2),
		  grepl("Don't know",ph049d2) ~ as.numeric(-1),
		  grepl("Selected",ph049d2) ~ as.numeric(1),
		  grepl("Not selected",ph049d2) ~ as.numeric(0),
		  TRUE ~ NA_real_),

#Bathing or showering 
		ph049d3 = case_when(
		  grepl("Refusal",ph049d3) ~ as.numeric(-2),
		  grepl("Don't know",ph049d3) ~ as.numeric(-1),
		  grepl("Selected",ph049d3) ~ as.numeric(1),
		  grepl("Not selected",ph049d3) ~ as.numeric(0),
		  TRUE ~ NA_real_),

#Eating, such as cutting up your food 
		ph049d4 = case_when(
		  grepl("Refusal",ph049d4) ~ as.numeric(-2),
		  grepl("Don't know",ph049d4) ~ as.numeric(-1),
		  grepl("Selected", ph049d4) ~ as.numeric(1),
		  grepl("Not selected", ph049d4) ~ as.numeric(0),
		  TRUE ~ NA_real_),

#Getting in or out of bed 
		ph049d5 = case_when(
		  grepl("Refusal",ph049d5) ~ as.numeric(-2),
		  grepl("Don't know",ph049d5) ~ as.numeric(-1),
		  grepl("Selected",ph049d5) ~ as.numeric(1),
		  grepl("Not selected",ph049d5) ~ as.numeric(0),
		 TRUE ~ NA_real_),

#Using the toilet, including getting up or down 
		ph049d6 = case_when(
		  grepl("Refusal",ph049d6) ~ as.numeric(-2),
		  grepl("Don't know",ph049d6) ~ as.numeric(-1),
		  grepl("Selected",ph049d6) ~ as.numeric(1),
		  grepl("Not selected",ph049d6) ~ as.numeric(0),
		TRUE ~ NA_real_),

#Using a map to figure out how to get around in a strange place 
		ph049d7 = case_when(
		  grepl("Refusal",ph049d7) ~ as.numeric(-2),
		  grepl("Don't know",ph049d7) ~ as.numeric(-1),
		  grepl("Selected",ph049d7) ~ as.numeric(1),
		  grepl("Not selected",ph049d7) ~ as.numeric(0),
		TRUE ~ NA_real_),

#Preparing a hot meal 
		ph049d8 = case_when(
		  grepl("Refusal",ph049d8) ~ as.numeric(-2),
		  grepl("Don't know",ph049d8) ~ as.numeric(-1),
		  grepl("Selected",ph049d8) ~ as.numeric(1),
		  grepl("Not selected",ph049d8) ~ as.numeric(0),
		TRUE ~ NA_real_),

#Shopping for groceries 
		ph049d9 = case_when(
		  grepl("Refusal",ph049d9) ~ as.numeric(-2),
		  grepl("Don't know",ph049d9) ~ as.numeric(-1),
		  grepl("Selected",ph049d9) ~ as.numeric(1),
		  grepl("Not selected",ph049d9) ~ as.numeric(0),
		TRUE ~ NA_real_),

#Making telephone calls 
		ph049d10 = case_when(
		  grepl("Refusal",ph049d10) ~ as.numeric(-2),
		  grepl("Don't know",ph049d10) ~ as.numeric(-1),
		  grepl("Selected",ph049d10) ~ as.numeric(1),
		  grepl("Not selected",ph049d10) ~ as.numeric(0),
		TRUE ~ NA_real_),

#Taking medications 
		ph049d11 = case_when(
		  grepl("Refusal",ph049d11) ~ as.numeric(-2),
		  grepl("Don't know",ph049d11) ~ as.numeric(-1),
		  grepl("Selected",ph049d11) ~ as.numeric(1),
		  grepl("Not selected",ph049d11) ~ as.numeric(0),
		TRUE ~ NA_real_),

#Doing work around the house or garden 
		ph049d12 = case_when(
		  grepl("Refusal",ph049d12) ~ as.numeric(-2),
		  grepl("Don't know",ph049d12) ~ as.numeric(-1),
		  grepl("Selected",ph049d12) ~ as.numeric(1),
		  grepl("Not selected",ph049d12) ~ as.numeric(0),
		TRUE ~ NA_real_),

# Managing money, such as paying bills and keeping track of expenses 
		ph049d13 = case_when(
		  grepl("Refusal",ph049d13) ~ as.numeric(-2),
		  grepl("Don't know",ph049d13) ~ as.numeric(-1),
		  grepl("Selected",ph049d13) ~ as.numeric(1),
		  grepl("Not selected",ph049d13) ~ as.numeric(0),
		  TRUE ~ NA_real_),
		  
#More difficulties:

#Walking 100 metres 
		ph048d1 = case_when(
		  grepl("Refusal",ph048d1) ~ as.numeric(-2),
		  grepl("Don't know",ph048d1) ~ as.numeric(-1),
		  grepl("Selected",ph048d1) ~ as.numeric(1),
		  grepl("Not selected",ph048d1) ~ as.numeric(0),
		TRUE ~ NA_real_),

#Sitting for about two hours 
		ph048d2 = case_when(
		  grepl("Refusal",ph048d2) ~ as.numeric(-2),
		  grepl("Don't know",ph048d2) ~ as.numeric(-1),
		  grepl("Selected",ph048d2) ~ as.numeric(1),
		  grepl("Not selected",ph048d2) ~ as.numeric(0),
		TRUE ~ NA_real_),

#Getting up from a chair after sitting for long periods 
		ph048d3 = case_when(
		  grepl("Refusal",ph048d3) ~ as.numeric(-2),
		  grepl("Don't know",ph048d3) ~ as.numeric(-1),
		  grepl("Selected",ph048d3) ~ as.numeric(1),
		  grepl("Not selected",ph048d3) ~ as.numeric(0),
		TRUE ~ NA_real_),

#Climbing several flights of stairs without resting 
		ph048d4 = case_when(
		  grepl("Refusal",ph048d4) ~ as.numeric(-2),
		  grepl("Don't know",ph048d4) ~ as.numeric(-1),
		  grepl("Selected",ph048d4) ~ as.numeric(1),
		  grepl("Not selected",ph048d4) ~ as.numeric(0),
		TRUE ~ NA_real_),

#Climbing one flight of stairs without resting 
		ph048d5 = case_when(
		  grepl("Refusal",ph048d5) ~ as.numeric(-2),
		  grepl("Don't know",ph048d5) ~ as.numeric(-1),
		  grepl("Selected",ph048d5) ~ as.numeric(1),
		  grepl("Not selected",ph048d5) ~ as.numeric(0),
		TRUE ~ NA_real_),

# Stooping, kneeling, or crouching 
		ph048d6 = case_when(
		  grepl("Refusal",ph048d6) ~ as.numeric(-2),
		  grepl("Don't know",ph048d6) ~ as.numeric(-1),
		  grepl("Selected",ph048d6) ~ as.numeric(1),
		  grepl("Not selected",ph048d6) ~ as.numeric(0),
		TRUE ~ NA_real_),

#Reaching or extending your arms above shoulder level 
		ph048d7 = case_when(
		  grepl("Refusal",ph048d7) ~ as.numeric(-2),
		  grepl("Don't know",ph048d7) ~ as.numeric(-1),
		  grepl("Selected",ph048d7) ~ as.numeric(1),
		  grepl("Not selected",ph048d7) ~ as.numeric(0),
		TRUE ~ NA_real_),

#Pulling or pushing large objects like a living room chair 
		ph048d8 = case_when(
		  grepl("Refusal",ph048d8) ~ as.numeric(-2),
		  grepl("Don't know",ph048d8) ~ as.numeric(-1),
		  grepl("Selected",ph048d8) ~ as.numeric(1),
		  grepl("Not selected",ph048d8) ~ as.numeric(0),
		TRUE ~ NA_real_),

#Lifting or carrying weights over 10 pounds/5 kilos, like a heavy bag of groceries 
		ph048d9 = case_when(
		  grepl("Refusal",ph048d9) ~ as.numeric(-2),
		  grepl("Don't know",ph048d9) ~ as.numeric(-1),
		  grepl("Selected",ph048d9) ~ as.numeric(1),
		  grepl("Not selected",ph048d9) ~ as.numeric(0),
		TRUE ~ NA_real_),

#LONG-TERM ILLNESS:

#Do you have any such health problems, illness, disability or infirmity?
		ph004_ = case_when(
		  grepl("Refusal",ph004_) ~ as.numeric(-2),
		  grepl("Don't know",ph004_) ~ as.numeric(-1),
		  grepl("Yes",ph004_) ~ as.numeric(1),
		  grepl("No",ph004_) ~ as.numeric(0),
		TRUE ~ NA_real_),

#LIMITED ACTIVITIES:

#To what extent have you been limited because of a health problem in activities people usually?
		ph005_ = case_when(
		  grepl("Refusal",ph005_) ~ as.numeric(-2),
		  grepl("Don't know",ph005_) ~ as.numeric(-1),
		  grepl("Limited, but not severely",ph005_) ~ as.numeric(1),
		  grepl("Severely limited",ph005_) ~ as.numeric(1),
		  grepl("Not limited",ph005_) ~ as.numeric(0),
		TRUE ~ NA_real_),

#DOCTOR TOLD YOU HAD CONDITIONS:

#A heart attack including myocardial 
		ph006d1 = case_when(
		  grepl("Refusal",ph006d1 ) ~ as.numeric(-2),
		  grepl("Don't know",ph006d1) ~ as.numeric(-1),
		  grepl("Selected", ph006d1) ~ as.numeric(1),
		  grepl("Not selected", ph006d1) ~ as.numeric(0),
		TRUE ~ NA_real_),

#High blood pressure or hypertension 
		ph006d2 = case_when(
		  grepl("Refusal",ph006d2) ~ as.numeric(-2),
		  grepl("Don't know",ph006d2) ~ as.numeric(-1),
		  grepl("Selected", ph006d2) ~ as.numeric(1),
		  grepl("Not selected", ph006d2) ~ as.numeric(0),
		TRUE ~ NA_real_),

#High blood cholesterol 
		ph006d3 = case_when(
		  grepl("Refusal",ph006d3) ~ as.numeric(-2),
		  grepl("Don't know",ph006d3) ~ as.numeric(-1),
		  grepl("Selected", ph006d3) ~ as.numeric(1),
		  grepl("Not selected", ph006d3) ~ as.numeric(0),
		TRUE ~ NA_real_),

#A stroke or cerebral vascular disease 
		ph006d4 = case_when(
		  grepl("Refusal",ph006d4) ~ as.numeric(-2),
		  grepl("Don't know",ph006d4) ~ as.numeric(-1),
		  grepl("Selected", ph006d4) ~ as.numeric(1),
		  grepl("Not selected", ph006d4) ~ as.numeric(0),
		TRUE ~ NA_real_),

#Diabetes or high blood sugar 
		ph006d5 = case_when(
		  grepl("Refusal",ph006d5) ~ as.numeric(-2),
		  grepl("Don't know",ph006d5) ~ as.numeric(-1),
		  grepl("Selected", ph006d5) ~ as.numeric(1),
		  grepl("Not selected", ph006d5) ~ as.numeric(0),
		TRUE ~ NA_real_),

#Chronic lung disease 
		ph006d6 = case_when(
		  grepl("Refusal",ph006d6) ~ as.numeric(-2),
		  grepl("Don't know",ph006d6) ~ as.numeric(-1),
		  grepl("Selected", ph006d6) ~ as.numeric(1),
		  grepl("Not selected", ph006d6) ~ as.numeric(0), 
		TRUE ~ NA_real_),

# Cancer or malignant tumour
		ph006d10 = case_when(
		  grepl("Refusal",ph006d10) ~ as.numeric(-2),
		  grepl("Don't know",ph006d10) ~ as.numeric(-1),
		  grepl("Selected", ph006d10) ~ as.numeric(1),
		  grepl("Not selected", ph006d10) ~ as.numeric(0),
		TRUE ~ NA_real_),

#Stomach or duodenal ulcer
		ph006d11 = case_when(
		  grepl("Refusal",ph006d11) ~ as.numeric(-2),
		  grepl("Don't know",ph006d11) ~ as.numeric(-1),
		  grepl("Selected", ph006d11) ~ as.numeric(1),
		  grepl("Not selected", ph006d11) ~ as.numeric(0),
		TRUE ~ NA_real_),

#Parkinson disease 
		ph006d12 = case_when(
		  grepl("Refusal",ph006d12) ~ as.numeric(-2),
		  grepl("Don't know",ph006d12) ~ as.numeric(-1),
		  grepl("Selected", ph006d12) ~ as.numeric(1),
		  grepl("Not selected", ph006d12) ~ as.numeric(0),
		TRUE ~ NA_real_),

#Cataracts 
		ph006d13 = case_when(
		  grepl("Refusal",ph006d13) ~ as.numeric(-2),
		  grepl("Don't know",ph006d13) ~ as.numeric(-1),
		  grepl("Selected", ph006d13) ~ as.numeric(1),
		  grepl("Not selected", ph006d13) ~ as.numeric(0),
		TRUE ~ NA_real_),

# Hip fracture 
		ph006d14 = case_when(
		  grepl("Refusal",ph006d14) ~ as.numeric(-2),
		  grepl("Don't know",ph006d14) ~ as.numeric(-1),
		  grepl("Selected", ph006d14) ~ as.numeric(1),
		  grepl("Not selected", ph006d14) ~ as.numeric(0),
		TRUE ~ NA_real_)
	)

#Here I create another dataframe with additional variable to be used in the construction of the frailty index. 
#GV_health imputation module merged with the cvmodule.
#The imputation in GV health is reliable? 

cv_panel <- CVdf %>%
	filter(mergeid %in% healthdf$mergeid) %>%
	dplyr::select(
		mergeid,
		age_int,
		wave,
		int_year,
		gender,
		int_month
	) %>%
	mutate(
	season = ifelse(
	int_month %in% c("June","July","August"), "Summer",
	ifelse(int_month %in% c("September","October", "November"), "Autumn",
	ifelse(int_month %in% c("December","January","February"),"Winter",
	ifelse(int_month %in% c("March","April","May"),"Spring",NA_character_)))))

hth_panel <- merge(healthdf,cv_panel, by = c("mergeid","wave"), all = TRUE)  %>%
	select(
	  mergeid,
	  wave,
	  gender,
	  age_int,
	  int_year,
	  season,
	  bmi,     #body mass index
	  orienti, #orientation to date, month, year and day of week
	  mobility,#mobility, arm function and fine motor limitation
	  phactiv, #physical inactivity
	  maxgrip  #maximum of grip strenght
	) %>%
	mutate(
		bmi = case_when(
			grepl(-3, bmi) ~ NA_real_,
			TRUE ~ as.numeric(bmi)),
		phactiv = case_when(
			grepl("Refusal", phactiv) ~ as.numeric(-2),
			grepl("Don't Know", phactiv)~ as.numeric(-1),
			grepl("Never vigorous nor moderate physical activity", phactiv)~ as.numeric(0),
			grepl("Other", phactiv)~ as.numeric(1),
			TRUE ~ NA_real_),
		mobility = ifelse(mobility < 0, mobility, ifelse(mobility >= 3, as.numeric(1),ifelse((mobility >= 1 & mobility < 3),as.numeric(0.5),as.numeric(0)))),
		maxgrip  = ifelse(maxgrip  < 0, maxgrip, ifelse((gender == "Male" & maxgrip <= 29 & bmi<=24),as.numeric(1),
		ifelse((gender == "Male" & maxgrip <= 30 & bmi>=24 & bmi <= 28),as.numeric(1),ifelse((gender == "Male"& maxgrip <= 32 & bmi > 28),as.numeric(1),
		ifelse((gender == "Female" & maxgrip <=29 & bmi <= 24), as.numeric(1),ifelse((gender == "Female"& maxgrip <= 30 & bmi>=24.1 & bmi <=28), as.numeric(1),
		ifelse((gender == "Female" & maxgrip <=32 & bmi>28),as.numeric(1),as.numeric(0)))))))),
		orienti = ifelse(orienti < 0, orienti, ifelse(orienti == 4, as.numeric(0),ifelse((orienti > 2 & orienti <= 3), as.numeric(0.5),as.numeric(1))))
	) %>% 
  mutate(
  bmi = ifelse(bmi < 0, bmi, ifelse((bmi <= 18.5 | bmi >30),as.numeric(1),ifelse((bmi >= 25 & bmi <30), as.numeric(0.5),as.numeric(0))))
  )
#Now that we have both dataframe we are ready to merge them into a single dataframe a create our frailty_index in SHARE.
#Moreover, we compute for each respondent the perc_uncertainty which relates to the (un)informativeness of each respondent. 
#Average over the overall number of items of missing value, refusal and don't know responses for each respondent. 

frailty_panel <- merge(hth_panel,ph_panel, by = c("mergeid","wave")) %>%
	dplyr::select(
		mergeid,
		wave,
		int_year,
		country,
		gender,
		age_int,
		season,
		int_year,
		bmi,     #body mass index
		orienti, #orientation to date, month, year and day of week
		mobility,#mobility, arm function and fine motor limitation
		phactiv, #physical inactivity
		maxgrip,  #maximum of grip strenght
		everything()
	) %>%
	dplyr::mutate(
		N_Na_frailty = apply(., 1, function(x) sum(is.na(x))),
		N_Refus_frailty = apply(., 1, function(x) sum(x == -2, na.rm = TRUE)),
		N_dont_frailty = apply(., 1, function(x) sum(x == -1, na.rm = TRUE)),
		perc_uncertainty_frailty = round((N_Na_frailty + N_Refus_frailty + N_dont_frailty)/dim(.)[2],2)
	) %>%
	dplyr::select(
		mergeid,
		wave,
		gender, 
		age_int, 
		int_year,
		country,
		season,
		N_Na_frailty,
		N_Refus_frailty,
		N_dont_frailty,
		perc_uncertainty_frailty,
		everything()
	) %>%
	dplyr::mutate(
		frailty_index = apply(.[,-c(1:11)],1, function(x) mean(x[x >= 0], na.rm = TRUE))
	) %>%
	dplyr::group_by(mergeid) %>%
	dplyr::mutate(
		duration_frailty = int_year - min(int_year), #duration in the panel 
		count_frailty = n() #number of observation in the panel 
	) %>%
	ungroup()

#Here we filter out only SHARELIFE respondents from mergeid in sharelife_new 
frailtySL_panel <- frailty_panel %>%
 	filter(
		mergeid %in% sharelife_new$mergeid,
	) 


#Which Sharelife respondents have not imputed values in gv_health imputation?
summary(sharelife_new[which(!(sharelife_new$mergeid %in% healthdf$mergeid)),]$firstwave)

#Here we select the respondent responses closer to the sharelife questionnaire and we merge the data set with the SL cross sectional weights
#Here we have still respondents younger than 50. 
frailtySL <- frailtySL_panel %>%
	group_by(mergeid) %>%
	dplyr::slice(ifelse((last(perc_uncertainty_frailty)> 0.5 & last(count_frailty) != 1), n()-1, n())) %>%  #select the last observation available and wih less uncertainy
	ungroup() %>%
	merge(., sharelife_new[,c("mergeid","cciw")],by = "mergeid", all = FALSE) %>%
	dplyr::select(
		mergeid,
		cciw, 
		everything()
	)

#Save data set
save(frailty_panel,file = "panel/frailty_panel.RData")
save(frailtySL_panel,file="panel/frailtySL_panel.RData")
save(frailtySL,file="Dataset_constructed/frailtySL.RData")

#Here we explore the cross-sectional SL constructed dataset with cross-sectional weight
 attach(frailtySL)
 y <- frailty_index[frailty_index >= 0]
 x <- season[frailty_index >=0]
 
 summary(lm(y ~ x -1))
#Tables 
#Descriptive statistics unweighted
stargazer(frailtySL)
#Descriptive statistics weighted

#Plots

#Boxplot
ggplot(frailtySL, aes(x=frailty_index, y=as.factor(country), fill = gender))+
geom_boxplot(alpha=0.8, position = "dodge")+
xlab("Frailty")+
ylab("Country")+
guides(fill = guide_legend(reverse = TRUE))
#ggsave("frailty_boxplot.png")

#Smooth regression
ggplot(frailtySL, aes(y = frailty_index,x = age_int, color = gender)) + geom_smooth(alpha =1.4) + facet_wrap(vars(country)) 
ggsave("frailty_smoothSL.png")

#Histogram
ggplot(frailtySL, aes(x = frailty_index, fill = gender, color = gender)) + geom_histogram(bins = 40) + facet_wrap(vars(country)) 
ggsave("frailty_smoothSL.png")

#Plots of Uncertainty in cross-sectional SL
#1: Distribution by country of Refusal don't know and missing
frailtySL %>%
 select(
	country,
	N_Na,
	N_Refus,
	N_dont,
	perc_uncertainty
	)%>%
 group_by(country) %>%
 mutate(country_uncertainty  = mean(perc_uncertainty, na.rm = TRUE)) %>%
 ungroup() %>%
 select(-c(perc_uncertainty,country_uncertainty)) %>%
 gather(Variable, values,-c(country)) %>%
 ggplot(.,aes(values,Variable, color = Variable)) + 
 geom_boxplot() +
 facet_wrap(vars(country))+
 ggtitle("Number of missing item by country and category of missing")+
 theme(legend.title = element_blank())
 