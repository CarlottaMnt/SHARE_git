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
# install.packages("qwraps2")
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
library(dplyr)
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
library(ggplot)
#Scatter and correlation plot
library(psych)
library(corrplot)
library(fastDummies)
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
############################ life_satisfaction and CASP ########################
################################################################################
################################################################################
rm(list=ls())
#Import the required dataset from the folder "~/Dataset_constructed/." and "~/panel/."
#Dataset with sharelife respondents (N = 57714) , excluding Ireland and proxy respondents. Be aware that this includes still respondent < 50.
#This is created in the script "sharelife_merge".

load("Dataset_constructed/sharelife_new.RData") 

#This is created in the script "~/Old_script/activitdf".

load("panel/activitdf.RData")

#This is created in the script "~/Old_script/cvdf". We will use use to gather info on age and gender of the respondent

load("panel/CVdf.Rdata")

lsat <- activitdf %>%
    dplyr::select(
		 mergeid,
		 wave,
		 country,
#On a scale from 0 to 10 where 0 means completely dissatisfied and 10 means completely satisfied, how satisfied are you with your life?
		 ac012_,
#Number of activities month(W1-W2): the typlogy of activities could be mediator of life satisfaction
		 ac002d1:ac002dno, 
#Number of activities year(W4-W7)	 
		 ac035d1:ac035dno
	)%>%  
	mutate(
		N_Na_lsat = apply(.[,-c(1:2)], 1, function(x) sum(is.na(x))),
		N_WaveMiss_lsat = apply(.[,-c(1:2)], 1, function(x) sum(x == -11, na.rm = TRUE)),
		N_Refus_lsat = apply(.[,-c(1:2)], 1, function(x) sum(x == "Refusal", na.rm = TRUE)),
		N_dont_lsat = apply(.[,-c(1:2)], 1, function(x) sum(x == "Don't know", na.rm = TRUE)),
		perc_uncertainty_lsat = round((N_Na_lsat + N_Refus_lsat + N_dont_lsat + N_WaveMiss_lsat)/(dim(.)[2]-2),2),
		lifesat = as.factor(ac012_),
#Individual resources that may mediate over the life satisfaction
		activities = case_when(
			grepl("Selected", ac035d1) ~ "charity",
			grepl("Selected", ac035d4) ~ "training_course",
			grepl("Selected", ac035d5) ~ "sport_club",
			grepl("Selected", ac035d7) ~ "pol_comm_org",
			grepl("Selected", ac035d8) ~ "read_book",
			grepl("Selected", ac035d9) ~ "sudoku",
			grepl("Selected", ac035d10)~ "cards_chess",
			grepl("Selected", ac002d1) ~ "charity",
			grepl("Selected", ac002d2) ~ "care_to_sick",
			grepl("Selected", ac002d3) ~ "help_friends",
			grepl("Selected", ac002d4) ~ "training_course",
			grepl("Selected", ac002d5) ~ "sport_club",
			grepl("Selected", ac002d6) ~ "religious_org",
			grepl("Selected", ac002d7) ~ "pol_comm_org",
			grepl("Selected", ac002dno) ~ "none",
			TRUE ~ NA_character_),
		charity = case_when(
			grepl("Selected", ac035d1) ~ as.numeric(1),
			grepl("Not selected", ac035d1) ~ as.numeric(0),
			grepl("Refusal",ac035d1)~ as.numeric(-2),
		    grepl("Don't know",ac035d1)~ as.numeric(-1),
			
			grepl("Selected", ac002d1) ~ as.numeric(1),
			grepl("Not selected", ac002d1) ~ as.numeric(0),
			grepl("Refusal",ac002d1)~ as.numeric(-2),
		    grepl("Don't know",ac002d1)~ as.numeric(-1),
			TRUE ~ NA_real_),
			
		training = case_when(
			grepl("Selected",ac035d4) ~ as.numeric(1),
			grepl("Not selected", ac035d4) ~ as.numeric(0),
			grepl("Refusal",ac035d4)~ as.numeric(-2),
		    grepl("Don't know",ac035d4)~ as.numeric(-1),
			
			grepl("Selected",ac002d4) ~ as.numeric(1),
			grepl("Not selected", ac002d4) ~ as.numeric(0),
			grepl("Refusal",ac002d4)~ as.numeric(-2),
		    grepl("Don't know",ac002d4)~ as.numeric(-1),
			TRUE ~ NA_real_),
			
		sport_club = case_when(
			grepl("Selected", ac035d5) ~ as.numeric(1),
			grepl("Not selected", ac035d5) ~ as.numeric(0),
			grepl("Refusal", ac035d5)~ as.numeric(-2),
		    grepl("Don't know", ac035d5)~ as.numeric(-1),
			
			grepl("Selected", ac002d5) ~ as.numeric(1),
			grepl("Not selected", ac002d5) ~ as.numeric(0),
			grepl("Refusal", ac002d5)~ as.numeric(-2),
		    grepl("Don't know", ac002d5)~ as.numeric(-1),
			TRUE ~ NA_real_),
			
		political_org =  case_when(
			grepl("Selected", ac035d7) ~ as.numeric(1),
			grepl("Not selected", ac035d7) ~ as.numeric(0),
			grepl("Refusal", ac035d7)~ as.numeric(-2),
		    grepl("Don't know", ac035d7)~ as.numeric(-1),			
			grepl("Selected", ac002d7) ~ as.numeric(1),
			grepl("Not selected", ac002d7) ~ as.numeric(0),
			grepl("Refusal", ac002d7)~ as.numeric(-2),
		    grepl("Don't know", ac002d7)~ as.numeric(-1),		
			TRUE ~ NA_real_),
			
		read_book = case_when(
			grepl("Selected", ac035d8) ~ as.numeric(1),
			grepl("Not selected", ac035d8) ~ as.numeric(0),
			grepl("Refusal", ac035d8)~ as.numeric(-2),
		    grepl("Don't know", ac035d8)~ as.numeric(-1),
			TRUE ~ NA_real_),	
			
		sudoku = case_when(
			grepl("Selected", ac035d9) ~ as.numeric(1),
			grepl("Not selected", ac035d9) ~ as.numeric(0),
			grepl("Refusal",ac035d9)~ as.numeric(-2),
		    grepl("Don't know",ac035d9)~ as.numeric(-1),
			TRUE ~ NA_real_),	
			
		card_chess = case_when(
			grepl("Selected", ac035d10) ~ as.numeric(1),
			grepl("Not selected", ac035d10) ~ as.numeric(0),
			grepl("Refusal", ac035d10) ~ as.numeric(-2),
		    grepl("Don't know", ac035d10) ~ as.numeric(-1),
			TRUE ~ NA_real_),	
			
		care_to_sick = case_when(
			grepl("Selected",  ac002d2) ~ as.numeric(1),
			grepl("Not selected",  ac002d2) ~ as.numeric(0),
			grepl("Refusal", ac002d2)~ as.numeric(-2),
		    grepl("Don't know", ac002d2)~ as.numeric(-1),
			TRUE ~ NA_real_),
			
		help_friends = case_when(
			grepl("Selected", ac002d3) ~ as.numeric(1),
			grepl("Not selected",ac002d3) ~ as.numeric(0),
			grepl("Refusal",ac002d3)~ as.numeric(-2),
		    grepl("Don't know",ac002d3)~ as.numeric(-1),
			TRUE ~ NA_real_),

		religious_org =  case_when(
			grepl("Selected", ac002d6) ~ as.numeric(1),
			grepl("Not selected", ac002d6) ~ as.numeric(0),
			grepl("Refusal", ac002d6)~ as.numeric(-2),
		    grepl("Don't know", ac002d6)~ as.numeric(-1),
			TRUE ~ NA_real_),
		
		# no_activities = case_when(
			# grepl("Selected", ac035dno) ~ as.numeric(1),
			# grepl("Not selected",ac035dno) ~ as.numeric(0),
			# grepl("Refusal",ac035dno)~ as.numeric(-2),
		    # grepl("Don't know",ac035dno)~ as.numeric(-1),
			
			# grepl("Selected", ac002dno) ~ as.numeric(1),
			# grepl("Not selected",ac002dno) ~ as.numeric(0),
			# grepl("Refusal",ac002dno)~ as.numeric(-2),
		    # grepl("Don't know",ac002dno)~ as.numeric(-1),
			# TRUE ~ NA_real_),
			) %>%
	select(-c(
	ac002d1:ac002dno,
	ac035d1:ac035dno)
	)%>%
	mutate_if(is.character,as.factor)%>%
	mutate(
	naly = apply(.[,c("charity","training","sport_club","political_org","read_book","sudoku","card_chess","care_to_sick","help_friends","religious_org")],1, function(x) sum(x[x > 0], na.rm = TRUE))
	)

#Now that we have the life satisfaction df we also have for each respondent the perc_uncertainty which relates to the (un)informativeness of each respondent. 
#Average over the overall number of items of missing value, refusal and don't know responses for each respondent in the activit module

#We merge the items df with some socio-demographic characteristic of our respondents: country, age and gender.
cv_panel <- CVdf %>%
	filter(mergeid %in% lsat$mergeid) %>%
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
	

 
#Which respondents have not lifesat items?

#Are these the same that have not frailty items?

head(setdiff(unique(CVdf$mergeid),unique(lsat$mergeid)))

#Which are the less certain items? 
#Which is the certainty thorugh which we measure the mental health?


lifesat_panel <- merge(lsat, cv_panel, by = c("mergeid","wave"), all = FALSE) %>%
	select(
		mergeid,
		wave,
		country,
		int_year,
		age_int,
		gender,
		season,
		N_Na_lsat,
		N_Refus_lsat, 
		N_dont_lsat,
		N_WaveMiss_lsat,
		perc_uncertainty_lsat,
		lifesat,
		naly,
		everything()
	) 

#Panel SL dataset

#Here we filter out from the overall panel only SHARELIFE respondents from sharelife_new 
lifesatSL_panel <- lifesat_panel %>%
	filter(
		mergeid %in% sharelife_new$mergeid,
	) %>%
	group_by(mergeid) %>%
	dplyr::mutate(
		duration_lsat = int_year - min(int_year), #duration in the panel 
		count_lsat = n() #number of observation in the panel 
	) %>%
	ungroup()

#Which Sharelife respondents have not imputed values in lifesat_panel?
summary(sharelife_new[which(!(sharelife_new$mergeid %in% lifesat_panel$mergeid)),]$firstwave)

#Cross-sectional SL dataset
#Here we select the respondent responses closer to the sharelife questionnaire and we merge the data set with the SL cross sectional weights

lifesatSL <- lifesatSL_panel %>%
 group_by(mergeid) %>%
 dplyr::slice(ifelse((last(perc_uncertainty_lsat)> 0.5 & last(count_lsat)!= 1), n()-1, n())) %>% #select the last observation available and wih less uncertainy
 ungroup() %>%
 merge(., sharelife_new[,c("mergeid","cciw")],by = "mergeid", all = FALSE) %>%
 dplyr::select(mergeid, cciw, everything())
 
 
 #Check whether season is relevant for life satisfaction.
 attach(lifesatSL)
 
 y <- ac012_[ac012_ >= 0]
 x <- season[ac012_ >=0]
 
 summary(lm(y ~ x -1))
#save df---------------------------------------------------------
save(lifesat_panel,file = "panel/lifesat_panel.RData")
save(lifesatSL_panel,file="panel/lifesatSL_panel.RData")
save(lifesatSL,file="Dataset_constructed/lifesatSL.RData")
#Here we explore the cross-sectional SL constructed dataset with cross-sectional weight
stargazer(lifesatSL)


#Plots

#Boxplot
ggplot(mentalSL, aes(y=as.factor(gender), x= as.integer(mental_index)))+
geom_boxplot() +
facet_wrap(vars(country))+
ylab("Gender")+
xlab("Depression scale")
ggsave("mental.png")

#Smooth regression
ggplot(mental, aes(y = mental_index,x = age,color = gender))+ geom_point(alpha = 0.3) + geom_smooth(alpha =1.4)+facet_wrap(vars(country))
ggsave("mental_smooth.png")

#Histogram

#Plots of Uncertainty in cross-sectional SL
#1: Distribution by country of Refusal don't know and missing
lifesatSL %>%
 select(
	country,
	Na_n,
	Refus_n ,
	dont_n,
	perc_uncertainty
	)%>%
 group_by(country) %>%
 mutate(country_uncertainty  = mean(perc_uncertainty, na.rm = TRUE)) %>%
 ungroup()%>%
 select(-c(perc_uncertainty)) %>%
 gather(Variable, values,-c(country,country_uncertainty)) %>%
 ggplot(.,aes(values,Variable, color = Variable)) + 
 geom_boxplot() +
 facet_wrap(vars(country))+
 ggtitle("Number of missing item by country and category of missing")
 
 #Tables: Avarage life satisfaction by age group 
 