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
install.packages("psych")
install.packages("surveysummary")
install.packages("stargazer")
install.packages("car")
install.packages("tables")
install.packages("Blackmore")
install.packages("fastDummies")
install.packages("tmaptools")
install.packages("tmap") 
install.packages("sf")
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
############################ mental_health #####################################
################################################################################
################################################################################
rm(list=ls())
#Import the required dataset from the folder "~/Dataset_constructed/." and "~/panel/."
#Dataset with sharelife respondents (N = 57714) , excluding Ireland, people < 50, proxy respondents.

#This is created in the script "sharelife_merge.R".

load("Dataset_constructed/sharelife_new.RData") 

#This is created in the script "~/Old_script/mentaldf".
load("panel/mentaldf.RData")

#This is created in the script "~/Old_script/cvdf". We will use use to gather info on age and gender of the respondent
load("panel/CVdf.Rdata")

#Create the dataframe with only the variable required for constructing the mental health.
#The selected items are taken from the SHARE_Scales_and_Multi-Item_Indicators pdf in the folder ~/Questionnaire

mh <- mentaldf %>%
	dplyr::select(
        mergeid,
		country,
		wave,
        mh002_, #DEPRESSION
		mh003_, #HOPE FOR THE FUTURE
		mh004_, #FELT WOULD RATHER BE DEAD
		mh005_, #FEELS GUILTY
		mh006_, #BLAME FOR WHAT
		mh007_, #TROUBLE SLEEPING
		mh008_, #LESS OR SAME INTEREST IN THING
		mh009_, #KEEPS UP INTEREST
		mh010_, #IRRITABILITY
		mh011_, #APPETITE
		mh012_, #EATING MORE OR LESS
		mh013_, #FATIGUE
		mh014_, #CONCENTRATION ON ENTERTAINMENT
		mh015_, #CONCENTRATION ON READING
		mh016_, #ENJOYMENT
		mh017_  #TEARFULNESS
		) %>% 
	mutate(
	sad_depressed = case_when(
		grepl("Refusal",mh002_)~ as.numeric(-2),
		grepl("Don't know",mh002_)~ as.numeric(-1),
		grepl("Yes",mh002_)~ as.numeric(1),
		grepl("No",mh002_)~ as.numeric(0),
		TRUE ~ NA_real_),
	hope_for_future = case_when(
		grepl("Refusal",mh003_)~ as.numeric(-2),
		grepl("Don't know",mh003_)~ as.numeric(-1),
		grepl("Any hopes mentioned",mh003_)~ as.numeric(0),
		grepl("No hopes mentioned",mh003_)~ as.numeric(1),
		TRUE ~ NA_real_),
	felt_would_die = case_when(
		grepl("Refusal",mh004_)~ as.numeric(-2),
		grepl("Don't know",mh004_)~ as.numeric(-1),
		grepl("Any mention of suicidal feelings or wish to be dead",mh004_)~ as.numeric(1),
		grepl("No such feelings",mh004_)~ as.numeric(0),
		TRUE ~ NA_real_),
	felt_guilty = case_when(
		grepl("Refusal",mh005_)~ as.numeric(-2),
		grepl("Don't know",mh005_)~ as.numeric(-1),
		grepl("Refusal",mh006_)~ as.numeric(-2),
		grepl("Don't know",mh006_)~ as.numeric(-1),
		grepl("Obvious excessive guilt or self-blame",mh005_)~ as.numeric(1),
		grepl("No such feelings",mh005_)~ as.numeric(0),
		grepl("Example(s) of obvious guilt or self-blame",mh006_)~ as.numeric(1),
		grepl("No example(s) of obvious guilt or self-blame or unclear",mh006_)~as.numeric(0),
		TRUE ~ NA_real_),
	trouble_sleep = case_when(
		grepl("Refusal",mh007_)~ as.numeric(-2),
		grepl("Don't know",mh007_)~ as.numeric(-1),
		grepl("Trouble with sleep or recent change in pattern",mh007_)~ as.numeric(1),
		grepl("No trouble sleeping",mh007_)~ as.numeric(0),
		TRUE ~ NA_real_),
	less_interest = case_when(
		grepl("Refusal",mh008_)~ as.numeric(-2),
		grepl("Don't know",mh008_)~ as.numeric(-1),
		grepl("Refusal",mh009_)~ as.numeric(-2),
		grepl("Don't know",mh009_)~ as.numeric(-1),
		grepl("Less interest than usual mentioned",mh008_)~ as.numeric(1),
		grepl("No mention of loss of interest",mh008_)~ as.numeric(0),
		grepl("Yes",mh009_)~ as.numeric(0),
		grepl("No",mh009_)~ as.numeric(1),
		TRUE ~ NA_real_),
	irritability =  case_when(
		grepl("Refusal",mh010_)~ as.numeric(-2),
		grepl("Don't know",mh010_)~ as.numeric(-1),
		grepl("Yes",mh010_)~ as.numeric(1),
		grepl("No",mh010_)~ as.numeric(0),
		TRUE ~ NA_real_),
	appetite =  case_when(
		grepl("Refusal",mh011_)~ as.numeric(-2),
		grepl("Don't know",mh011_)~ as.numeric(-1),
		grepl("Refusal",mh012_)~ as.numeric(-2),
		grepl("Don't know",mh012_)~ as.numeric(-1),
		grepl("Diminution in desire for food", mh011_)~ as.numeric(1),
		grepl("No diminution in desire for food",mh011_)~ as.numeric(0),
		grepl("Less",mh012_)~ as.numeric(1),
		grepl("More",mh012_)~ as.numeric(0),
		grepl("Neither more nor less",mh012_)~ as.numeric(0),
		TRUE ~ NA_real_),
	fatigue = case_when(
		grepl("Refusal",mh013_)~ as.numeric(-2),
		grepl("Don't know",mh013_)~ as.numeric(-1),
		grepl("Yes",mh013_)~ as.numeric(1),
		grepl("No",mh013_)~ as.numeric(0),
		TRUE ~ NA_real_),
	difficulty_concentrating =case_when(
		grepl("Refusal",mh014_)~ as.numeric(-2),
		grepl("Don't know",mh014_)~ as.numeric(-1),
		grepl("Refusal",mh015_)~ as.numeric(-2),
		grepl("Don't know",mh015_)~ as.numeric(-1),
		grepl("Difficulty in concentrating",mh014_)~ as.numeric(1),
		grepl("No such difficulty mentioned",mh014_)~ as.numeric(0),
		grepl("Difficulty in concentrating",mh015_)~ as.numeric(1),
		grepl("No such difficulty mentioned",mh015_)~ as.numeric(0),
		TRUE ~ NA_real_),
	enjoyment  = case_when(
		grepl("Refusal",mh016_)~ as.numeric(-2),
		grepl("Don't know",mh016_)~ as.numeric(-1),
		grepl("Fails to mention any enjoyable activity", mh016_)~ as.numeric(1),
		grepl("Mentions any enjoyment from activity", mh016_)~ as.numeric(0),
		TRUE ~ NA_real_),
	tearfulness = case_when(
		grepl("Refusal",mh017_)~ as.numeric(-2),
		grepl("Don't know",mh017_)~ as.numeric(-1),
		grepl("Yes",mh017_)~ as.numeric(1),
		grepl("No",mh017_)~ as.numeric(0),
		TRUE ~ NA_real_)
	) %>%
	dplyr::select(
		-c(
        mh002_, #DEPRESSION
		mh003_, #HOPE FOR THE FUTURE
		mh004_, #FELT WOULD RATHER BE DEAD
		mh005_, #FEELS GUILTY
		mh006_, #BLAME FOR WHAT
		mh007_, #TROUBLE SLEEPING
		mh008_, #LESS OR SAME INTEREST IN THING
		mh009_, #KEEPS UP INTEREST
		mh010_, #IRRITABILITY
		mh011_, #APPETITE
		mh012_, #EATING MORE OR LESS
		mh013_, #FATIGUE
		mh014_, #CONCENTRATION ON ENTERTAINMENT
		mh015_, #CONCENTRATION ON READING
		mh016_, #ENJOYMENT
		mh017_  #TEARFULNESS
		)  
	)


#Now that we have the mental item df we are ready to create the mental health index (depression index).

#Moreover, we compute for each respondent the perc_uncertainty which relates to the (un)informativeness of each respondent. 
#Average over the overall number of items of missing value, refusal and don't know responses for each respondent. 

#We merge the items df with some socio-demographic characteristic of our respondents: country, age and gender.
cv_panel <- CVdf %>%
	filter(mergeid %in% mh$mergeid) %>%
	dplyr::select(
	 mergeid,
	 age_int,
	 wave,
	 int_year,
	 int_month,
	 gender
	) %>%
	dplyr::mutate(
	season = ifelse(
	int_month %in% c("June","July","August"), "Summer",
	ifelse(int_month %in% c("September","October", "November"), "Autumn",
	ifelse(int_month %in% c("December","January","February"),"Winter",
	ifelse(int_month %in% c("March","April","May"),"Spring",NA_character_)))))%>%
	dplyr::select(-c(int_month))

 
#Which respondents have not mental health items?

#Are these the same that have not frailty items?

head(setdiff(unique(CVdf$mergeid),unique(mentaldf$mergeid)))

#Which are the less certain items? 
#Which is the certainty thorugh which we measure the mental health?


mental_panel <- merge(mh, cv_panel, by = c("mergeid","wave"), all = FALSE) %>%
	select(
	  mergeid,
	  wave,
	  int_year,
	  country,
	  gender,
	  season,
	  age_int,
	  everything()
	) %>%
	mutate(
		N_Na_mental = apply(.[,-c(1:6)], 1, function(x) sum(is.na(x))),
		N_Refus_mental = apply(.[,-c(1:6)], 1, function(x) sum(x == -2, na.rm = TRUE)),
		N_dont_mental = apply(.[,-c(1:6)], 1, function(x) sum(x == -1, na.rm = TRUE)),
		perc_uncertainty_mental = round((N_Na_mental + N_Refus_mental + N_dont_mental)/12,2)
	) %>%
	select(
		mergeid,
		wave,
		country,
		int_year,
		age_int,
		gender,
		season,
		N_Na_mental,
		N_Refus_mental, 
		N_dont_mental,
		perc_uncertainty_mental,
		everything()
	) %>%
	mutate(
		mental_index = apply(.[,-c(1:11)],1, function(x) sum(x[x >= 0], na.rm = TRUE))
	)

#Panel SL dataset
#Here we filter out from the overall panel only SHARELIFE respondents from sharelife_new 
mentalSL_panel <- mental_panel %>%
	filter(
		mergeid %in% sharelife_new$mergeid,
	) %>%
	dplyr::group_by(mergeid) %>%
	dplyr::mutate(
		duration_mental = int_year - min(int_year), #duration in the panel 
		count_mental = n() #number of observation in the panel 
	) %>%
	ungroup()

#Which Sharelife respondents have not imputed values in mental health panel?
summary(sharelife_new[which(!(sharelife_new$mergeid %in% mental_panel$mergeid)),]$firstwave)

#Cross-sectional SL dataset
#Here we select the respondent responses closer to the sharelife questionnaire and we merge the data set with the SL cross sectional weights

mentalSL <- mentalSL_panel %>%
 group_by(mergeid) %>%
 dplyr::slice(ifelse((last(perc_uncertainty_mental)> 0.5 & last(count_mental)!= 1), n()-1, n())) %>% #select the last observation available and wih less uncertainy
 ungroup() %>%
 merge(., sharelife_new[,c("mergeid","cciw")],by = "mergeid", all = FALSE) %>%
 dplyr::select(mergeid, cciw, everything())

#Save data set
save(mental_panel,file = "panel/mental_panel.RData")
save(mentalSL_panel,file="panel/mentalSL_panel.RData")
save(mentalSL,file="Dataset_constructed/mentalSL.RData")

#Here we explore the cross-sectional SL constructed dataset with cross-sectional weight
stargazer(mentalSL)

attach(mentalSL)
 
 y <- mental_index[mental_index >= 0]
 x <- season[mental_index >=0]
 
 summary(lm(y ~ x -1))

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
mentalSL %>%
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
