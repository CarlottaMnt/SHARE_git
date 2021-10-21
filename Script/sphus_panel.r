setwd("C:/Users/carlotta/Desktop/Dropbox/SHARE_new/R")
#setwd("//crc/Team_work/Caralpvk/SHARE_new/R")
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
############################ Perceived health  (SPHUS) #########################
################################################################################
################################################################################
rm(list=ls())
#Import the required dataset from the folder "~/Dataset_constructed/." and "~/panel/."
#Dataset with sharelife respondents (N = 57714) , excluding Ireland and proxy respondents. Be aware that this includes still respondent < 50.
#This is created in the script "sharelife_merge".

load("Dataset_constructed/sharelife_new.RData") 

#This is created in the script "~/Old_script/activitdf".

load("panel/physicaldf.RData")

#This is created in the script "~/Old_script/cvdf". We will use use to gather info on age and gender of the respondent

load("panel/CVdf.Rdata")

sphus <- physicaldf %>%
    dplyr::select(
		 mergeid,
		 wave,
		 country,
#Would you say your health is... 
		 ph003_
	)%>%  
	filter(wave != 7) %>%
	mutate(
	    sphus_cat = ph003_,
		sphus = case_when(
		grepl("Poor",ph003_) ~ as.numeric(1),
		grepl("Fair",ph003_) ~ as.numeric(0.75),
		grepl("Good",ph003_) ~ as.numeric(0.5),
		grepl("Very good",ph003_) ~ as.numeric(0.25),
		grepl("Excellent",ph003_) ~ as.numeric(0),
		grepl("Refusal",ph003_) ~ as.numeric(-2),
		grepl("Don't know",ph003_) ~ as.numeric(-1),
		TRUE ~ NA_real_)) %>%
		dplyr::select(-c(ph003_))%>%
		mutate(season = NA)
		

sphusSL <- sharelife_new %>%
	dplyr::select(
		 mergeid,
		 wave,
		 country,
		 int_month,		 
#Would you say your health is... 
		 sl_ph003_
	)%>%  
	mutate(
	    sphus_cat = sl_ph003_,
		sphus = case_when(
			grepl("Poor",sl_ph003_) ~ as.numeric(1),
			grepl("Fair",sl_ph003_) ~ as.numeric(0.75),
			grepl("Good",sl_ph003_) ~ as.numeric(0.5),
			grepl("Very good",sl_ph003_) ~ as.numeric(0.25),
			grepl("Excellent",sl_ph003_) ~ as.numeric(0),
			grepl("Refusal",sl_ph003_) ~ as.numeric(-2),
			grepl("Don't know",sl_ph003_) ~ as.numeric(-1),
			TRUE ~ NA_real_),
		wave = case_when(
			grepl("SL_7",wave)~ 7,
			grepl("SL_3",wave)~3,
			TRUE ~ NA_real_)
		) %>%
		dplyr::select(-c(sl_ph003_))%>%
	mutate(
	season = ifelse(
	int_month %in% c("June","July","August"), "Summer",
	ifelse(int_month %in% c("September","October", "November"), "Autumn",
	ifelse(int_month %in% c("December","January","February"),"Winter",
	ifelse(int_month %in% c("March","April","May"),"Spring",NA_character_)))))%>%
	dplyr::select(-c(int_month))

sphus <- rbind(sphus,sphusSL) %>% arrange(wave) 
#Now that we have the life satisfaction df we also have for each respondent the perc_uncertainty which relates to the (un)informativeness of each respondent. 
#Average over the overall number of items of missing value, refusal and don't know responses for each respondent in the activit module

#We merge the items df with some socio-demographic characteristic of our respondents: country, age and gender.
cv_panel <- CVdf %>%
	filter(mergeid %in% sphus$mergeid) %>%
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

head(setdiff(unique(CVdf$mergeid),unique(sphus$mergeid)))

#Which are the less certain items? 
#Which is the certainty thorugh which we measure the mental health?


sphus_panel <- merge(sphus, cv_panel, by = c("mergeid","wave"), all = FALSE) %>%
	select(
		mergeid,
		wave,
		country,
		int_year,
		age_int,
		gender,
		sphus,
		sphus_cat,
		season.x,
		season.y,
		everything()
	)
sphus_panel$season <- coalesce(sphus_panel$season.y,sphus_panel$season.x)
sphus_panel <- sphus_panel[,-which(names(sphus_panel) %in% c("season.x","season.y"))]
#Panel SL dataset

#Here we filter out from the overall panel only SHARELIFE respondents from sharelife_new 
sphusSL_panel <- sphus_panel %>%
	filter(
		mergeid %in% sharelife_new$mergeid,
	) %>%
	group_by(mergeid) %>%
	dplyr::mutate(
		duration_sphus = int_year - min(int_year), #duration in the panel 
		count_sphus = n() #number of observation in the panel 
	) %>%
	ungroup()

#Which Sharelife respondents have not imputed values in lifesat_panel?
summary(sharelife_new[which(!(sharelife_new$mergeid %in% sphus_panel$mergeid)),]$firstwave)

#Cross-sectional SL dataset
#Here we select the respondent responses closer to the sharelife questionnaire and we merge the data set with the SL cross sectional weights

sphusSL <- sphusSL_panel %>%
 filter(wave %in% c(3,7)) %>% #select observation in the year of interest
 ungroup() %>%
 merge(., sharelife_new[,c("mergeid","cciw")],by = "mergeid", all = FALSE) %>%
 dplyr::select(mergeid, cciw, everything())
 
 
#save df---------------------------------------------------------
save(sphus_panel,file = "panel/sphus_panel.RData")
save(sphusSL_panel,file="panel/sphusSL_panel.RData")
save(sphusSL,file="Dataset_constructed/sphusSL.RData")
#Here we explore the cross-sectional SL constructed dataset with cross-sectional weight
stargazer(happySL)

attach(sphusSL)
 
 y <- sphus[sphus >= 0]
 x <- season[sphus >=0]
 
 summary(lm(y ~ x -1))
#Plots
panel <- lifesatSL_panel %>%
 filter(count_lsat> 1, !(lifesat %in% c(-11,-1,-2)), !is.na(lifesat)) %>%
 select(mergeid, ac012_) %>%
 group_by(mergeid) %>% 
 mutate(P1d = ac012_ - lag(ac012_)) %>% 
 ungroup

 load( "panel/lifesatSL_panel.RData")
panel <- lifesatSL_panel %>% 
 filter(mergeid %in% sumTable$mergeid,count_lsat> 1, !(lifesat %in% c(-11,-1,-2)), !is.na(lifesat))%>%
 select(mergeid,ac012_, lifesat,wave)
panel[which(panel$mergeid == "AT-664523-02"),]

panel<-  transform(panel, P1d = ave(ac012_, mergeid, FUN = function(x) c(NA, diff(x))))%>%
		filter(P1d != -10)
AT-664523-02
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
 