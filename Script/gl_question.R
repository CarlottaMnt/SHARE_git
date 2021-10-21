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
#General life question. 


gl_question <- sharelife_new %>%
 dplyr::select(
        mergeid,
		gender,
		country,
		age_int,
		int_year,
		yrbirth,
        sl_gl002_, #period of more happiness
			sl_gl003_, #when start
			sl_gl004_, #when stop
		sl_gl005_, #Period of more stress
			sl_gl006_, #when start
			sl_gl007_, #when stop
		sl_gl011_, #Period of financial hardship
			sl_gl012_, #when start
			sl_gl013_, #when stop
		sl_gl014_,     #Period of hunger
			sl_gl015_, #when start
			sl_gl016_, #when stop
#Ever discriminated/persecuted
		sl_gl022_, #Have you ever been the victim of such persecution or discrimination? 
		sl_gl023_, #Reason for persecution
		sl_gl024_  #Persecution force to stop working
# #Consequence of persecution on work
			# sl_gl026d1,#Denied promotions
			# sl_gl026d2,#Assignment to task
			# sl_gl026d3,#Working on task
			# sl_gl026d4,#Harassment by your boss
			# sl_gl026dno,#None of these
# #Difficulties finding adequate job because of persecution	
		# sl_gl028_,
		# sl_gl029_, #Year of first difficulties
# #Dispossessed as a consequence of persecution
		# sl_gl031_,    
		# sl_gl032d1_1, #Ddispossessed of business company 
		# sl_gl032d2_1, #Dispossessed of houses/building
		# sl_gl032d3_1, #Dispossesed of Farmland
		# sl_gl032d4_1, #Dispossesed of flat/apartment
		# sl_gl032d5_1, #Dispossesed money
		# sl_gl033_1, #When property was taken
		# sl_gl034_1, #Compensated for dispossession
		# sl_gl035_1:sl_gl035_3, #Other time of dispossessed
		) %>%
	mutate(
	sl_gl004_ = case_when(
			grepl(2995,sl_gl004_) ~ 1995,
			grepl(2078,sl_gl004_) ~ 2008,
			TRUE ~ as.numeric(sl_gl004_)),
	sl_gl007_ = case_when(
			grepl(2300,sl_gl007_) ~ as.numeric(2003),
			grepl(2204,sl_gl007_) ~ as.numeric(2004),
			grepl(2115,sl_gl007_) ~ as.numeric(2005),
			grepl(2100,sl_gl007_) ~ as.numeric(2001),
			grepl(2020,sl_gl007_) ~ as.numeric(2002),
			TRUE ~ as.numeric(sl_gl007_)),
	sl_gl015_ = case_when(
			grepl(2300,sl_gl015_) ~ as.numeric(2003),
			grepl(2204,sl_gl015_) ~ as.numeric(2004),
			grepl(2115,sl_gl015_) ~ as.numeric(2005),
			grepl(2100,sl_gl015_) ~ as.numeric(2001),
			grepl(2020,sl_gl015_) ~ as.numeric(2002),
			TRUE ~ as.numeric(sl_gl015_)),
	sl_gl016_ = case_when(
			grepl(2300,sl_gl016_) ~ as.numeric(2003),
			grepl(2204,sl_gl016_) ~ as.numeric(2004),
			grepl(2115,sl_gl016_) ~ as.numeric(2005),
			grepl(2100,sl_gl016_) ~ as.numeric(2001),
			grepl(2020,sl_gl016_) ~ as.numeric(2002),
			TRUE ~ as.numeric(sl_gl016_))
		) %>%
	mutate(
		ever_happier = case_when(
			grepl("No", sl_gl002_, fixed =TRUE) ~ 0,
			grepl("Yes", sl_gl002_, fixed =TRUE) ~ 1,
			grepl("Refusal", sl_gl002_, fixed =TRUE) ~ -2,
			grepl("Don't know", sl_gl002_,fixed =TRUE) ~ -1,
			TRUE ~ NA_real_),
		
		age_when_start_happyperiod = ifelse((sl_gl003_%in% c(-2,-1,-11)),sl_gl003_, sl_gl003_- yrbirth),
        age_when_stop_happyperiod = ifelse((sl_gl004_%in% c(-2,-1))|(sl_gl004_ > 9000), sl_gl004_, sl_gl004_- yrbirth), 
       
		ever_stress = case_when(
			grepl("No", sl_gl005_, fixed =TRUE) ~ 0,
			grepl("Yes", sl_gl005_,fixed =TRUE) ~ 1,
			grepl("Refusal", sl_gl005_, fixed =TRUE) ~ -2,
			grepl("Don't know",sl_gl005_,fixed =TRUE) ~ -1,
			TRUE ~ NA_real_),
		
		age_when_start_stressperiod = ifelse((sl_gl006_ %in% c(-2,-1,-11)),sl_gl006_, sl_gl006_- yrbirth),
        age_when_stop_stressperiod = ifelse((sl_gl007_%in% c(-2,-1,-11))|(sl_gl007_ > 9000), sl_gl007_, sl_gl007_- yrbirth),
		
		ever_financialstress = case_when(
			grepl("No", sl_gl011_, fixed =TRUE) ~ 0,
			grepl("Yes", sl_gl011_,fixed =TRUE) ~ 1,
			grepl("Refusal", sl_gl011_, fixed =TRUE) ~ -2,
			grepl("Don't know",sl_gl011_,fixed =TRUE) ~ -1,
			TRUE ~ NA_real_),
		
		age_when_start_fin_stressperiod = ifelse((sl_gl012_ %in% c(-2,-1,-11)),sl_gl012_, sl_gl012_- yrbirth),
		age_when_stop_fin_stressperiod = ifelse((sl_gl013_%in% c(-2,-1,-11))|(sl_gl013_ > 9000), sl_gl013_, sl_gl013_- yrbirth),
		
		ever_hunger = case_when(
			grepl("No", sl_gl014_, fixed =TRUE) ~ 0,
			grepl("Yes", sl_gl014_,fixed =TRUE) ~ 1,
			grepl("Refusal", sl_gl014_, fixed =TRUE) ~ -2,
			grepl("Don't know",sl_gl014_,fixed =TRUE) ~ -1,
			TRUE ~ NA_real_),
		
		age_when_start_hungerperiod = ifelse(( sl_gl015_ %in% c(-2,-1)), sl_gl015_,ifelse((sl_gl015_- yrbirth) < 0, NA_real_, (sl_gl015_- yrbirth))),
		age_when_stop_hungerperiod = ifelse(( sl_gl016_ %in% c(-2,-1,-11))|(sl_gl016_ > 9000), sl_gl016_,ifelse((sl_gl016_- yrbirth) < 0, NA_real_, (sl_gl016_- yrbirth))),

		ever_discriminated = case_when(
			grepl("No", sl_gl022_, fixed =TRUE) ~ 0,
			grepl("Yes", sl_gl022_,fixed =TRUE) ~ 1,
			grepl("Refusal", sl_gl022_, fixed =TRUE) ~ -2,
			grepl("Don't know",sl_gl022_,fixed =TRUE) ~ -1,
			TRUE ~ NA_real_),
		
		reason_discrimination = case_when(
			grepl(0, ever_discriminated, fixed =TRUE) ~ "no_discrimination",
			TRUE ~ as.character(sl_gl023_))
		) %>%		
		dummy_cols(., select_columns = c("reason_discrimination"),
           remove_selected_columns = FALSE) %>%
		dplyr::select(-c(sl_gl002_: sl_gl024_))
		
#plot discrimation
gl_question %>%
	dplyr::select(country, yrbirth, ever_hunger) %>%
	filter(country %in% c("Greece","Germany"), !is.na(ever_hunger), !(ever_hunger %in% c(-1,-2)), yrbirth <= 1942) %>%
	mutate(age_famine = case_when(
	grepl("Greece", country) ~ 1942- yrbirth,
	grepl("Germany",country)~ 1948 - yrbirth,
	TRUE ~ NA_real_))%>%
	filter(age_famine <= 20) %>%
	dplyr::group_by(age_famine)%>%
	dplyr::summarise(prop = mean(ever_hunger, na.rm = T))%>%
	ggplot(., aes(x = age_famine, y = prop)) + geom_line()
#plot hunger period 
#Check whther discrimnation happen mostly for migrant

discrimintion_migration <- merge(ac_fn , gl_question, by = c("mergeid", "country"))
					
mode1 <- lm(ac012_~  age_int.y + agesq ,  lifesat_migrantion)
lifesat_adj <- residuals(mode1)

childhood_migration <- merge(ac_fn ,childhood, by = c("mergeid", "country"))
			
df <- discrimintion_migration%>%
	select(country, change_region, sl_gl022_, gender) %>%
    filter(!is.na(sl_gl022_), !is.na(change_region),!(sl_gl022_ %in% c("Refusal","Don't know" ))) %>%
	mutate(sl_gl022_ = as.factor(sl_gl022_))%>%
	droplevels()

mod <- glm(sl_gl022_ ~ gender + change_region + country , data = df, family = binomial)

stargazer(gl_question)

save(gl_question,file="Dataset_constructed/gl.RData")