#Import the package

library(fastDummies)
library(psych)
library(car)
library(janitor)
library(tables)
library(stargazer)
library(Hmisc)
library(reshape2)
library(caret)
library(qwraps2)
library(tidyverse)# This package is very useful for data analysis
library(knitr)
library(bestNormalize)
library(xtable)
library(plyr)     #Useful for gathering
library(gtable)
library(grid)
library(corrplot)
library(here)
library(naniar)
library(gridExtra)
library(foreign)
library(data.table)
library(foreign)
#import the data
rm(list =ls())
#import the functions

source("Functions.R")
#####################################################################################################
#####################   DATA SET CONSTRUCTION   #####################################################
#####################################################################################################
##Here we import the module where we know how reliable the respondent is

goodW3 <- read.dta("Data/sharew3_rel7-1-0_iv.dta") %>% 
       filter(sl_iv004_ %in% c("Good","Very good","Fair")) %>% 
  select(mergeid)
goodW7 <- read.dta("Data/sharew7_rel7-1-0_iv.dta") %>%  #Interview modules
  filter(iv004_ %in% c("Good","Very good","Fair"))%>% 
  select(mergeid)

goodmergeid <- rbind(goodW3, goodW7)

#Load the rawdata

#this comes from the ~Old_script/sharelife_merge script. 57740 respondent, No Ireland but yes people aged less 50.
load("Dataset_constructed/sharelife_new.RData")

#Which are the items with less than 25% missing data?
x <- sharelife_new[lapply(sharelife_new, function(x) sum(is.na(x)) / length(x) ) < 0.25 ] #select columns for which the numebr of missing is less then 10% of the overall

#Start creating new "base" variables
base <-  sharelife_new %>%
  filter(mergeid %in% goodmergeid$mergeid) %>% 
  mutate(
	age_int = case_when(
		grepl(-9,age_int)~ NA_real_,
		TRUE ~ as.numeric(age_int)),
	int_year = case_when(
		grepl(-9,int_year)~ 2009,
		TRUE ~ as.numeric(int_year)),
	yrbirthp = case_when(
		grepl(-9,yrbirthp) ~ NA_real_,
		TRUE ~ as.numeric(yrbirthp)),
	sl_re002_= case_when(
 		grepl(-3, sl_re002_)~ NA_real_,
		TRUE ~ as.numeric(sl_re002_)),
	CNTR_CODE = substr(toupper(country),1, 3)
	)%>%
	mutate(
	CNTR_CODE = case_when(
		grepl("AUS",CNTR_CODE) ~ "AT",
		grepl("SPA",CNTR_CODE) ~ "ES",
		grepl("DEN",CNTR_CODE) ~ "DK",
		grepl("GER",CNTR_CODE) ~ "DE",
		grepl("IRE",CNTR_CODE) ~ "IE",
		grepl("NET",CNTR_CODE) ~ "NL",
		grepl("POL",CNTR_CODE) ~ "PL",
		grepl("SWI",CNTR_CODE) ~ "CH",
		grepl("GRE",CNTR_CODE) ~ "EL",
		grepl("SWE",CNTR_CODE) ~ "SE",
		TRUE ~ substr(CNTR_CODE,1, 2)),

#Person and Room childhood adjustment
	sl_cs003_= case_when(
 			grepl(-3,sl_cs003_) ~ NA_real_,
			TRUE ~ as.numeric(sl_cs003_)),
	sl_cs002_= as.numeric(sl_cs002_),			
	age = int_year - yrbirth
	) %>%
	mutate(	
	age_group = 
	  ifelse((age >= 49 & age <=54),1,
	  ifelse((age >= 55 & age <=59),2,
	  ifelse((age >= 60 & age <=64),3,
	  ifelse((age >= 65 & age <=69),4,
	  ifelse((age >= 70 & age <=74),5,
	  ifelse((age >= 75 & age <=79),6,
	  ifelse((age >= 80 & age <=84),7,
	  ifelse(age >= 85,8, NA)))))))),
	cohort = 
	  ifelse((yrbirth < 1930),5,
	  ifelse((yrbirth >= 1930 & yrbirth <= 1939),4,
	  ifelse((yrbirth >= 1940 & yrbirth <= 1949),3,
	  ifelse((yrbirth >= 1950 & yrbirth <= 1959),2,
	  ifelse((yrbirth >= 1960 & yrbirth <= 1969),1, NA)))))
	  )%>%
	arrange(mergeid)

#Descriptive statistic statistics.---------------------

base %>%
 group_by(country,gender) %>%
 dplyr::summarise(n = n(),age= mean(age, na.rm = TRUE)) %>%
 kable(digit = 1, format = "latex")
summary(base)
rg <- sf::st_read("Data/NUTS_RG_01M_2016_4326_LEVL_1.shp") %>% 
dplyr::select(-c(NAME_LATN,MOUNT_TYPE,URBN_TYPE,COAST_TYPE,FID,LEVL_CODE))
plot(rg)

#Country, age-group

sumTable <- base %>%  
    dplyr::select(age,cciw,country,hhid,gender,wave, mergeid)%>%
	droplevels() %>%
	filter(age >= 49, age <= 89, !(country %in% c("Netherland","Ireland")))%>%
	mutate(age_group = 
	  ifelse((age >= 50 & age <=59),"49-59",
	  ifelse((age >= 60 & age <=69),"60-69",
	  ifelse((age >= 70 & age <=79),"70-79",
	  ifelse((age >= 80 & age <=89),"80-89", NA)))))%>%
	 mutate(age_group = as.factor(age_group))%>%
	dplyr::group_by(country, age_group) %>%
	dplyr::summarise(n_weight = sum(cciw,na.rm= TRUE),n_unweight = n()) 

DF.wide <- dcast(sumTable, country ~ age_group, value.var="n_unweight")

DF.wide$ratio_1 <- DF.wide[["49-59"]]/(DF.wide[["60-69"]] + DF.wide[["50-59"]] + DF.wide[["70-79"]] + DF.wide[["80-89"]])
DF.wide$ratio_2 <- DF.wide[["60-69"]]/(DF.wide[["60-69"]] + DF.wide[["49-59"]] + DF.wide[["70-79"]] + DF.wide[["80-89"]])
DF.wide$ratio_3 <- DF.wide[["70-79"]]/(DF.wide[["60-69"]] + DF.wide[["49-59"]] + DF.wide[["70-79"]] + DF.wide[["80-89"]])
DF.wide$ratio_4 <- DF.wide[["80-89"]]/(DF.wide[["60-69"]] + DF.wide[["49-59"]] + DF.wide[["70-79"]] + DF.wide[["80-89"]])
DF.wide$tot <- (DF.wide[["60-69"]] + DF.wide[["49-59"]] + DF.wide[["70-79"]] + DF.wide[["80-89"]])
kable(DF.wide,format="latex", digits=2)

#Country, gender

sumTable <- base %>%  
    dplyr::select(age,cciw,country,hhid,gender)%>%
	droplevels() %>%
	filter(age >= 49, age <= 89)%>%
	mutate(age_group = 
	  ifelse((age >= 49 & age <=59),"49-59",
	  ifelse((age >= 60 & age <=69),"60-69",
	  ifelse((age >= 70 & age <=79),"70-79",
	  ifelse((age >= 80 & age <=89),"80-89", NA)))))%>%
	 mutate(age_group = as.factor(age_group))%>%
	dplyr::group_by(gender,country)%>%
	dplyr::summarise(n_weight = sum(cciw,na.rm= TRUE),n_unweight = n()) 

DF.wide <- dcast(sumTable, country ~ gender, value.var="n_unweight")

DF.wide$ratio_fm <- DF.wide[["Female"]]/(DF.wide[["Male"]])
DF.wide$ratio_ftot <- DF.wide[["Female"]]/(DF.wide[["Female"]] + DF.wide[["Male"]])
DF.wide$tot <- DF.wide[["Female"]] + DF.wide[["Male"]]
kable(DF.wide,format="latex", digits=2)

#Country, gender, age-group

sumTable <- base %>%  
    dplyr::select(age,cciw,country,hhid,gender)%>%
	droplevels() %>%
	filter(age >= 49, age <= 89)%>%
	mutate(age_group = 
	  ifelse((age >= 49 & age <=59),"49-59",
	  ifelse((age >= 60 & age <=69),"60-69",
	  ifelse((age >= 70 & age <=79),"70-79",
	  ifelse((age >= 80 & age <=89),"80-89", NA)))))%>%
	 mutate(age_group = as.factor(age_group))%>%
	dplyr::group_by(age_group,gender)%>%
	dplyr::summarise(n_weight = sum(cciw,na.rm= TRUE),n_unweight = n()) 



DF.wide <- dcast(sumTable, gender ~ age_group, value.var="n_unweight")

kable(DF.wide,format="latex", digits=2)

#Country, gender, cohort

sumTable <- base %>%  
    dplyr::select(mergeid,cohort,age,yrbirth,cciw,country,hhid,gender)%>%
	droplevels() %>%
	filter(age >= 49, age <= 89)%>%
	mutate(
	cohort = 
	  ifelse((yrbirth < 1930),"< 1930",
	  ifelse((yrbirth >= 1930 & yrbirth <= 1939),"1930-1939",
	  ifelse((yrbirth >= 1940 & yrbirth <= 1949),"1940-1949",
	  ifelse((yrbirth >= 1950 & yrbirth <= 1959),"1950-1959",
	  ifelse((yrbirth >= 1960 & yrbirth <= 1969),"1960-1969",
	   ifelse((yrbirth >= 1970),">= 1970", NA))))))
	  )%>%
	mutate(cohort = as.factor(cohort))%>%
	dplyr::group_by(country,cohort, gender)%>%
	dplyr::summarise(n_weight = sum(cciw,na.rm= TRUE),n_unweight = n()) 



DF.wide <- dcast(sumTable, country ~  gender + cohort, value.var="n_unweight")

kable(DF.wide,format="latex", digits=2)

#CREATION ON THE DEMOGRAPHIC DATA FRAME
load("Dataset_constructed/current_adultJEP_new")
load("Dataset_constructed/imputation_SL")


df1 <- base[,c("mergeid","cciw","age","hhid","yrbirth","gender","wave","hhsize","country")]%>% 
  left_join(current_adultJEP) %>%      
	dplyr::select(mergeid,age,yrbirth,wave,cciw,hhid,country,country_birth_2,gender, country_birth_)%>%
	filter(age >= 49, age <= 89)%>%
	mutate(cohort = 
			ifelse((yrbirth < 1930),"< 1930",
			ifelse((yrbirth >= 1930 & yrbirth <= 1939),"1930-1939",
			ifelse((yrbirth >= 1940 & yrbirth <= 1949),"1940-1949",
			ifelse((yrbirth >= 1950 & yrbirth <= 1959),"1950-1959",
			ifelse((yrbirth >= 1960 & yrbirth <= 1969),"1960-1969",
			ifelse((yrbirth >= 1970),">= 1970", NA)))))),
			age_group = 
			ifelse((age >= 49 & age <=59),"49-59",
			ifelse((age >= 60 & age <=69),"60-69",
			ifelse((age >= 70 & age <=79),"70-79",
			ifelse((age >= 80 & age <=89),"80-89", NA)))))%>%
			mutate(
			age_group = as.factor(age_group),
			country_birth_3 = ifelse(country_birth_2 == "EU-Citizenship" ,country_birth_2,ifelse(is.na(country_birth_2), country_birth_2, "NO-EU-Citizenship")),
			country_birth_ = case_when(
			  grepl("Germany", country_birth_) ~ "Germany",
			  TRUE~ country_birth_)) %>%
      mutate(migrant = ifelse(country_birth_ != country, 1,0)) %>% 
      mutate(migrant = ifelse((country_birth_ %in% c("Slovakia","Czechoslovakia") & country == "Czech Republic"), 0,migrant)) %>% 
			dplyr::select(-c(age,yrbirth))%>%
			droplevels()%>%
            remove_constant()

#Create educational level 

df2 <- base[,c("mergeid","cciw","age","yrbirth","gender","wave","hhsize","country")] %>% 
  left_join(imputation_SL[,c("mergeid","yedu","isced")]) %>%  
    dplyr::select(mergeid,age,cciw,gender,yedu, isced, country)%>%
	filter(age >= 49, age <= 89)%>%
	mutate(
	isced_2 = 
		ifelse((isced == "Isced-97 code 5" | isced == "Isced-97 code 6"),"High",
		ifelse((isced == "Isced-97 code 4" | isced =="Isced-97 code 3"),"Medium",
		ifelse((isced == "Isced-97 code 2"| isced == "Isced-97 code 1"),"Low",
	    ifelse((isced == "None"),"No education",NA))))
	  )%>%
	mutate(isced_2 = as.factor(isced_2))%>%
	dplyr::select(-c(isced))

demographic <- df1 %>% left_join(df2)

#Create race

demographic <- demographic %>%
	mutate(
	race = case_when(
	 grepl("North Africa", country_birth_2, ignore.case = FALSE) ~ "White",
	 grepl("Africa", country_birth_2, ignore.case=FALSE) ~ "Black",
	 grepl("Europe", country_birth_2, ignore.case=FALSE) ~ "White",
	 grepl("EU", country_birth_2, ignore.case=FALSE) ~ "White",
	 grepl("Caribbean", country_birth_2, ignore.case=FALSE) ~ "Hawaiian",
	 grepl("Oceanian", country_birth_2, ignore.case=FALSE) ~ "White",
	 grepl("Antarctica", country_birth_2, ignore.case=FALSE) ~ "White",
	 grepl("Center_America", country_birth_2, ignore.case=FALSE) ~ "White",
	 grepl("Asia", country_birth_2, ignore.case=FALSE) ~ "Asian",
	 grepl("South_America", country_birth_2, ignore.case=FALSE) ~ "Hispanic",
	 grepl("Asia", country_birth_2, ignore.case=FALSE) ~ "Asian",
	 grepl("Center_America", country_birth_2, ignore.case=FALSE) ~ "Hispanic",
	 grepl("United Kingdom", country_birth_2, ignore.case=FALSE) ~ "White",
	 grepl("Switzerland", country_birth_2, ignore.case=FALSE) ~ "White",
	 grepl("United States of America", country_birth_2, ignore.case=FALSE) ~ "White",
	 grepl("Canada", country_birth_2, ignore.case=FALSE) ~ "White",
	 TRUE ~ country_birth_2))
	  

x = nearZeroVar(demographic , saveMetrics = TRUE)
 

##################################### CURRENT ADULT DATASET #######################################

######################################################################################################
##################################### imputation_SL.RData ############################################
######################################################################################################

#Education ,current marital status, current income, current wealth: from imputation modules. 

#This dataset has been created in the script ~Old_Script/gv_imputationSL.R (be aware there is another script similar!!)
#Dataset stored in the folder Dataset_constructed.

#Current marital status and current job status
sumTable1 <- base[,c("mergeid","cciw","age","yrbirth")] %>% 
   left_join(current_adultJEP)%>%  
    dplyr::select(
		mergeid,
		age,
		yrbirth,
		cur_retired,
		cur_employed,
		never_work,
		#situation,
		year_when_retired,
		cur_married,
		East_Germany,
		West_Germany,
		cur_age_youngest_child,
		)%>%
	filter(age >= 49, age <= 89) %>%
	mutate(age_when_retired = ifelse(is.na(year_when_retired), year_when_retired, ifelse((year_when_retired - yrbirth) < 15, NA,year_when_retired - yrbirth)))%>%
	dplyr::select(-c(year_when_retired,age,yrbirth)) %>%
	#dummy_cols(.,"situation", remove_selected_columns = TRUE)%>%
	droplevels()%>%
    remove_constant()

x = nearZeroVar(sumTable1, saveMetrics = TRUE)

#Remvoe near zero variance 

sumTable1 <- sumTable1[,-which(names(sumTable1) %in% rownames(x[x[,"nzv"] > 0, ]))]
sumTable1 <- sumTable1 %>%
             droplevels()%>%
			 remove_constant()

sumTable2 <- base[,c("mergeid","cciw","age","country","gender")] %>% 
  left_join(imputation_SL[,c("mergeid","yedu","income","con_in","con_out","ngrchild")]) %>% 
	filter(age >= 49, age <= 89) %>%
	mutate(yedu = ifelse(yedu > 40, NA_real_, yedu))%>%
	dplyr::select(-c(age))

#CREATION OF THE CURRENT DF
current_df <- merge(sumTable1,sumTable2, by = "mergeid", all = TRUE)


stargazer(current_df, omit.summary.stat = c("p25","p75"))


############################# CHILDHOOD DATASET ########################################

#############################################################################################
############################### df_child_new.R  #################################################
#############################################################################################
load("Dataset_constructed/df_child_new.RData")

sumTable <- base[,c("mergeid","cciw","age","country","gender")] %>% 
  left_join(df_child) %>%  
	filter(age >= 49, age <= 89) %>%
	dplyr::select(-c(age))%>%
	droplevels()%>%
	remove_constant()   %>%
	dummy_cols(.,
				c(	
				"math_level",
				"language_level",
				"ISCO_par",
				"shr",
				"nr_book"
				)
				) %>%
	mutate (
	ISCO_par = as.numeric(as.character(ISCO_par)),
	shr =  as.numeric(as.character(shr)),
	nr_book = as.numeric(as.character(nr_book))
	)%>%
	select(-c(math_level,language_level,math_level_num,language_level_num))

x = nearZeroVar(sumTable, saveMetrics = TRUE)


sumTable <- sumTable[,-which(names(sumTable) %in% rownames(x[x[,"nzv"] > 0, ]))]

sumTable1 <- sumTable %>%
				droplevels()%>%
				remove_constant()   %>%
				tidyr::gather(Variable, Value,
				-c(
				mergeid,
				gender,
				cciw,
				country)
				)%>%
				filter(!(Value %in% c(9997,9996,9777)))%>%
				tidyr::spread(Variable,Value)

stargazer(sumTable1, omit.summary.stat = c("p25","p75"))

childhood <- sumTable1

stargazer(childhood)

#####################################partner.RData##################################################
#Respondent family situation
load("Dataset_constructed/partner.RData")

sumTable <- base[,c("mergeid","cciw","age")] %>% 
  left_join(partner) %>%  
	filter(age >= 49, age <= 89) %>%
	dplyr::select(-c(age))

x = nearZeroVar(sumTable, saveMetrics = TRUE)


sumTable1 <- sumTable %>%
				droplevels()%>%
				remove_constant()   %>%
				tidyr::gather(Variable, Value,-c(mergeid,gender,cciw,country,yrbirth))%>%
				filter(!(Value %in% c(9997,9996,9777)))%>%
				tidyr::spread(Variable,Value)

stargazer(sumTable1, omit.summary.stat = c("p25","p75"))

df_partner <- sumTable1
stargazer(df_partner)

#####################################children.RData##################################################

#Respondents children: here I select all the sahrelife variables in the retrospective children module

load("Dataset_constructed/children.RData")

sumTable <- base[,c("mergeid","cciw","age")] %>% 
  left_join(children) %>%  
	filter(age >= 49, age <= 89) %>%
	dplyr::select(-c(age))

x = nearZeroVar(sumTable, saveMetrics = TRUE)


children_df <- sumTable

stargazer(children_df, omit.summary.stat = c("p25","p75"))


#####################################Job episode panel.RData##################################################

n_job <- sharelife_new %>%
  dplyr::select(
  mergeid,
  sl_re015_1:sl_re015_20)%>%
  setNames(gsub( "(.*_)+", "",
            names(.))) %>% 
  gather(Ord_job,Job_title,
  -c(
     mergeid,
	 ))%>%
   na.omit(Job_title) %>%
   group_by(mergeid)%>%
   dplyr::summarise(n_job= n())

#Job title

sumTable <- base[,c("mergeid","cciw","age","gender")] %>% 
    left_join(current_adultJEP[,c("mergeid","industry","job_title","year_working","year_unemployed")]) %>% 
	filter(age >= 49, age <= 89) %>%
	dplyr::select(-c(age)) %>%
	droplevels()%>%
	remove_constant()   %>%
	dummy_cols(.,
				c(	
				"industry",
				"job_title"
				),
				remove_selected_columns = TRUE
				) 
x = nearZeroVar(sumTable, saveMetrics = TRUE)

sumTable <- sumTable[,-which(names(sumTable) %in% rownames(x[x[,"nzv"] > 0, ]))]

job_df <- sumTable %>% left_join(n_job)


stargazer(job_df, omit.summary.stat = c("p25","p75"))

#####################################################################################################
##################################### gl.RData #######################################################
######################################################################################################

#General life question. 

#This are created in the script: gl_question: ~Old_Script/gl_question

#Dataset stored in ~Datset_constructed/gl.RData

load("Dataset_constructed/gl.RData")
sumTable <- base[,c("mergeid","cciw","age")] %>% 
  left_join(gl_question) %>%  
	filter(age >= 49, age <= 89) %>%
	dplyr::select(-c(age, reason_discrimination)) 

x = nearZeroVar(sumTable, saveMetrics = TRUE)

sumTable <- sumTable[,-which(names(sumTable) %in% rownames(x[x[,"nzv"] > 0, ]))]

colnames(sumTable)<- sub("sl_gl023__","Discr:",colnames(sumTable))
gl_df <- sumTable

stargazer(gl_df, omit.summary.stat = c("p25","p75"))
#####################################################################################################
##################################### work_env.RData ################################################
#####################################################################################################

load("Dataset_constructed/work_env.RData")

#Working environment
#frequency tables

sumTable <- base[,c("mergeid","cciw","age","gender")] %>% 
  left_join(work_env) %>% 
	filter(age >= 49, age <= 89) %>%
	dplyr::select(-c(age))

x = nearZeroVar(sumTable, saveMetrics = TRUE)


sumTable1 <- sumTable %>%
				droplevels()%>%
				remove_constant()   %>%
				tidyr::gather(Variable, Value,-c(mergeid,gender,country,cciw,int_year,yrbirth))%>%
				filter(!(Value %in% c(9997,9996,9777)))%>%
				tidyr::spread(Variable,Value)

stargazer(sumTable1, omit.summary.stat = c("p25","p75"))

work_env_df <- sumTable
 
#####################################################################################################
##################################### ac_fn.RData.RData ###########################################
######################################################################################################
load("Dataset_constructed/ac_fn.RData")

sumTable <- base[,c("mergeid","cciw","age","gender")] %>% 
  left_join(ac_fn) %>%  
	filter(age >= 49, age <= 89) %>%
	dplyr::select(-c(age))

stargazer(sumTable1, omit.summary.stat = c("p25","p75"))

ac_fn_df <- sumTable

######################################################################################################
######################################### saving dataset #####################################################
######################################################################################################

 save(demographic, file = "MyData_file/demographic.RData")
 
 save(current_df, file = "MyData_file/current_df.RData")
 
 save(childhood, file = "MyData_file/childhood.RData")
 
 save(job_df, file = "MyData_file/job_df.RData")
 
 save(df_partner, file = "MyData_file/df_partner.RData")
 
 save(children_df, file = "MyData_file/children_df.RData")
 
 save(gl_df, file = "MyData_file/gl_df.RData")
 
 save(ac_fn_df, file = "MyData_file/ac_fn_df.RData")
 
 save(work_env_df, file = "MyData_file/work_env_df.RData")	
 