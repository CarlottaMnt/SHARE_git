#setwd("C:/Users/carlotta/Desktop/Dropbox/SHARE_new/R")
#setwd("//crc/Team_work/Caralpvk/SHARE_new/R")

if(any(!has)) install.packages(wants[!has])
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
library(sf)
#import the data
rm(list =ls())
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
source("Functions.R")
#this comes from the ~Old_script/sharelife_merge script. 57740 respondent, No Ireland but yes people aged less 50.
load("Dataset_constructed/sharelife_new.RData")
#Which are the items with less than 25% missing data?
x <- sharelife_new[lapply(sharelife_new, function(x) sum(is.na(x)) / length(x) ) < 0.25 ] #select columns for which the numebr of missing is less then 10% of the overall

#Start creating new variables

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

####################################Country birth and year of education####################################

load("Dataset_constructed/current_adultJEP_new.RData")

#Country of birth
sumTable <- base[,c("mergeid","cciw","age","yrbirth","gender","country")] %>% 
            left_join(current_adultJEP) %>%      
	dplyr::select(mergeid,age,yrbirth,cciw,country_birth_,country_birth_2, gender,yedu,country)%>%
	filter(age >= 49, age <= 89) %>%
	mutate(cohort = 
			ifelse((yrbirth < 1930),"< 1930",
			ifelse((yrbirth >= 1930 & yrbirth <= 1939),"1930-1939",
			ifelse((yrbirth >= 1940 & yrbirth <= 1949),"1940-1949",
			ifelse((yrbirth >= 1950 & yrbirth <= 1959),"1950-1959",
			ifelse((yrbirth >= 1960 & yrbirth <= 1969),"1960-1969",
			ifelse((yrbirth >= 1970),">= 1970", NA))))))
			)%>%
	mutate(cohort = as.factor(cohort))%>%
	dplyr::group_by(country_birth_2,gender)%>%
	dplyr::summarise(n_weight = sum(cciw,na.rm= TRUE),n_unweight = n()) 



DF.wide <- dcast(sumTable, country_birth_2 ~  gender , value.var="n_unweight")

DF.wide$ratio_fm <- DF.wide[["Female"]]/(DF.wide[["Male"]])
DF.wide$ratio_ftot <- DF.wide[["Female"]]/(DF.wide[["Female"]] + DF.wide[["Male"]])
DF.wide$tot <- DF.wide[["Female"]] + DF.wide[["Male"]]
kable(DF.wide,format="latex", digits=2)
sum(DF.wide$Female, na.rm = TRUE) + sum(DF.wide$Male, na.rm = TRUE)

kable(DF.wide,format="latex", digits=2)

#Educational level
load("Dataset_constructed/imputation_SL.RData")

sumTable <- base[,c("mergeid","cciw","gender","country","age")] %>%
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
	dplyr::group_by(country,isced_2,gender)%>%
	dplyr::summarise(n_weight = sum(cciw,na.rm= TRUE),n_unweight = n())



DF.wide <- dcast(sumTable, country ~   gender + isced_2 , value.var="n_unweight")
DF.wide$tot <- apply(DF.wide[,-1],1,sum, na.rm = TRUE)
tot <- apply(DF.wide[,-1],2,sum, na.rm = TRUE)
DF.wide = rbind(DF.wide,tot)
kable(DF.wide,format="latex", digits=2)

#CREATION ON THE DEMOGRAPHIC DATA FRAME

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
 
graphdf <- demographic %>% 
	mutate(
		age = Hmisc::cut2(age, g = 4, include.lowest = TRUE),
		yedu = Hmisc::cut2(yedu , g = 5, include.lowest = TRUE),
		)%>%
	gather(Variable,Value,-c(mergeid,gender,cciw))%>%
	filter(!is.na(Value))
 

ggplot(graphdf,aes(Value, col = gender, fill = gender))+ 
	geom_bar()+
	facet_wrap(~Variable,scales = "free")+
	theme(axis.text.x = element_text(angle = 90))
##################################### CURRENT ADULT DATASET #######################################

######################################################################################################
##################################### imputation_SL.RData ############################################
######################################################################################################

#Education ,current marital status, current income, current wealth: from imputation modules. 

#This dataset has been created in the script ~Old_Script/gv_imputationSL.R (be aware there is another script similar!!)
#Dataset stored in the folder Dataset_constructed.

load("Dataset_constructed/imputation_SL.RData")
load("Dataset_constructed/current_adultJEP_new.RData")

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
#Distribution of retired across age group and countries
sumTable <- current_df %>% 
	filter(!is.na(cur_retired)) %>%
	droplevels() %>%
	filter(age_int >= 49, age_int <= 89)%>%
	mutate(age_group = 
	  ifelse((age_int >= 49 & age_int <=59),"49-59",
	  ifelse((age_int >= 60 & age_int<=69),"60-69",
	  ifelse((age_int >= 70 & age_int <=79),"70-79",
	  ifelse((age_int>= 80 & age_int <=89),"80-89", NA)))))%>%
	 mutate(age_group = as.factor(age_group)
	 )%>%
	dplyr::group_by(country,age_group, cur_retired) %>%
	dplyr::summarise(n_weight = sum(cciw,na.rm= TRUE),n_unweight = n())



DF.wide <- dcast(sumTable, country ~  age_group + cur_retired, value.var="n_unweight")
Not_retired <- round(DF.wide[["80-89_0"]]/DF.wide1[["80-89"]],2)
DF.wide1$tot <- apply(DF.wide[,-1],1,sum, na.rm = TRUE)
tot <- apply(DF.wide[,-1],1,sum, na.rm = TRUE)
DF.wide = rbind(DF.wide,tot)
kable(DF.wide,format="latex", digits=2)

#Plot of Year of education
current_df  %>%
 ggplot(.,aes(yedu,fill = gender, col = gender))+
 geom_histogram() + 
 facet_wrap(vars(country),scales = "free") + 
 ggtitle("Years of Education")

#Plot ofIncome
current_df  %>%
 filter(income != 0) %>%
 ggplot(.,aes(log(income),fill = gender, col = gender))+
 geom_histogram() + 
 facet_wrap(vars(country),scales = "free") + 
 ggtitle("(Log)Income")

graphdf <- current_df %>% 
	select(-c(country))%>%
	mutate(
		cur_employed = as.factor(cur_employed),
		cur_retired = as.factor(cur_retired),
		never_work = as.factor(never_work),	
		cur_married = as.factor(cur_married),
		cur_age_youngest_child = Hmisc::cut2(cur_age_youngest_child, g = 4, include.lowest = TRUE),
		age_when_retired = Hmisc::cut2(age_when_retired, g = 4, include.lowest = TRUE),
		income = Hmisc::cut2(income, g = 10, include.lowest = TRUE),
		con_in = Hmisc::cut2(con_in, g = 10, include.lowest = TRUE),
		con_out = Hmisc::cut2(con_out , g = 10, include.lowest = TRUE),
		yedu = Hmisc::cut2(yedu , g = 5, include.lowest = TRUE),
		)%>%
	gather(Variable,Value,-c(mergeid,gender,cciw))%>%
	filter(!is.na(Value))
 

ggplot(graphdf,aes(Value, col = gender, fill = gender))+ 
	geom_bar()+
	facet_wrap(~Variable,scales = "free")+
	theme(axis.text.x = element_text(angle = 90))

######################################################################################################
##################################### adult_health.RData ############################################
######################################################################################################

#Retrospective adult health
#Dataset stored in the folder Dataset_constructed.
#ONLY SHARELIFE W3 SO WE CANNOT USE IT
load("Dataset_constructed/adult_health.RData")

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


#Frequency tables
graphdf_ses <- merge(base[,c("mergeid","age","gender")], df_child, by = c("mergeid"))%>%  
	filter(age >= 49, age <= 89) %>%
	mutate(
	roomperson = Hmisc::cut2(roomperson, g = 4, include.lowest = TRUE)
	) %>%
	dplyr::select(-c(age,sl_ac002d1,math_level,language_level))%>%
	dplyr::select(c(1:18))%>%
	droplevels()%>%
	remove_constant() %>%
	gather(Variable,Value,-c(mergeid,gender))%>%
	filter(!is.na(Value)) %>%
	filter(!(Value %in% c(-1,-2,9997,9996,9777)))
 
ggplot(graphdf_ses,aes(Value, col = gender, fill = gender))+ 
	geom_bar()+
	facet_wrap(~Variable,scales = "free")+
	theme(axis.text.x = element_text(angle = 90))+ ggtitle("Childohod SES")

graphdf_health <- merge(base[,c("mergeid","age","gender")], df_child, by = c("mergeid"))%>%  
	filter(age >= 49, age <= 89) %>%
	dplyr::select(-c(age))%>%
	dplyr::select(c(1:2,21:37))%>%
	droplevels()%>%
	remove_constant() %>%
	gather(Variable,Value,-c(mergeid,gender))%>%
	filter(!is.na(Value)) %>%
	filter(!(Value %in% c(-1,-2,9997,9996,9777)))
 
ggplot(graphdf_health,aes(Value, col = gender, fill = gender))+ 
	geom_bar()+
	facet_wrap(~Variable,scales = "free")+
	theme(axis.text.x = element_text(angle = 90))+ ggtitle("Childhood Health")

#####################################partner.RData##################################################
#Respondent family situation
#install.packages("DataCombine")
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

#Frequency tables
graphdf <- sumTable1 %>% 
	mutate(
		ever_breakdown = as.factor(ever_breakdown),
		ever_cohabit = as.factor(ever_cohabit),
		ever_married = as.factor(ever_married),	
		ever_widowed = as.factor(ever_widowed),	
		ever_noncohabitating = as.factor(ever_noncohabitating),
		still_with_firstpartner = as.factor(still_with_firstpartner),
		age_first_married = Hmisc::cut2(age_first_married, g = 4, include.lowest = TRUE),
		age_first_noncohabitation = Hmisc::cut2(age_first_noncohabitation, g = 4, include.lowest = TRUE),
		age_first_divorce = Hmisc::cut2(age_first_divorce , g = 4, include.lowest = TRUE),
		age_first_widowed = Hmisc::cut2(age_first_widowed, g = 4, include.lowest = TRUE),
		age_first_break = Hmisc::cut2(age_first_break, g = 4, include.lowest = TRUE),	
		)%>%
	gather(Variable,Value,-c(mergeid,gender,cciw,yrbirth,country))%>%
	filter(!is.na(Value))
 
ggplot(graphdf,aes(Value, col = gender, fill = gender))+ 
	geom_bar()+
	facet_wrap(~Variable,scales = "free")+
	theme(axis.text.x = element_text(angle = 90))

#####################################children.RData##################################################

#Respondents children: here I select all the sahrelife variables in the retrospective children module
load("Dataset_constructed/children.RData")

sumTable <- base[,c("mergeid","cciw","age")] %>% 
  left_join(children) %>%  
	filter(age >= 49, age <= 89) %>%
	dplyr::select(-c(age))

x = nearZeroVar(sumTable, saveMetrics = TRUE)



# sumTable1 <- sumTable %>%
# 				droplevels()%>%
# 				remove_constant()   %>%
# 				tidyr::gather(Variable, Value,-c(mergeid,gender_child_1,gender_child_2,gender,cciw,country,yrbirth))%>%
# 				filter(!(Value %in% c(9997,9996,9777)))%>%
# 				tidyr::spread(Variable,Value)
# 				
children_df <- sumTable

stargazer(sumTable, omit.summary.stat = c("p25","p75"))

#Frequency tables
graphdf <- sumTable1 %>% 
	mutate(
		ever_children = as.factor(ever_children),
		ever_adopted_ch= as.factor(ever_adopted_ch),
		first_ch_d = as.factor(first_ch_d),	
		second_ch_d = as.factor(second_ch_d),	
		third_ch_d = as.factor(first_ch_d),	
		age_when_first_child = Hmisc::cut2(age_when_first_child, g = 4, include.lowest = TRUE),
		age_when_first_child_die = Hmisc::cut2(age_when_first_child_die, g = 4, include.lowest = TRUE),
		age_when_second_child = Hmisc::cut2(age_when_second_child , g = 4, include.lowest = TRUE),
		age_when_second_child_die = Hmisc::cut2(age_when_second_child_die, g = 4, include.lowest = TRUE)
		)%>%
	gather(Variable,Value,-c(mergeid,gender,cciw,yrbirth,country))%>%
	filter(!is.na(Value))
 

ggplot(graphdf,aes(Value, col = gender, fill = gender))+ 
	geom_bar()+
	facet_wrap(~Variable,scales = "free")+
	theme(axis.text.x = element_text(angle = 90))

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



graphdf <- job_df %>%
	gather(Variable,Value,-c(mergeid,gender,cciw))%>%
	filter(!is.na(Value))
 

ggplot(graphdf,aes(Value, col = gender, fill = gender))+ 
	geom_bar()+
	facet_wrap(~Variable,scales = "free")+
	theme(axis.text.x = element_text(angle = 90))

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

# sumTable1 <- sumTable %>%
#             droplevels()%>%
# 			remove_constant()   %>%
# 			tidyr::gather(Variable, Value,-c(mergeid,country,gender,cciw,age_int,int_year,yrbirth, reason_discrimination))%>%
# 			filter(!(Value %in% c(-1,-2, 9997,9996,9777)))%>%
# 			tidyr::spread(Variable,Value)

#stargazer(sumTable1, omit.summary.stat = c("p25","p75"))

colnames(sumTable)<- sub("sl_gl023__","Discr:",colnames(sumTable))
gl_df <- sumTable


graphdf <- sumTable %>% 
	mutate(
		ever_stress = as.factor(ever_stress),
		ever_financialstress = as.factor(ever_financialstress),
		ever_happier = as.factor(ever_happier),	
		age_when_start_fin_stressperiod = Hmisc::cut2(age_when_start_fin_stressperiod, g = 4, include.lowest = TRUE),
		age_when_start_happyperiod = Hmisc::cut2(age_when_start_happyperiod, g = 4, include.lowest = TRUE),	
		age_when_start_stressperiod = Hmisc::cut2(age_when_start_stressperiod,g = 4, include.lowest = TRUE),
		age_when_stop_fin_stressperiod = Hmisc::cut2(age_when_stop_fin_stressperiod , g = 4, include.lowest = TRUE),
		age_when_stop_happyperiod = Hmisc::cut2(age_when_stop_happyperiod, g = 4, include.lowest = TRUE),
		age_when_stop_stressperiod = Hmisc::cut2(age_when_stop_stressperiod, g = 4, include.lowest = TRUE)
		)%>%
	gather(Variable,Value,-c(mergeid,gender,country,gender,cciw,age_int,int_year,yrbirth))%>%
	filter(!is.na(Value))
 

ggplot(graphdf,aes(Value, col = gender, fill = gender))+ 
	geom_bar()+
	facet_wrap(~Variable,scales = "free")+
	theme(axis.text.x = element_text(angle = 90))

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
#Frequency tables
graphdf <- sumTable1 %>%
 gather(Variable,Value,-c(mergeid,yrbirth,gender,cciw,country,int_year))
graphdf$Value <- factor(graphdf$Value ,
levels = c(-1,-2,1,2,3,4),
labels = c("Refusal","Don't know","Strongly agree", "Agree", "Disagree","Strongly disagree"))
 
ggplot(graphdf,aes(Value,col = gender, fill = gender))+
 geom_bar()+
 facet_wrap(~Variable)+
 theme(axis.text.x = element_text(angle = 90))


 
#####################################################################################################
##################################### ac_fn.RData.RData ###########################################
######################################################################################################
load("Dataset_constructed/ac_fn.RData")

sumTable <- base[,c("mergeid","cciw","age","gender")] %>% 
  left_join(ac_fn) %>%  
	filter(age >= 49, age <= 89) %>%
	dplyr::select(-c(age))

# x = nearZeroVar(sumTable, saveMetrics = TRUE)
# 
# sumTable <- sumTable[,-which(names(sumTable) %in% rownames(x[x[,"nzv"] > 0, ]))]
# 
# sumTable1 <- sumTable %>%
# 				droplevels()%>%
# 				remove_constant() %>%
# 				tidyr::gather(Variable, Value,-c(mergeid,gender,cciw,int_year,yrbirth,type_privateresidence))%>%
# 				filter(!(Value %in% c(9997,9996,9777)))%>%
# 				tidyr::spread(Variable,Value)

stargazer(sumTable1, omit.summary.stat = c("p25","p75"))
ac_fn_df <- sumTable
#Frequency tables
graphdf <- sumTable %>% 
	mutate(
		ever_life_insurance_policy = as.factor(ever_life_insurance_policy),
		ever_mutualfunds = as.factor(ever_mutualfunds),
		ever_retirement_account = as.factor(ever_retirement_account),	
		no_bad_events = as.factor(no_bad_events),
		age_ownhome = Hmisc::cut2(age_ownhome, g = 4, include.lowest = TRUE)
		)%>%
	gather(Variable,Value,-c(mergeid,gender,cciw,int_year,yrbirth))%>%
	filter(!is.na(Value))
 

ggplot(graphdf,aes(Value, col = gender, fill = gender))+ 
	geom_bar()+
	facet_wrap(~Variable,scales = "free")+
	theme(axis.text.x = element_text(angle = 90))


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
 