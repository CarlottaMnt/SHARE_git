#setwd("C:/Users/carlotta/Desktop/Dropbox/SHARE_new/R")
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
rm(list= ls())
load("Dataset_constructed/sharelife_new.RData")
load("Dataset_constructed/lifesatSL.RData")
load("MyData_file/childhood.RData")
#Accomodation_financial
#frequency tables

#Accomodation/financial and current status
 
  ac_fn <- sharelife_new %>%
  dplyr::select(
    mergeid,
    yrbirth,
    age_int,
    int_year,
    country,
    sl_ac002d1:sl_ac002dno, 	#Special events in accommodation
    sl_ac003_,			 	#When established own home
    sl_ac004_, 				#Residence where born
    sl_ac005_2:sl_ac005_29,	#Second residence where living              
    sl_ac006_1:sl_ac006_29, 	#When did you start living in 1/2/3/4..
    sl_ac007_1:sl_ac007_19, 	#start living in a accomodation 1/2/3/4/..
    sl_ac013_1:sl_ac013_29, 	# Was residence in current country?    
    sl_ac014c_1:sl_ac014c_20, # which country?       
    sl_ac017_1:sl_ac017_29, 	# Area of residence. rural/ urban
    sl_ac015c_1:sl_ac015c_29, #region of residence  1/2/3/4/..
    sl_ac021_1:sl_ac021_28, 	# Stopped living at residence 1 and 2 
    sl_ac027_, 				# PROXY CHECK
    sl_fs004_,			 	# Ever had any mutual funds
    sl_fs006_,  				# Ever had retirement account
    sl_fs008_, 	 			# Ever taken out a life insurance policy
    sl_fs010_,  				# Ever owned buiseness
    sl_fs013_) %>% 			# PROXY CHECK
    select(
      where(
        ~!all(is.na(.x))
      )
    )%>%
    droplevels()
  
  #Remove columns where there are more than 50% missing
  
  #ac_fn <- ac_fn[lapply(ac_fn , function(x) sum(is.na(x)) / length(x) ) < 0.90 ]
  
  #Creation of the data set: What about the treatment of refusal or don t know, are these 
  #relevant information about the question? Or about the answer? Maybe people tends to refuse 
  #to report something relevant. For now I leave them as they are.
  
  
  Never_endNa <-  function(x) replace(x, x %in% c(9997,9996,9799,9777) , NA)
  xc <- ac_fn %>% select(starts_with(c("sl_ac006_","sl_ac021_"))) %>% names(.)
  xc <- which(names(ac_fn) %in% xc)
  ac_fn[,xc] <-	lapply(ac_fn[,xc], Never_endNa)
  
  xc <- ac_fn %>% select(starts_with("sl_ac015c")) %>% names()
  xc <- which(names(ac_fn) %in% xc)
  
  ac_fn[xc] <- sapply(ac_fn[xc] ,as.character)
  #convert floating to one number
  xe <- ac_fn %>% dplyr::select(starts_with("sl_ac006")) %>% names()
  ac_fn$sl_residences1 = apply(ac_fn[,which(names(ac_fn) %in% xe)],1, function(x) sum(!is.na(x)))
  ac_fn$sl_residences2 = apply(ac_fn[,which(names(ac_fn) %in% xe)],1, function(x) sum(x < 0, na.rm = T))
  ac_fn$n_residences = ac_fn$sl_residences1 - ac_fn$sl_residences2
  
  ac_fn <- ac_fn %>%
  	mutate(
  	cohort = 
  	  ifelse((yrbirth < 1930),5,
  	  ifelse((yrbirth >= 1930 & yrbirth <= 1939),4,
  	  ifelse((yrbirth >= 1940 & yrbirth <= 1949),3,
  	  ifelse((yrbirth >= 1950 & yrbirth <= 1959),2,
  	  ifelse((yrbirth >= 1960 & yrbirth <= 1969),1, NA))))),
      sl_ac003_= case_when(
  		grepl(9997,sl_ac003_) ~ as.numeric(9997), #these are those that never set own home: terrible!!
  	  
  	#Clearly a typos
  		grepl(2975,sl_ac003_)~ as.numeric(1975),
  		grepl(9182,sl_ac003_)~ as.numeric(1982),
  		TRUE ~ as.numeric(sl_ac003_)),
  	
  	sl_ac006_1 = case_when(
  	# grepl(c(-1),sl_ac006_1)~ NA_real_,
  	# grepl(c(-2),sl_ac006_1)~ NA_real_,
  		grepl(9997,sl_ac006_1) ~ as.numeric(9997),
  		TRUE ~ as.numeric(sl_ac006_1)),
  	sl_ac006_2=case_when(
  	# grepl(c(-1),sl_ac006_2)~ NA_real_,
  	# grepl(c(-2),sl_ac006_2)~ NA_real_,
  		grepl(9997,sl_ac006_2) ~  as.numeric(9997),
  		TRUE ~ as.numeric(sl_ac006_2)),
  	sl_ac021_1=case_when(
  	# grepl(c(-1),sl_ac021_1)~ NA_real_,
  	# grepl(c(-2),sl_ac021_1)~ NA_real_,
  		grepl(9997,sl_ac021_1) ~ as.numeric(9997),
  		TRUE ~ as.numeric(sl_ac021_1)),
  	sl_ac021_2=case_when(
  	# grepl(c(-1),sl_ac021_2)~ NA_real_,
  	# grepl(c(-2),sl_ac021_2)~ NA_real_,
  		grepl(9997,sl_ac021_2) ~ as.numeric(9997),
  		TRUE ~ as.numeric(sl_ac021_2)),
  	#Special kind of accomodation when children 
  	children_house = case_when(
  		grepl("Selected",sl_ac002d1)~ as.numeric(1),
  		grepl("Not selected",sl_ac002d1)~ as.numeric(0),
  		grepl("Refusal",sl_ac002d1)~ as.numeric(-1),
  		grepl("Don't know",sl_ac002d1)~ as.numeric(-2),
  		TRUE ~ NA_real_),
  	foster_withother = case_when(
  		grepl("Selected",sl_ac002d2)~ as.numeric(1),
  		grepl("Not selected",sl_ac002d2)~ as.numeric(0),
  		grepl("Refusal",sl_ac002d2)~ as.numeric(-1),
  		grepl("Don't know",sl_ac002d2)~ as.numeric(-2),
  		TRUE ~ NA_real_),
  	evacuated_war = case_when(
  		grepl("Selected",sl_ac002d3)~ as.numeric(1),
  		grepl("Not selected",sl_ac002d3)~ as.numeric(0),
  		grepl("Refusal",sl_ac002d3)~ as.numeric(-1),
  		grepl("Don't know",sl_ac002d3)~ as.numeric(-2),
  		TRUE ~ NA_real_),
  	prisoner_camp = case_when(
  		grepl("Selected",sl_ac002d4)~ as.numeric(1),
  		grepl("Not selected",sl_ac002d4)~ as.numeric(0),
  		grepl("Refusal",sl_ac002d4)~ as.numeric(-1),
  		grepl("Don't know",sl_ac002d4)~ as.numeric(-2),
  		TRUE ~ NA_real_),
  	prison = case_when(
  		grepl("Selected",sl_ac002d5)~ as.numeric(1),
  		grepl("Not selected",sl_ac002d5)~ as.numeric(0),
  		grepl("Refusal",sl_ac002d5)~ as.numeric(-1),
  		grepl("Don't know",sl_ac002d5)~ as.numeric(-2),
  		TRUE ~ NA_real_),
  	labor_camp = case_when(
  		grepl("Selected",sl_ac002d6)~ as.numeric(1),
  		grepl("Not selected",sl_ac002d6)~ as.numeric(0),
  		grepl("Refusal",sl_ac002d6)~ as.numeric(-1),
  		grepl("Don't know",sl_ac002d6)~ as.numeric(-2),
  		TRUE ~ NA_real_),
  	concentration_camp= case_when(
  		grepl("Selected",sl_ac002d7)~ as.numeric(1),
  		grepl("Not selected",sl_ac002d7)~ as.numeric(0),
  		grepl("Refusal",sl_ac002d7)~ as.numeric(-1),
  		grepl("Don't know",sl_ac002d7)~ as.numeric(-2),
  		TRUE ~ NA_real_),
  	TBinstitution = case_when(
  		grepl("Selected",sl_ac002d8)~ as.numeric(1),
  		grepl("Not selected",sl_ac002d8)~ as.numeric(0),
  		grepl("Refusal",sl_ac002d8)~ as.numeric(-1),
  		grepl("Don't know",sl_ac002d8)~ as.numeric(-2),
  		TRUE ~ NA_real_),
  	psychiatric_hospital = case_when(
  		grepl("Selected",sl_ac002d9)~ as.numeric(1),
  		grepl("Not selected",sl_ac002d9)~ as.numeric(0),
  		grepl("Refusal",sl_ac002d9)~ as.numeric(-1),
  		grepl("Don't know",sl_ac002d9)~ as.numeric(-2),
  		TRUE ~ NA_real_),
  	homeless = case_when(
  		grepl("Selected",sl_ac002d10)~ as.numeric(1),
  		grepl("Not selected",sl_ac002d10)~ as.numeric(0),
  		grepl("Refusal",sl_ac002d10)~ as.numeric(-1),
  		grepl("Don't know",sl_ac002d10)~ as.numeric(-2),
  		TRUE ~ NA_real_),
  	no_bad_events = case_when(
  		grepl("Selected",sl_ac002dno)~ as.numeric(1),
  		grepl("Not selected",sl_ac002dno)~ as.numeric(0),
  		grepl("Refusal",sl_ac002d10)~ as.numeric(-1),
  		grepl("Don't know",sl_ac002d10)~ as.numeric(-2),
  		TRUE ~ NA_real_),
  	
  
  	#Create variable for urban rural and viceversa migration in childhood
  	rural_urban_1 = ifelse(n_residences == 1,0, ifelse((is.na(sl_ac017_2) & sl_ac015c_1 == sl_ac015c_2),0,ifelse(str_detect(sl_ac017_1,"rural") & str_detect(sl_ac017_2,"city"),1,0))),
  	urban_rural_1 = ifelse(n_residences == 1,0,ifelse((is.na(sl_ac017_2) & sl_ac015c_1 == sl_ac015c_2),0,ifelse(str_detect(sl_ac017_1,"city") & str_detect(sl_ac017_2,"rural"),1,0))),
  		
  	rural_urban_2 = ifelse(n_residences <= 2,0,ifelse((is.na(sl_ac017_2) & sl_ac015c_1 == sl_ac015c_2),0,ifelse(str_detect(sl_ac017_2,"rural") & str_detect(sl_ac017_3,"city"),1,0))),
  	urban_rural_2 = ifelse(n_residences <=  2,0,ifelse((is.na(sl_ac017_2) & sl_ac015c_1 == sl_ac015c_2),0,ifelse(str_detect(sl_ac017_2,"city") & str_detect(sl_ac017_3,"rural"),1,0))),
  	
  	rural_urban_3 = ifelse(n_residences <=  3,0,ifelse(str_detect(sl_ac017_3,"rural") & str_detect(sl_ac017_4,"city"),1,0)),
  	urban_rural_3 = ifelse(n_residences <=  3,0,ifelse(str_detect(sl_ac017_3,"city") & str_detect(sl_ac017_4,"rural"),1,0)),	
  	
  	rural_urban_4 = ifelse(n_residences <= 4,0,ifelse(str_detect(sl_ac017_4,"rural") & str_detect(sl_ac017_5,"city"),1,0)),
  	urban_rural_4 = ifelse(n_residences <=  4,0,ifelse(str_detect(sl_ac017_4,"city") & str_detect(sl_ac017_5,"rural"),1,0)),	
  	
  	rural_urban_5 = ifelse(n_residences <=  5,0,ifelse(str_detect(sl_ac017_5,"rural") & str_detect(sl_ac017_6,"city"),1,0)),
  	urban_rural_5 = ifelse(n_residences <=  5,0,ifelse(str_detect(sl_ac017_5,"city") & str_detect(sl_ac017_6,"rural"),1,0)),	
  	#Create a variable from small town to big town 
  	
  	small_large_1 =  ifelse(n_residences <= 1,0,ifelse(str_detect(sl_ac017_1,"small") & str_detect(sl_ac017_2,"large"),1,0)),
  	large_small_1 =  ifelse(n_residences <= 1,0,ifelse(str_detect(sl_ac017_1,"large") & str_detect(sl_ac017_2,"small"),1,0)),
  	
  	small_large_2 =  ifelse(n_residences <= 2,0,ifelse(str_detect(sl_ac017_2,"small") & str_detect(sl_ac017_3,"large"),1,0)),
  	large_small_2 =  ifelse(n_residences <= 2,0,ifelse(str_detect(sl_ac017_2,"large") & str_detect(sl_ac017_3,"small"),1,0)),
  	
  	small_large_3 =  ifelse(n_residences <= 3,0,ifelse(str_detect(sl_ac017_3,"small") & str_detect(sl_ac017_4,"large"),1,0)),
  	large_small_3 =  ifelse(n_residences <= 3,0,ifelse(str_detect(sl_ac017_3,"large") & str_detect(sl_ac017_4,"small"),1,0)),
  	
  	small_large_4 =  ifelse(n_residences <= 4,0,ifelse(str_detect(sl_ac017_4,"small") & str_detect(sl_ac017_5,"large"),1,0)),
  	large_small_4 =  ifelse(n_residences <= 4,0,ifelse(str_detect(sl_ac017_4,"large") & str_detect(sl_ac017_5,"small"),1,0)),
  	
  	small_large_5 =  ifelse(n_residences <= 5,0,ifelse(str_detect(sl_ac017_5,"small") & str_detect(sl_ac017_6,"large"),1,0)),
  	large_small_5 =  ifelse(n_residences <= 5,0,ifelse(str_detect(sl_ac017_5,"large") & str_detect(sl_ac017_6,"small"),1,0)),
  	
  	#Create variable for between country migration, change of country 
  	change_country_1 = ifelse((is.na(sl_ac013_1) & is.na(sl_ac013_2)),NA_real_,ifelse(is.na(sl_ac013_2),0, ifelse(sl_ac013_1 != sl_ac013_2,1,0))),
  	change_country_2 = ifelse((is.na(sl_ac013_1) & is.na(sl_ac013_3)),change_country_1,ifelse(is.na(sl_ac013_3),0, ifelse(sl_ac013_2 != sl_ac013_3,1,0))),
  	change_country_3 = ifelse((is.na(sl_ac013_3) & is.na(sl_ac013_4)),change_country_1,ifelse(is.na(sl_ac013_4),0, ifelse(sl_ac013_3 != sl_ac013_4,1,0))),
  	change_country_4 = ifelse((is.na(sl_ac013_4) & is.na(sl_ac013_5)),change_country_1,ifelse(is.na(sl_ac013_5),0, ifelse(sl_ac013_4 != sl_ac013_5,1,0))),
  	change_country_5 = ifelse((is.na(sl_ac013_5) & is.na(sl_ac013_6)),change_country_1,ifelse(is.na(sl_ac013_6),0, ifelse(sl_ac013_5 != sl_ac013_6,1,0))),
  	
  	#create global variable
  	change_country = case_when(
  					(change_country_1 == 1 ) ~ 1,
                      (change_country_2 == 1 ) ~ 1,
                      (change_country_3 == 1 ) ~ 1,
                      (change_country_4 == 1 ) ~ 1,
  					(change_country_5 == 1 ) ~ 1,
                      TRUE ~ 0),
      
  	#Create variable for intra country migration and change of region 
  	change_region_1 = ifelse((is.na(sl_ac015c_1) & is.na(sl_ac015c_2)) & is.na(change_country_1),NA_real_,ifelse(is.na(sl_ac015c_2) | is.na(sl_ac015c_1) & !is.na(change_country_1), change_country_1, ifelse(sl_ac015c_1 != sl_ac015c_2,1,0))),
  	
  	change_region_2 = ifelse((is.na(sl_ac015c_2) & is.na(sl_ac015c_3))& is.na(change_country_2),NA_real_,ifelse(is.na(sl_ac015c_2) | is.na(sl_ac015c_3) & !is.na(change_country_2),change_country_2, ifelse(sl_ac015c_2 != sl_ac015c_3,1,0))),
  	
  	change_region_3 = ifelse((is.na(sl_ac015c_3) & is.na(sl_ac015c_4))& is.na(change_country_3),NA_real_,ifelse(is.na(sl_ac015c_4) | is.na(sl_ac015c_3)& !is.na(change_country_3),change_country_3, ifelse(sl_ac015c_3 != sl_ac015c_4,1,0))),
  	
  	change_region_4 = ifelse((is.na(sl_ac015c_4) & is.na(sl_ac015c_5))& is.na(change_country_4),NA_real_,ifelse(is.na(sl_ac015c_5) | is.na(sl_ac015c_4)& !is.na(change_country_4),change_country_4, ifelse(sl_ac015c_4 != sl_ac015c_5,1,0))),
  	
  	change_region_5 = ifelse((is.na(sl_ac015c_5) & is.na(sl_ac015c_6))& is.na(change_country_5),NA_real_,ifelse(is.na(sl_ac015c_6) | is.na(sl_ac015c_5)& !is.na(change_country_5),change_country_5, ifelse(sl_ac015c_5 != sl_ac015c_6,1,0))),
  	
  	#create global variable
  	change_region = case_when((change_region_1 == 1 ) ~ 1,
                              (change_region_2  == 1 ) ~ 1,
                              (change_region_3  == 1 ) ~ 1,
                              (change_region_4  == 1 ) ~ 1,
  							(change_region_5  == 1 ) ~ 1,
                              TRUE ~ 0),
  	#Create variable for the timing of migration/displacement:	
  	
  	#Age first migration, for most is zero while is not zero for those that do not live in 
  	#the residence where they born.
  	#In 1
  	age_immigration_1 =  ifelse((sl_ac006_1 %in% c(-2,-1,-11))|(sl_ac006_1 > 9000), sl_ac006_1,ifelse(sl_ac006_1- yrbirth < 0,0,sl_ac006_1- yrbirth)), #this should be mostly zero for all	
  	
  	#Out 1
  	age_outmigration_1 = ifelse((sl_ac021_1 %in% c(-2,-1,-11))|( sl_ac021_1 > 9000),  sl_ac021_1,  sl_ac021_1- yrbirth),
  	
  	#In 2
  	age_immigration_2 =  ifelse((sl_ac006_2 %in% c(-2,-1,-11))|(sl_ac006_2 > 9000), sl_ac006_2, sl_ac006_2- yrbirth),
  	age_immigration_2 =  ifelse((age_immigration_2 %in% c(-2,-1,-11))|!is.na(age_immigration_2)|(age_immigration_2 > 9000), sl_ac007_2- yrbirth, age_immigration_2) ,
  	age_immigration_2 = ifelse(is.na(age_immigration_2) & !is.na(age_outmigration_1), age_outmigration_1,age_immigration_2), 
  	age_outmigration_1 =  ifelse(is.na(age_outmigration_1) & !is.na(age_immigration_2), age_immigration_2, age_outmigration_1),
  	
  	#Out 2
  	age_outmigration_2 =  ifelse((sl_ac021_2 %in% c(-2,-1,-11))|(sl_ac021_2 > 9000), sl_ac021_2, sl_ac021_2- yrbirth),
  	age_outmigration_2 =  ifelse(age_outmigration_2 < -2, NA_real_, age_outmigration_2),
  	
  	#In 3
  	age_immigration_3 =  ifelse((sl_ac006_3 %in% c(-2,-1,-11))|(sl_ac006_3 > 9000), sl_ac006_3, sl_ac006_3 - yrbirth),
  	age_immigration_3 =  ifelse((age_immigration_3 %in% c(-2,-1,-11))|!is.na(age_immigration_3)|(age_immigration_3 > 9000), sl_ac007_3 - yrbirth,age_immigration_3) ,
  	age_immigration_3 = ifelse(is.na(age_immigration_3) & !is.na(age_outmigration_2), age_outmigration_2,age_immigration_3), 
  	
  	#Out 3
  	age_outmigration_3 =  ifelse((sl_ac021_3 %in% c(-2,-1,-11))|(sl_ac021_3 > 9000), sl_ac021_3, sl_ac021_3- yrbirth),
  	age_outmigration_3 =  ifelse(age_outmigration_3 < -2, NA_real_, age_outmigration_3),
  	
  	age_outmigration_2 =  ifelse(is.na(age_outmigration_2) & !is.na(age_immigration_3), age_immigration_3, age_outmigration_2),
  
  	#In 4
  	age_immigration_4 =  ifelse((sl_ac006_4 %in% c(-2,-1,-11))|(sl_ac006_4 > 9000), sl_ac006_4, sl_ac006_4- yrbirth),
  	age_immigration_4 =  ifelse((age_immigration_4 %in% c(-2,-1,-11))|!is.na(age_immigration_4)|(age_immigration_4 > 9000), sl_ac007_4- yrbirth,age_immigration_4) ,
  	age_immigration_4 = ifelse(is.na(age_immigration_4) & !is.na(age_outmigration_3), age_outmigration_3,age_immigration_4), 
  	
  	#Out 4
  	age_outmigration_4 =  ifelse((sl_ac021_4 %in% c(-2,-1,-11))|(sl_ac021_4 > 9000), sl_ac021_4, sl_ac021_4- yrbirth),
    age_outmigration_4 =  ifelse(age_outmigration_4 < -2, NA_real_, age_outmigration_4),
  	
  	age_outmigration_3 =  ifelse(is.na(age_outmigration_3) & !is.na(age_immigration_4), age_immigration_4, age_outmigration_3),
  	#In 5
  	age_immigration_5 =  ifelse((sl_ac006_5  %in% c(-2,-1,-11))|(sl_ac006_5 > 9000), sl_ac006_5 , sl_ac006_5 - yrbirth),
  	age_immigration_5  =  ifelse((age_immigration_5 %in% c(-2,-1,-11))|!is.na(age_immigration_5)|(age_immigration_5 > 9000), sl_ac007_5- yrbirth,age_immigration_5) ,
  	age_immigration_5 = ifelse(is.na(age_immigration_5) & !is.na(age_outmigration_4), age_outmigration_4,age_immigration_5), 
  	#Out 5
  	age_outmigration_5 =  ifelse((sl_ac021_5 %in% c(-2,-1,-11))|(sl_ac021_5 > 9000), sl_ac021_5, sl_ac021_5- yrbirth),
  	age_outmigration_5 =  ifelse(age_outmigration_5 < -2, NA_real_, age_outmigration_5),	
  	age_outmigration_4 =  ifelse(is.na(age_outmigration_4) & !is.na(age_immigration_5), age_immigration_5, age_outmigration_4),
  	
  	#In 6
  	age_immigration_6 =  ifelse((sl_ac006_6  %in% c(-2,-1,-11))|(sl_ac006_6  > 9000), sl_ac006_6 , sl_ac006_6 - yrbirth),
  	age_immigration_6  =  ifelse((age_immigration_6 %in% c(-2,-1,-11))|!is.na(age_immigration_6)|(age_immigration_6 > 9000), sl_ac007_6- yrbirth,age_immigration_6) ,
  	age_immigration_6 = ifelse(is.na(age_immigration_6) & !is.na(age_outmigration_5), age_outmigration_5,age_immigration_6), 
  	#Out 6
  	age_outmigration_6 =  ifelse((sl_ac021_6 %in% c(-2,-1,-11))|(sl_ac021_6 > 9000), sl_ac021_6, sl_ac021_6- yrbirth),
  	age_outmigration_6 =  ifelse(age_outmigration_6 < -2, NA_real_, age_outmigration_6),	
  	
  	age_outmigration_5 =  ifelse(is.na(age_outmigration_5) & !is.na(age_immigration_6), age_immigration_6, age_outmigration_5),
  	) %>% 
    mutate(
	#Region replace to previous region if there is no migration
	
	region_1  = ifelse(is.na(sl_ac015c_1) & !is.na(sl_ac014c_1),as.character(sl_ac014c_1),as.character(sl_ac015c_1))
	) %>% 
  mutate(
	region_2  = ifelse(is.na(sl_ac015c_2) & change_region_1 == 0, as.character(region_1),as.character(sl_ac015c_2))
	) %>% 
  mutate(
	region_3  = ifelse(is.na(sl_ac015c_3) & change_region_2 == 0, as.character(region_2),as.character(sl_ac015c_3))
	) %>% 
	mutate(
  region_4  = ifelse(is.na(sl_ac015c_4) & change_region_3 == 0, as.character(region_3),as.character(sl_ac015c_4))
	) %>% 
  mutate(
  region_5  = ifelse(is.na(sl_ac015c_5) & change_region_4 == 0,as.character(region_4),as.character(sl_ac015c_5))
  ) %>% 
  mutate(
  region_6  = ifelse(is.na(sl_ac015c_6) & change_region_5 == 0,as.character(region_5),as.character(sl_ac015c_6)),
  ) %>% 
  mutate(
	#Country
	
	country_1 = ifelse(sl_ac013_1 == "Yes",as.character(country), as.character(sl_ac014c_1)),  
	country_2 = ifelse(is.na(sl_ac013_2)|sl_ac013_2 == "Yes",as.character(country), as.character(sl_ac014c_2)),  
	country_3 = ifelse(is.na(sl_ac013_3)|sl_ac013_3 == "Yes",as.character(country), as.character(sl_ac014c_3)),  
	country_4 = ifelse(is.na(sl_ac013_4)|sl_ac013_4 == "Yes",as.character(country), as.character(sl_ac014c_4)),  
	country_5 = ifelse(is.na(sl_ac013_5)|sl_ac013_5 == "Yes",as.character(country), as.character(sl_ac014c_5)),  
	country_6 = ifelse(is.na(sl_ac013_6)|sl_ac013_6 == "Yes",as.character(country),as.character(sl_ac014c_6)),  
	
	#Duration migration spell
	duration_spell_1 = ifelse(n_residences == 1, age_int - age_immigration_1, age_outmigration_1 - age_immigration_1),
	
	duration_spell_2 = ifelse(n_residences == 2, age_int - age_immigration_2, 
	ifelse(n_residences == 1, age_int - age_immigration_1,
	age_outmigration_2 - age_immigration_2)),
	
	duration_spell_3 =  ifelse(n_residences == 3, age_int - age_immigration_3,
	ifelse(n_residences == 2, age_int - age_immigration_2,
	ifelse(n_residences == 1, age_int - age_immigration_1, 
	age_outmigration_3 - age_immigration_3))),
	
	# ifelse(!is.na(age_immigration_3) & is.na(age_outmigration_3), age_int - age_immigration_3,,
	# ifelse(is.na(age_immigration_3)& is.na(age_outmigration_3), age_int - age_immigration_1,
	# ifelse(age_outmigration_3 %in% c(-1,-2),age_outmigration_3, age_outmigration_3 - age_immigration_3)),
	
	duration_spell_4 =  ifelse(n_residences == 4, age_int - age_immigration_4,
	ifelse(n_residences == 3, age_int - age_immigration_3,
	ifelse(n_residences == 2, age_int - age_immigration_2,
	ifelse(n_residences == 1, age_int - age_immigration_1, 
	age_outmigration_4 - age_immigration_4)))),
	# ifelse(!is.na(age_immigration_4)& is.na(age_outmigration_4),, age_int - age_immigration_4,
	# ifelse(is.na(age_immigration_4)& is.na(age_outmigration_3), age_int - age_outmigration_1,
	# ifelse(age_outmigration_4 %in% c(-1,-2),age_outmigration_4, age_outmigration_4 - age_immigration_4)),
	
	duration_spell_5 = ifelse(n_residences == 5, age_int - age_immigration_5,
	ifelse(n_residences == 4, age_int - age_immigration_4,
	ifelse(n_residences == 3, age_int - age_immigration_3,
	ifelse(n_residences == 2, age_int - age_immigration_2, 
	ifelse(n_residences == 1, age_int - age_immigration_1, 
	age_outmigration_5 - age_immigration_5))))),
	
	duration_spell_6 = ifelse(n_residences == 6, age_int - age_immigration_6,
	ifelse(n_residences == 5, age_int - age_immigration_5,
	ifelse(n_residences == 4, age_int - age_immigration_4,
	ifelse(n_residences == 3, age_int - age_immigration_3,
	ifelse(n_residences == 2, age_int - age_immigration_2, 
	ifelse(n_residences == 1, age_int - age_immigration_1, 
	age_outmigration_6 - age_immigration_6)))))),
	)%>%
	mutate(
	age_immigration_1 =  ifelse(age_immigration_1 < -2, NA_real_, age_immigration_1),
	age_outmigration_1 = ifelse((age_outmigration_1 < -2), NA_real_, age_outmigration_1),
	
	age_immigration_2 =  ifelse((age_immigration_2 < -2), NA_real_, age_immigration_2),
	age_outmigration_2 =  ifelse(	age_outmigration_2 < -2, NA_real_, 	age_outmigration_2),
	
	age_immigration_3 =  ifelse(age_immigration_3  < -2, NA_real_, age_immigration_3),
	age_outmigration_3 =  ifelse(age_immigration_3  < -2, NA_real_, age_outmigration_3),
	
	age_immigration_4 =  ifelse(age_immigration_4 < -2, NA_real_, age_immigration_4),
	age_outmigration_4 =  ifelse(age_outmigration_4 < -2, NA_real_, age_outmigration_4),
	
	age_immigration_5 =  ifelse(age_immigration_5 < -2, NA_real_, age_immigration_5),
	age_outmigration_5 =  ifelse(age_outmigration_5 < -2, NA_real_, age_outmigration_5),

	age_immigration_6 =  ifelse(age_immigration_6 < -2, NA_real_,age_immigration_6),
	age_outmigration_6 =  ifelse(age_outmigration_6 < -2, NA_real_, age_outmigration_6),

		#remove duration spell that are negatives
	duration_spell_1 = ifelse(duration_spell_1 < -2, NA_real_, duration_spell_1),
	duration_spell_2 = ifelse(duration_spell_2 < -2, NA_real_, duration_spell_2),
	duration_spell_3 = ifelse(duration_spell_3 < -2, NA_real_, duration_spell_3),
	duration_spell_4 = ifelse(duration_spell_4 < -2, NA_real_, duration_spell_4),
	duration_spell_5 = ifelse(duration_spell_5 < -2, NA_real_, duration_spell_5),
	duration_spell_6 = ifelse(duration_spell_6 < -2, NA_real_, duration_spell_6),
	#Age when establish own home/household
	
	age_ownhome = ifelse((sl_ac003_ %in% c(-2,-1,-11))|(sl_ac003_ > 9000), sl_ac003_, sl_ac003_- yrbirth),
	ever_ownhome = ifelse((sl_ac003_ %in% c(-2,-1,-11)),sl_ac003_,ifelse(sl_ac003_ == 9997, 0, 1)),
	#What happens when adult
	ever_mutualfunds = case_when(
		grepl("Yes",sl_fs004_) ~ as.numeric(1),
		grepl("No",sl_fs004_) ~ as.numeric(0),
		grepl("Refusal",sl_fs004_)~ as.numeric(-1),
		grepl("Don't know",sl_fs004_)~ as.numeric(-2),
		TRUE ~ NA_real_),	
	ever_retirement_account = case_when(
		grepl("Yes",sl_fs006_) ~ as.numeric(1),
		grepl("No",sl_fs006_) ~ as.numeric(0),
		grepl("Refusal",sl_fs006_) ~ as.numeric(-1),
		grepl("Don't know",sl_fs006_) ~ as.numeric(-2),
		TRUE ~ NA_real_),	
	ever_life_insurance_policy = case_when(
		grepl("Yes",sl_fs008_) ~ as.numeric(1),
		grepl("No",sl_fs008_) ~ as.numeric(0),
		grepl("Refusal",sl_fs008_) ~ as.numeric(-1),
		grepl("Don't know",sl_fs008_) ~ as.numeric(-2),
		TRUE ~ NA_real_),	
	ever_owned_business = case_when(
		grepl("Yes",sl_fs010_) ~ as.numeric(1),
		grepl("No",sl_fs010_) ~ as.numeric(0),
		grepl("Refusal",sl_fs010_) ~ as.numeric(-1),
		grepl("Don't know",sl_fs010_) ~ as.numeric(-2),
		TRUE ~ NA_real_),
	) 
	


ac_fn <- ac_fn %>%
	dplyr::select(-starts_with(c("sl_fs0", "sl_ac0")),-c(sl_residences2,sl_residences1)) 

#remove other unused variables
change_region <- ac_fn %>%
	select(mergeid,change_region, n_residences, sl_ac015c_1,sl_ac015c_2, age_immigration1,urban_rural1 ,age_outmigration1,age_immigration2)%>%
	filter(change_region == 1)

#Basic plot
ac_fn %>%
	filter(change_region_1 == 1)%>%
	summarise(mean_coun = sum(change_country_1, na.rm = T))
ggplot(ac_fn, aes(age_outmigration_1)) + geom_density() 

xe <- which(ac_fn$change_region == 1 )
mean(ac_fn[xe,"change_country"])

ac_fn[xe,] %>%	ggplot(.,aes(duration_spell_2))+ geom_histogram()

#Merge the migration history with life satisfaction
lifesat_migrantion <- merge(ac_fn , lifesatSL, by = c("mergeid", "country"))%>%
					filter(!(is.na(ac012_)),!(ac012_ %in% c(-1,-2,-11)),change_region == 1)%>%
					mutate( agesq = (age_int.y)^2)
					
mode1 <- lm(ac012_~  age_int.y + agesq ,  lifesat_migrantion)
lifesat_adj <- residuals(mode1)

childhood_migration <- merge(ac_fn ,childhood, by = c("mergeid", "country"))
			
childhood_migration %>%
    dplyr::filter(!(ISCO_par  %in% c(-1,-2)),!(age_outmigration_1  %in% c(-1,-2)))%>%
	dplyr::group_by(ISCO_par,country_1,age_outmigration_1) %>%
	dplyr::summarise(n = n())


lifesat_migrantion <- cbind(lifesat_adj ,lifesat_migrantion)

ggplot(lifesat_migrantion, aes(y = lifesat_adj, x = age_outmigration_1)) + geom_smooth() + xlab("age_at_migration") + ylab("adj_lifesat")
ggplot(lifesat_migrantion, aes(y = ac012_, x = age_outmigration_1)) + geom_smooth() + xlab("age_at_migration") + ylab("adj_lifesat")
#Frequency tables for each variables
attach(ac_fn)
y <- ever_owned_business[ever_owned_business >=0 & !(is.na(ever_owned_business)) & !is.na(rural_urban) & !is.na(urban_rural)]
T1 <- rural_urban[ever_owned_business >=0 & !(is.na(ever_owned_business)) &!is.na(rural_urban) & !is.na(urban_rural)]
T2 <- urban_rural[!is.na(rural_urban) & !is.na(urban_rural)]


stargazer(ac_fn)

ac_fn %>% filter(age_first_moveout < 18) %>%
 group_by(country) %>%
 summarise(n())
##Causal effect of migration on those left behind


save(ac_fn,file = "Dataset_constructed/ac_fn.RData")
