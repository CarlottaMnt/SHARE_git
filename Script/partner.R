setwd("C:/Users/carlotta/Desktop/Dropbox/SHARE_new/R")
setwd("//crc/Team_work/Caralpvk/SHARE_new/R")

#PACKAGES

library(caret)
library(tidyverse)
library(dplyr)
library(fastDummies)
library(naniar)
library(DataCombine)
######################################################################################################
##################################### partner.RData #######################################################
######################################################################################################
rm(list = ls())
load("Dataset_constructed/sharelife_new.RData")

#Here I select all the variables in sharelife in the partner section

partner_df <- sharelife_new %>%
 dplyr::select(
        mergeid,
		gender,
		country,
		yrbirth,
		int_year,
		sl_rp002_, # EVER BEEN MARRIED
		sl_rp002d_,# EVER lived unmarried together with someone as a couple
		sl_rp002e_,# How often married
		sl_rp003_11: sl_rp003_17,#Year start living with partner
		sl_rp004b_1: sl_rp004b_5, #Year start living with married partner
		sl_rp004c_1: sl_rp004c_17,#When start relation with partner
		sl_rp008_1: sl_rp008_5,# Year married
		sl_rp009_1:sl_rp009_16,# STILL LIVING WITH PARTNER
		sl_rp010_1:sl_rp010_16,# Reason not living with partner
		sl_rp011_1:sl_rp011_15,# year of partner die
		sl_rp012_1:sl_rp012_17,# year stop living partner
		sl_rp013_1:sl_rp013_4, # Divorced partner
		sl_rp014_1:sl_rp014_4, # year of divorce
		sl_rp015a_11:sl_rp015a_17, #cohabitation		
		sl_rp016_ ,# NON COHABITATING PARTNERS 
		sl_rp017_1:sl_rp017_5, #year start non cohabiting relation
		sl_rp019_1: sl_rp019_5, #still with non-cohabitation partner
		sl_rp020_1: sl_rp020_5, #end non-cohabitation 
		sl_rp021_1:sl_rp021_5 #any other long term relation
    )


#Here I select all the variables in sharelife in the partner section with non response rate < 80%

x <- partner_df[lapply(partner_df, function(x) sum(is.na(x)) / length(x) ) < 0.90]

#Here I create the variable of interest
partner <- 	partner_df %>%
	mutate(
	  
	#Marriages
	ever_married = case_when(
		grepl("Yes",sl_rp002_,fixed =TRUE)~ 1,
		grepl("No",sl_rp002_,fixed =TRUE)~ 0,
		grepl("Refusal",sl_rp002_,fixed =TRUE)~ -2,
		grepl("Don't know",sl_rp002_,fixed =TRUE)~ -1,
		TRUE ~ NA_real_),
	
	ever_unmarried_partner <- case_when(
		grepl("Yes",sl_rp002d_,fixed =TRUE)~ 1,
		grepl("No",sl_rp002d_,fixed =TRUE)~ 0,
		grepl("Refusal",sl_rp002d_,fixed =TRUE)~ -2,
		grepl("Don't know",sl_rp002d_,fixed =TRUE)~ -1,
		TRUE ~ NA_real_),
		
	n_marriages = ifelse(ever_married == 1 & sl_rp002e_ == 0, 1, sl_rp002e_),
	
	sl_rp013_1 = case_when(
	  grepl("Yes", sl_rp009_1)~ "No",
	  grepl("No",  sl_rp009_1)~ as.character(sl_rp013_1),
	  TRUE ~ NA_character_
	),
	
	age_married_1 = ifelse((sl_rp008_1 %in% c(-2,-1,-11)), sl_rp008_1, sl_rp008_1- yrbirth),
	age_married_2 = ifelse((sl_rp008_2 %in% c(-2,-1,-11)), sl_rp008_2, sl_rp008_2- yrbirth),
	age_married_3 = ifelse((sl_rp008_3 %in% c(-2,-1,-11)), sl_rp008_3, sl_rp008_3- yrbirth),
	age_married_4 = ifelse((sl_rp008_4 %in% c(-2,-1,-11)), sl_rp008_4, sl_rp008_4- yrbirth),

	
	marry_1 = ifelse(ever_married != 0, 1,0),
	marry_2 = ifelse(n_marriages > 1, 1,0),
	marry_3_plus = ifelse(n_marriages > 2, 1,0),
	marry_4 = ifelse(n_marriages > 3,1,0),
	
	still_with_partner_1 = case_when(
		grepl("Yes",sl_rp009_1,fixed =TRUE)~ 1,
		grepl("No",sl_rp009_1,fixed =TRUE)~ 0,
		grepl("Refusal",sl_rp009_1,fixed =TRUE)~ -2,
		grepl("Don't know",sl_rp009_1,fixed =TRUE)~ -1,
		TRUE ~ NA_real_),
	
	still_with_partner_2 = case_when(
		grepl("Yes",sl_rp009_2,fixed =TRUE)~ 1,
		grepl("No",sl_rp009_2,fixed =TRUE)~ 0,
		grepl("Refusal",sl_rp009_2,fixed =TRUE)~ -2,
		grepl("Don't know",sl_rp009_2,fixed =TRUE)~ -1,
		TRUE ~ NA_real_),
	
	still_with_partner_3 = case_when(
		grepl("Yes",sl_rp009_3,fixed =TRUE)~ 1,
		grepl("No",sl_rp009_3,fixed =TRUE)~ 0,
		grepl("Refusal",sl_rp009_3,fixed =TRUE)~ -2,
		grepl("Don't know",sl_rp009_3,fixed =TRUE)~ -1,
		TRUE ~ NA_real_),
	
	still_with_partner_4 = case_when(
		grepl("Yes",sl_rp009_4,fixed =TRUE)~ 1,
		grepl("No",sl_rp009_4,fixed =TRUE)~ 0,
		grepl("Refusal",sl_rp009_4,fixed =TRUE)~ -2,
		grepl("Don't know",sl_rp009_4,fixed =TRUE)~ -1,
		TRUE ~ NA_real_),
	
	#COHABITATING######################################################################################### 
	
	ever_cohabit = case_when(
		grepl("Yes",sl_rp002d_,fixed =TRUE)~ 1,
		grepl("No",sl_rp002d_,fixed =TRUE)~ 0,
		grepl("Refusal",sl_rp002d_,fixed =TRUE)~ -2,
		grepl("Don't know",sl_rp002d_,fixed =TRUE)~ -1,
		TRUE ~ NA_real_),
	
	ever_cohabit_1 = case_when(
	  grepl("Yes",sl_rp015a_11,fixed =TRUE)~ 1,
	  grepl("No",sl_rp015a_11,fixed =TRUE)~ 0,
	  grepl("Refusal",sl_rp015a_11,fixed =TRUE)~ -2,
	  grepl("Don't know",sl_rp015a_11,fixed =TRUE)~ -1,
	TRUE ~ NA_real_),

	ever_cohabit_2 = case_when(
	  grepl("Yes",sl_rp015a_12,fixed =TRUE)~ 1,
	  grepl("No",sl_rp015a_12,fixed =TRUE)~ 0,
	  grepl("Refusal",sl_rp015a_12,fixed =TRUE)~ -2,
	  grepl("Don't know",sl_rp015a_12,fixed =TRUE)~ -1,
	TRUE ~ NA_real_),

	ever_cohabit_3 = case_when(
	  grepl("Yes",sl_rp015a_13,fixed =TRUE)~ 1,
	  grepl("No",sl_rp015a_13,fixed =TRUE)~ 0,
	  grepl("Refusal",sl_rp015a_13,fixed =TRUE)~ -2,
	  grepl("Don't know",sl_rp015a_13,fixed =TRUE)~ -1,
	TRUE ~ NA_real_),
	
	age_start_cohabitation_1 = ifelse((sl_rp003_11 %in% c(-2,-1)),sl_rp003_11,sl_rp003_11 - yrbirth),
	age_start_cohabitation_2 = ifelse((sl_rp003_12 %in% c(-2,-1)),sl_rp003_12, sl_rp003_12- yrbirth),
	age_start_cohabitation_3 = ifelse((sl_rp003_13 %in% c(-2,-1)),sl_rp003_13, sl_rp003_13- yrbirth),
	
	age_stop_cohabitation_1 = ifelse((sl_rp012_11 %in% c(-2,-1)),sl_rp012_11, sl_rp012_11 - yrbirth),
	age_stop_cohabitation_2 = ifelse((sl_rp012_12 %in% c(-2,-1)),sl_rp012_12, sl_rp012_12- yrbirth),
	age_stop_cohabitation_3 = ifelse((sl_rp012_13 %in% c(-2,-1)),sl_rp012_13, sl_rp012_13- yrbirth),

	
	#Non_Cohabitating#########################################################################################
	
	ever_noncohabitating = case_when(
		grepl("Yes",sl_rp016_,fixed =TRUE)~ 1,
		grepl("No",sl_rp016_,fixed =TRUE)~ 0,
		grepl("Refusal",sl_rp016_,fixed =TRUE)~ - 2,
		grepl("Don't know",sl_rp016_,fixed =TRUE)~ -1,
		TRUE ~ NA_real_),
	
	age_start_noncohabitation_1 = ifelse((sl_rp017_1 %in% c(-2,-1)),sl_rp017_1, sl_rp017_1- yrbirth),
	age_start_noncohabitation_2 = ifelse((sl_rp017_2 %in% c(-2,-1)),sl_rp017_2, sl_rp017_2- yrbirth),
	age_start_noncohabitation_3 = ifelse((sl_rp017_3 %in% c(-2,-1)),sl_rp017_3, sl_rp017_3- yrbirth),

	age_stop_noncohabitation_1 = ifelse((sl_rp020_1 %in% c(-2,-1)),sl_rp020_1, sl_rp020_1- yrbirth),
	age_stop_noncohabitation_2 = ifelse((sl_rp020_2 %in% c(-2,-1)),sl_rp020_2, sl_rp020_2- yrbirth),
	age_stop_noncohabitation_3 = ifelse((sl_rp020_3 %in% c(-2,-1)),sl_rp020_3, sl_rp020_3- yrbirth),
	
	#Divorced#########################################################################################
	
	ever_divorced_1 = case_when(
		grepl("Yes",sl_rp013_1,fixed =TRUE)~ as.numeric(1),
		grepl("No",sl_rp013_1,fixed =TRUE)~ as.numeric(0),
		grepl(0, ever_married)~ as.numeric(0),
		grepl("Refusal",sl_rp013_1,fixed =TRUE)~ as.numeric(-2),
		grepl("Don't know",sl_rp013_1,fixed =TRUE)~ as.numeric(-1),
		TRUE ~ NA_real_),
	
	ever_divorced_2 = case_when(
		grepl("Yes",sl_rp013_2,fixed =TRUE)~ as.numeric(1),
		grepl("No",sl_rp013_2,fixed =TRUE)~ as.numeric(0),
   	grepl("Refusal",sl_rp013_2,fixed =TRUE)~ as.numeric(-2),
	  grepl("Don't know",sl_rp013_2,fixed =TRUE)~ as.numeric(-1),
		grepl(0, ever_married)~ as.numeric(0),
		grepl(0, marry_2)~ as.numeric(0),
		grepl(1,still_with_partner_2) ~ as.numeric(0),
	  TRUE ~ NA_real_),
	
	ever_divorced_3 = case_when(	
		grepl("Yes",sl_rp013_3)~ as.numeric(1),
		grepl("No",sl_rp013_3)~ as.numeric(0),
		grepl("Refusal",sl_rp013_3,fixed =TRUE)~ as.numeric(-2),
		grepl("Don't know",sl_rp013_3,fixed =TRUE)~ as.numeric(-1),
		grepl(0, ever_married)~ as.numeric(0),
		grepl(0, marry_3_plus)~ as.numeric(0),
		grepl(1,still_with_partner_3) ~ as.numeric(0),
		TRUE ~ NA_real_),
	
	ever_divorced_4 = case_when(		
		grepl("Yes",sl_rp013_4)~ as.numeric(1),
		grepl("No",sl_rp013_4)~ as.numeric(0),
		grepl("Refusal",sl_rp013_4,fixed =TRUE)~ as.numeric(-2),
		grepl("Don't know",sl_rp013_4,fixed =TRUE)~ as.numeric(-1),
		grepl(0, ever_married)~ as.numeric(0),
		grepl(1, still_with_partner_4) ~ as.numeric(0),
		grepl(0, marry_4)~ as.numeric(0),
		TRUE ~ NA_real_),
	
	ever_divorce_2_plus = case_when(
	  (ever_divorced_2 == 1 ) ~ 1,
	  (ever_divorced_3 == 1 ) ~ 1,
	  (ever_divorced_4 == 1 ) ~ 1,
	  TRUE ~ 0),
	
	age_divorce_1 = ifelse(ever_divorced_1 %in% c(-1,-2)& (sl_rp014_1 %in% c(-2,-1)),sl_rp014_1,
	                       ifelse(ever_divorced_1 == 1 & (sl_rp014_1 %in% c(-2,-1)) ,sl_rp014_1,
	                              ifelse(ever_divorced_1 == 0 & (sl_rp014_1 %in% c(-2,-1)),
	                                     sl_rp014_1,sl_rp014_1 - yrbirth))),
	                                                                     
	age_divorce_2 = ifelse(ever_divorced_2 %in% c(-1,-2) & (sl_rp014_2 %in% c(-2,-1)), sl_rp014_2,
	                       ifelse(ever_divorced_2 == 1 & (sl_rp014_2 %in% c(-2,-1)), sl_rp014_2,
	                              ifelse(ever_divorced_2 == 0 & (sl_rp014_2 %in% c(-2,-1)),
	                                     sl_rp014_2, sl_rp014_2 - yrbirth))),
	                                 
	age_divorce_3 = ifelse(ever_divorced_3%in% c(-1,-2)& (sl_rp014_3 %in% c(-2,-1)),sl_rp014_3,
	                        ifelse(ever_divorced_3 == 1 & (sl_rp014_3 %in% c(-2,-1)), sl_rp014_3,
	                               ifelse(ever_divorced_3 == 0 & (sl_rp014_3 %in% c(-2,-1)),
	                                sl_rp014_3, sl_rp014_3 - yrbirth))),
	
	age_divorce_4 = ifelse(ever_divorced_4 %in% c(-1,-2)& (sl_rp014_4 %in% c(-2,-1)),sl_rp014_4,
	                        ifelse(ever_divorced_4 == 1 & (sl_rp014_4 %in% c(-2,-1)),sl_rp014_4,
	                               ifelse(ever_divorced_4 == 0 & (sl_rp014_4 %in% c(-2,-1)),
	                               sl_rp014_4, sl_rp014_4 - yrbirth))),  
	#Widowed
	ever_widowed_1 = case_when(
		grepl("Widowed/partner died",sl_rp010_1)~ as.numeric(1),
		grepl("Refusal",sl_rp010_2,fixed =TRUE)~ as.numeric(-2),
		grepl("Don't know",sl_rp010_2,fixed =TRUE)~ as.numeric(-1),
		grepl(0, ever_married)~ as.numeric(0),
		grepl(1, ever_divorced_1)~ as.numeric(0),
		TRUE ~ as.numeric(0)),
	
	ever_widowed_2 = case_when(
	  grepl("Widowed/partner died",sl_rp010_2)~ as.numeric(1),
	  grepl("Refusal",sl_rp010_2,fixed =TRUE)~ as.numeric(-2),
	  grepl("Don't know",sl_rp010_2,fixed =TRUE)~ as.numeric(-1),
	  grepl(0, ever_married) ~ as.numeric(0),
	  grepl(0, marry_2) ~ as.numeric(0),
	  grepl(1, ever_divorced_2)~ as.numeric(0),
	  TRUE ~ NA_real_)
	)

partner <- partner %>% 
  mutate_all(funs(str_detect(., "Widowed/partner died")))  %>% 
  mutate(partner, ever_widowed_3_plus = .) 

partner$n_pdie <- rowSums(partner$ever_widowed_3_plus, na.rm = T)

partner <- partner %>% 
  mutate(
  ever_widowed_3 = case_when(
    partner$n_pdie > 2 ~ as.numeric(1),
	  grepl("Widowed/partner died",sl_rp010_3)~ as.numeric(1),
	  grepl(0, ever_married) ~ as.numeric(0),
	  grepl("Refusal",sl_rp010_3,fixed =TRUE)~ as.numeric(-2),
	  grepl("Don't know",sl_rp010_3,fixed =TRUE)~ as.numeric(-1),
	  grepl(1, ever_divorced_3)~ as.numeric(0),
	  grepl(0, marry_3_plus) ~ as.numeric(0),
	  TRUE ~ NA_real_),
  ever_widowed_3_plus = ifelse(n_pdie > 2,1,0),
	
  age_widowed_1 = ifelse(ever_widowed_1 %in% c(-1,-2) & (sl_rp011_1 %in% c(-2,-1)), sl_rp011_1,
	                       ifelse(ever_widowed_1 == 1 & (sl_rp011_1 %in% c(-2,-1)), sl_rp011_1,
	                              ifelse(ever_widowed_1 == 0 & (sl_rp011_1 %in% c(-2,-1)),
	                                     sl_rp011_1, sl_rp011_1 - yrbirth))),
  
  age_widowed_2 = ifelse(ever_widowed_2 %in% c(-1,-2) & (sl_rp011_2 %in% c(-2,-1)), sl_rp011_2,
                         ifelse(ever_widowed_2 == 1 & (sl_rp011_2 %in% c(-2,-1)), sl_rp011_2,
                                ifelse(ever_widowed_2 == 0 & (sl_rp011_2 %in% c(-2,-1)),
                                       sl_rp011_2, sl_rp011_2 - yrbirth))),
  
  age_widowed_3 = ifelse(ever_widowed_3 %in% c(-1,-2) & (sl_rp011_3 %in% c(-2,-1)), sl_rp011_3,
                         ifelse(ever_widowed_3 == 1 & (sl_rp011_3 %in% c(-2,-1)), sl_rp011_3,
                                ifelse(ever_widowed_3 == 0 & (sl_rp011_3 %in% c(-2,-1)),
                                       sl_rp011_3, sl_rp011_3 - yrbirth)))
  )%>% 
  mutate(year_widowed_last =  coalesce(sl_rp011_11 ,
                                           sl_rp011_12 ,
                                           sl_rp011_13 ,
                                           sl_rp011_14 ,
                                           sl_rp011_15) 
    ) %>% 
   mutate(
	
  #Breakdown
	ever_breakdown = case_when(
		grepl("Relationship breakdown (including divorce)",sl_rp010_1,fixed =TRUE)~ as.numeric(1),
		grepl("Relationship breakdown (including divorce)",sl_rp010_2,fixed =TRUE)~ as.numeric(1),
		grepl("Relationship breakdown (including divorce)",sl_rp010_3,fixed =TRUE)~ as.numeric(1),
		grepl("Relationship breakdown (including divorce)",sl_rp010_4,fixed =TRUE)~ as.numeric(1),
		grepl("Relationship breakdown (including divorce)",sl_rp010_11,fixed =TRUE)~ as.numeric(1),
		grepl("Relationship breakdown (including divorce)",sl_rp010_12,fixed =TRUE)~ as.numeric(1),
		grepl("Relationship breakdown (including divorce)",sl_rp010_13,fixed =TRUE)~ as.numeric(1),
		grepl("Relationship breakdown (including divorce)",sl_rp010_14,fixed =TRUE)~ as.numeric(1),
		grepl("Relationship breakdown (including divorce)",sl_rp010_15,fixed =TRUE)~ as.numeric(1),
		grepl("Relationship breakdown (including divorce)",sl_rp010_16,fixed =TRUE)~ as.numeric(1),
		TRUE ~ as.numeric(0)),
	
		year_divorce_1 = as.numeric(!is.na(sl_rp014_1)),
		year_divorce_2 = as.numeric(!is.na(sl_rp014_2)),
		year_divorce_3 = as.numeric(!is.na(sl_rp014_3)),
		year_divorce_4 = as.numeric(!is.na(sl_rp014_4))
  )%>%
		mutate(
	    age_married_1 = ifelse(age_married_1 < -2  , NA_real_,age_married_1)
		# duration_marry_1 = ifelse(n_marriages == 1 & still_with_partner_1 == 1,
		# ifelse(sl_rp008_1 %in% c(-1,-2)|is.na(sl_rp008_1) & sl_rp004c_1 %in% c(-1,-2)|is.na(sl_rp004c_1), 
		# NA_real_,ifelse(sl_rp008_1 %in% c(-1,-2)|is.na(sl_rp008_1) & !is.na(sl_rp004c_1),
		# int_year - sl_rp004c_1, int_year - sl_rp008_1)), 
		# ifelse(year_died_1 == 1,ifelse(sl_rp008_1 %in% c(-1,-2)|is.na(sl_rp008_1),sl_rp008_1, sl_rp011_1 - sl_rp008_1),
		# ifelse(sl_rp009_1 == "No",ifelse(sl_rp008_1 %in% c(-1,-2)|is.na(sl_rp008_1),sl_rp008_1, sl_rp012_1 - sl_rp008_1),
		# ifelse(n_marriages > 1,ifelse(sl_rp008_1 %in% c(-1,-2)|is.na(sl_rp008_1) & !(sl_rp008_2 %in% c(-1,-2)),int_year - sl_rp008_2,sl_rp008_2 - sl_rp008_1), NA_real_))))
		)

partner$ever_divorced_1 <- ifelse((partner$still_with_partner_1 == 0 & partner$sl_rp010_1 == "Widowed/partner died"), 0, partner$ever_divorced_1)
partner$ever_divorced_2 <- ifelse((partner$still_with_partner_2 == 0 & partner$sl_rp010_2 =="Widowed/partner died"), 0, partner$ever_divorced_2)
partner$ever_divorced_3 <- ifelse((partner$still_with_partner_3 == 0 & partner$sl_rp010_3 == "Widowed/partner died"), 0, partner$ever_divorced_3)


partner$n_divorce <- rowSums(partner[,c("year_divorce_1",
	                                      "year_divorce_2",
										                    "year_divorce_3",
									                     	"year_divorce_4")])

partner$n_divorce <- ifelse(partner$ever_married == 0, 0, partner$n_divorce)
# partner$ever_divorced2 <- ifelse(partner$n_divorce > 0, 1, 0)

partner <- partner %>% 
dplyr::select(-c(year_divorce_1:year_divorce_4),-c(sl_rp002_:sl_rp021_5))

save(partner, file = "Dataset_constructed/partner.RData")

stargazer()
partner %>%
 dplyr::select(-c(year_died_1:year_died_9,year_divorce_1:year_divorce_4,age_first_widowed_1)) %>%
 tidyr::gather(Variable,Value,-c(mergeid,yrbirth,country,gender))%>%
 #mutate(Value= as.factor(Value))%>%
 #filter(!is.na(Value))%>%
 ggplot(.,aes((Value)))+ geom_bar()+facet_wrap(~Variable,scales="free")