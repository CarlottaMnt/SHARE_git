setwd("C:/Users/carlotta/Desktop/Dropbox/SHARE_new/R")
setwd("//crc/Team_work/Caralpvk/SHARE_new/R")

#PACKAGES

library(caret)
library(tidyverse)
library(dplyr)
library(fastDummies)
library(naniar)
library(DataCombine)
library(foreign)
######################################################################################################
##################################### df_child_new.RData #######################################################
######################################################################################################
load("Dataset_constructed/sharelife_new.RData")
gv_isco <- read.dta("Data/sharew1_rel7-1-0_gv_isco.dta")
load("Panel/demodf.RData")
demoSL <- demodf %>%
	filter(mergeid %in% sharelife_new$mergeid) %>%
	select(dn029isco_1, dn029isco_2, mergeid,wave)%>%
	filter(dn029isco_1 != "Not asked in this wave",dn029isco_2 != "Not_asked in this wave")
head(sharelife_new %>% filter(mergeid %in% demoSL$mergeid) %>% select(wave))
iscoSL <- gv_isco %>%
	filter(mergeid %in% sharelife_new$mergeid) %>%
	select(isco_fa, text_fa, isco_mo, text_mo,mergeid)
	filter(dn029isco_1 != "Not asked in this wave",dn029isco_2 != "Not_asked in this wave")

#Extract isco fist character to have ISCO category 1,
df <- iscoSL %>%
 dplyr::select(isco_fa, text_fa,isco_mo, text_mo,mergeid)%>%
 mutate(
	ISCO_fa = as.integer(substring(isco_fa, 1, 1)),
	ISCO_mo = as.integer(substring(isco_mo, 1, 1)))%>%
	dplyr::select(ISCO_fa,ISCO_mo, mergeid)
 
 #Works only done by female
 

#Create in Demo df the missing text category
demoSL <- demodf %>%
	filter(mergeid %in% sharelife_new$mergeid) %>%
	select(dn029isco_1, dn029isco_2, mergeid)%>%
	filter(dn029isco_1 != "Not asked in this wave",dn029isco_2 != "Not asked in this wave")%>%
	mutate(
	ISCO_fa = as.integer(substring(dn029isco_1, 1, 1)),
	ISCO_mo = as.integer(substring(dn029isco_2, 1, 1)))
dfSL <- rbind(demoSL,df)
#Which are the duplicated row?

xx <- dfSL$mergeid[which(duplicated(dfSL$mergeid))]
#Pick just one observation for each mergeid
dfSLunique <- dfSL %>%
   arrange(mergeid) %>% 
   distinct(mergeid, .keep_all = TRUE)

dfSL[which(dfSL$mergeid %in% xx),]

#Socio Economic status in childhood
ses <- sharelife_new %>% 
    dplyr::select(
		 mergeid,
		 sl_cs003_ , #NUMBER OF PEOPLE LIVING IN HOUSEHOLD WHEN TEN 
		 sl_cs002_, # ROOMS WHEN TEN YEARS OLD 
         sl_cs008_, # NUMBER OF BOOKS WHEN TEN 
		 sl_cs009_, # OCCUPATION OF MAIN BREADWINNER WHEN TEN 
		 sl_cs007d1,#Fixed bath 
		 sl_cs007d2, #Cold running water supply 
		 sl_cs007d3, #Hot running water supply 
		 sl_cs007d4, #Inside toilet 
		 sl_cs007d5, # Central heating  
		 sl_cs007dno, # None of these 
		 sl_cs004d1:sl_cs004d9,
		 sl_cs010_,  # MATH performance when ten
         sl_cs010a_, # Languange performace when ten
		 sl_ac002d1
        ) %>%
	mutate(
	math_level = case_when(
	grepl("Much worse",sl_cs010_, fixed = TRUE) ~ "Worse",
	grepl("Much better",sl_cs010_, fixed = TRUE) ~ "Better",
	TRUE ~ as.character(sl_cs010_)),
	language_level =  case_when(
	grepl("Much worse",sl_cs010a_, fixed = TRUE) ~ "Worse",
	grepl("Much better",sl_cs010a_, fixed = TRUE) ~ "Better",
	TRUE ~ as.character(sl_cs010a_))
    )%>%
	mutate(math_level_num = case_when(
	grepl("Not applicable: did not go to school",math_level)~ 0	,
	grepl("Worse",math_level)~ 1,
	grepl("About the same",math_level)~ 2	,
	grepl("Better",math_level)~ 3,
	grepl("Refusal",math_level)~ -2	,
	grepl("_Don't know",math_level)~ -1	,
	TRUE~ NA_real_),
	language_level_num = case_when(
	grepl("Worse",language_level)~ 1,
	grepl("About the same",language_level)~ 2,
	grepl("Better",language_level)~ 3,
	grepl("Refusal",language_level)~ -2	,
	grepl("_Don't know",language_level)~ -1	,
	TRUE~ NA_real_))
ses <- ses %>%
 mutate(
	live_childrenhome = case_when(
		grepl("Selected",sl_ac002d1)~ as.numeric(1),
		grepl("Not selected",sl_ac002d1)~ as.numeric(0),
		grepl("Refusal", sl_ac002d1) ~ as.numeric(-2),
		grepl("Don't know", sl_ac002d1) ~ as.numeric(-1),
		TRUE ~ NA_real_),
    h_water = case_when(
		grepl("Not selected", sl_cs007d3) ~ as.numeric(0),
		grepl("Selected", sl_cs007d3) ~ as.numeric(1),
		grepl("Refusal", sl_cs007d3) ~ as.numeric(-2),
		grepl("Don't know", sl_cs007d3) ~ as.numeric(-1),
		TRUE ~ NA_real_),
	c_water = case_when(
		grepl("Not selected", sl_cs007d2) ~ as.numeric(0),
		grepl("Selected", sl_cs007d2) ~ as.numeric(1),
		grepl("Refusal", sl_cs007d2) ~ as.numeric(-2),
		grepl("Don't know", sl_cs007d2) ~ as.numeric(-1),
		TRUE ~ NA_real_), 
	bath = case_when(
		grepl("Not selected", sl_cs007d1) ~ as.numeric(0),
		grepl("Selected", sl_cs007d1) ~ as.numeric(1),
		grepl("Refusal", sl_cs007d1) ~ as.numeric(-2),
		grepl("Don't know", sl_cs007d1) ~ as.numeric(-1),
		TRUE ~ NA_real_),
	in_toilet = case_when(
		grepl("Not selected", sl_cs007d4) ~ as.numeric(0),
		grepl("Selected", sl_cs007d4) ~ as.numeric(1),
		grepl("Refusal", sl_cs007d4) ~ as.numeric(-2),
		grepl("Don't know", sl_cs007d4) ~ as.numeric(-1),
		TRUE ~ NA_real_),
	heating = case_when(
		grepl("Not selected", sl_cs007d5) ~ as.numeric(0),
		grepl("Selected", sl_cs007d5) ~ as.numeric(1),
		grepl("Refusal",  sl_cs007d5) ~ as.numeric(-2),
		grepl("Don't know",  sl_cs007d5) ~ as.numeric(-1),
		TRUE ~ NA_real_),
	bio_mother = case_when(
		grepl("Not selected", sl_cs004d1) ~ as.numeric(0),
		grepl("Selected", sl_cs004d1) ~ as.numeric(1),
		grepl("Refusal", sl_cs004d1) ~ as.numeric(-2),
		grepl("Don't know", sl_cs004d1) ~ as.numeric(-1),
		TRUE ~ NA_real_),
	bio_father = case_when(
		grepl("Not selected", sl_cs004d2) ~ as.numeric(0),
		grepl("Selected", sl_cs004d2) ~ as.numeric(1),
		grepl("Refusal", sl_cs004d2) ~ as.numeric(-2),
		grepl("Don't know", sl_cs004d2) ~ as.numeric(-1),
		TRUE ~ NA_real_),
	nonbio_mother = case_when(
		grepl("Not selected", sl_cs004d3) ~ as.numeric(0),
		grepl("Selected", sl_cs004d3) ~ as.numeric(1),
		grepl("Refusal", sl_cs004d3) ~ as.numeric(-2),
		grepl("Don't know", sl_cs004d3) ~ as.numeric(-1),
		TRUE ~ NA_real_), 
	nonbio_father = case_when(
		grepl("Not selected",  sl_cs004d4) ~ as.numeric(0),
		grepl("Selected", sl_cs004d4) ~ as.numeric(1),
		grepl("Refusal",  sl_cs004d4) ~ as.numeric(-2),
		grepl("Don't know",  sl_cs004d4) ~ as.numeric(-1),
		TRUE ~ NA_real_), 
	bio_brother = case_when(
		grepl("Not selected", sl_cs004d5) ~ as.numeric(0),
		grepl("Selected", sl_cs004d5) ~ as.numeric(1),
		grepl("Refusal", sl_cs004d5) ~ as.numeric(-2),
		grepl("Don't know",sl_cs004d5) ~ as.numeric(-1),
		TRUE ~ NA_real_), 
	nonbio_brother = case_when(
		grepl("Not selected", sl_cs004d6) ~ as.numeric(0),
		grepl("Selected", sl_cs004d6) ~ as.numeric(1),
		grepl("Refusal",sl_cs004d6) ~ as.numeric(-2),
		grepl("Don't know", sl_cs004d6) ~ as.numeric(-1),
		TRUE ~ NA_real_),
	grandparents = case_when(
		grepl("Not selected", sl_cs004d7) ~ as.numeric(0),
		grepl("Selected",sl_cs004d7) ~ as.numeric(1),
		grepl("Refusal",sl_cs004d7) ~ as.numeric(-2),
		grepl("Don't know",sl_cs004d7) ~ as.numeric(-1),
		TRUE ~ NA_real_),
	other_bio = case_when(
		grepl("Not selected", sl_cs004d8) ~ as.numeric(0),
		grepl("Selected",sl_cs004d8) ~ as.numeric(1),
		grepl("Refusal", sl_cs004d8) ~ as.numeric(-2),
		grepl("Don't know", sl_cs004d8) ~ as.numeric(-1),
		TRUE ~ NA_real_),
	other_nonbio = case_when(
		grepl("Not selected", sl_cs004d9) ~ as.numeric(0),
		grepl("Selected", sl_cs004d9) ~ as.numeric(1),
		grepl("Refusal",  sl_cs004d9) ~ as.numeric(-2),
		grepl("Don't know",  sl_cs004d9) ~ as.numeric(-1),
		TRUE ~ NA_real_),
	sl_cs003_= ifelse(sl_cs003_ == 0,1,sl_cs003_),
	roomperson = ifelse((sl_cs002_ < 0 | sl_cs003_ < 0) & (sl_cs002_ == sl_cs003_), sl_cs002_,
	ifelse((sl_cs002_ < 0 | sl_cs003_ < 0) & (sl_cs002_ != sl_cs003_),sl_cs003_,round(sl_cs002_/sl_cs003_,2))))

n_fac = apply(ses[,c("h_water","c_water","bath","in_toilet","heating")],1, function(x) sum(x[x > 0], na.rm = TRUE))

ses <- ses %>%
 mutate(n_fac = n_fac)

#ISCO categories of occupation
ses <- ses %>% full_join(dfSLunique) 



ses <- ses  %>%
 mutate(ISCO_par = case_when(
grepl("Skilled agricultural or fishery worker", sl_cs009_) ~ as.numeric(2),
grepl("Craft or related trades worker", sl_cs009_) ~ as.numeric(2),
grepl("Plant/machine operator or assembler", sl_cs009_) ~ as.numeric(2),
grepl("Legislator, senior official or manager", sl_cs009_) ~ as.numeric(4),
grepl("Technician or associate professional", sl_cs009_) ~ as.numeric(3),
grepl("Service, shop or market sales worker", sl_cs009_) ~ as.numeric(2),
grepl("Clerk", sl_cs009_) ~ as.numeric(3),
grepl("Professional", sl_cs009_) ~ as.numeric(4),
grepl("Armed forces",sl_cs009_) ~ as.numeric(0),
grepl("Elementary occupation", sl_cs009_) ~ as.numeric(1),
grepl("Refusal", sl_cs009_) ~ as.numeric(-2),
grepl("Don't know", sl_cs009_) ~ as.numeric(-1),
TRUE ~ NA_real_))
#Apply a function for replacing when possible missing value with data from other sources
 myfunction <- function(df){
					ifelse(!(is.na(df[3])),df[3],ifelse(!(is.na(df[2])),df[2],ifelse(!(is.na(df[1])), df[1], NA_real_)))
				    }

ses$ISCO_par <- apply(as.matrix(rev(ses)[c(1:3)]),1, function(x) myfunction(x))



#Categories of number of book
ses <- ses %>%
	mutate(sl_cs008_ = as.character(sl_cs008_)) %>% 
	mutate(nr_book = case_when(
	grepl("None or very few (0-10 books)", sl_cs008_, fixed =TRUE) ~ 1,
	grepl("Enough to fill one shelf (11-25 books)", sl_cs008_,fixed =TRUE) ~ 2,
	grepl("Enough to fill one bookcase (26-100 books)", sl_cs008_,fixed =TRUE) ~ 3,
	grepl("Enough to fill two or more bookcases (more than 200 books)", sl_cs008_,fixed =TRUE) ~ 4,
	grepl("Enough to fill two bookcases (101-200 books)", sl_cs008_,fixed =TRUE) ~ 5,
	grepl("Refusal", sl_cs008_,fixed =TRUE) ~ -2,
	grepl("Don't know", sl_cs008_,fixed =TRUE) ~ -1,
	TRUE ~ NA_real_)) %>%
	mutate(nr_book = as.factor(nr_book))


ses <- ses %>%
dplyr::select(-c(
		sl_cs008_, # NUMBER OF BOOKS WHEN TEN 
		 sl_cs009_, # OCCUPATION OF MAIN BREADWINNER WHEN TEN 
		 sl_cs007d1:sl_cs007dno,
		 sl_cs002_,
		 sl_cs003_,
		 sl_cs004d1:sl_cs004d9,
		 h_water,
		 c_water,
		 sl_cs010_,
		 sl_cs010a_,
		 bath,
		 in_toilet,
		 heating,
		 sl_ac002d1))


#CHILDHOOD HEALTH
health <- sharelife_new %>% 
  dplyr::select(
        mergeid,
		sl_hs003_, #  CHILDHOOD HEALTH STATUS  
        sl_hs004_, # CHILDHOOD HEALTH MISSED SCHOOL FOR 1 MONTH+  
		sl_hs006_, # CHILDHOOD HEALTH: IN HOSPITAL FOR 1 MONTH+ 
		sl_hs008d1:sl_hs008dno,#CHILDHOOD ILLNESSES 1 
		sl_hs009d1:sl_hs009dot, # CHILDHOOD ILLNESSES 2 
		# sl_hs045d1:sl_hs045dno, # DID PARENTS SMOKE DURING CHILDHOOD 
		sl_hc002_, # VACCINATIONS DURING CHILDHOOD 
		# sl_hc005_, # USUAL SOURCE OF CARE
		sl_hc015_, # EVER REGUALR DENTIST
		# sl_hc052_, # REGULAR BLOOD TEST
	    sl_hc040_ # Regular blood pressure checks             
        # sl_hc041_, # In which year did you start
        # sl_hc042_, # continuity blood pressure
        # sl_hc042ad1:sl_hc042ad7,#When no blood pressure
        # sl_hc050d1:sl_hc050dot #Reason for no regular_bloodpressure, 		
        )  
#Objective health measures
health <- health %>%
	mutate(
	Infect_dis = case_when(
		grepl("Not selected", sl_hs008d1) ~ as.numeric(0),
		grepl("Selected",sl_hs008d1) ~ as.numeric(1),
		grepl("Refusal", sl_hs008d1) ~ as.numeric(-2),
		grepl("Don't know",sl_hs008d1) ~ as.numeric(-1),
		TRUE ~ NA_real_),
		
	polio = case_when(
		grepl("Not selected", sl_hs008d2) ~ as.numeric(0),
		grepl("Selected", sl_hs008d2) ~ as.numeric(1),
		grepl("Refusal", sl_hs008d2) ~ as.numeric(-2),
		grepl("Don't know",sl_hs008d2) ~ as.numeric(-1),
		TRUE ~ NA_real_), 
		
	asthma = case_when(
		grepl("Not selected", sl_hs008d3) ~ as.numeric(0),
		grepl("Selected", sl_hs008d3) ~ as.numeric(1),
		grepl("Refusal", sl_hs008d3) ~ as.numeric(-2),
		grepl("Don't know",sl_hs008d3) ~ as.numeric(-1),
		TRUE ~ NA_real_),
		
	respir_probl = case_when(
		grepl("Not selected", sl_hs008d4) ~ as.numeric(0),
		grepl("Selected", sl_hs008d4) ~ as.numeric(1),
		grepl("Refusal", sl_hs008d4) ~ as.numeric(-2),
		grepl("Don't know",sl_hs008d4) ~ as.numeric(-1),
		TRUE ~ NA_real_), 
		
	allergies = case_when(
		grepl("Not selected", sl_hs008d5) ~ as.numeric(0),
		grepl("Selected", sl_hs008d5) ~ as.numeric(1),
		grepl("Refusal", sl_hs008d5) ~ as.numeric(-2),
		grepl("Don't know",sl_hs008d5) ~ as.numeric(-1),
		TRUE ~ NA_real_),
		
	diarrhoea = case_when(
		grepl("Not selected", sl_hs008d6) ~ as.numeric(0),
		grepl("Selected", sl_hs008d6) ~ as.numeric(1),
		grepl("Refusal", sl_hs008d6) ~ as.numeric(-2),
		grepl("Don't know",sl_hs008d6) ~ as.numeric(-1),
		TRUE ~ NA_real_),
		
	meningitis = case_when(
		grepl("Not selected", sl_hs008d7) ~ as.numeric(0),
		grepl("Selected", sl_hs008d7) ~ as.numeric(1),
		grepl("Refusal", sl_hs008d7) ~ as.numeric(-2),
		grepl("Don't know",sl_hs008d7) ~ as.numeric(-1),
		TRUE ~ NA_real_),
		
	ear_prob = case_when(
		grepl("Not selected", sl_hs008d8) ~ as.numeric(0),
		grepl("Selected", sl_hs008d8) ~ as.numeric(1),
		grepl("Refusal", sl_hs008d8) ~ as.numeric(-2),
		grepl("Don't know",sl_hs008d8) ~ as.numeric(-1),
		TRUE ~ NA_real_),
		
	speech = case_when(
		grepl("Not selected", sl_hs008d9) ~ as.numeric(0),
		grepl("Selected", sl_hs008d9) ~ as.numeric(1),
		grepl("Refusal", sl_hs008d9) ~ as.numeric(-2),
		grepl("Don't know",sl_hs008d9) ~ as.numeric(-1),
		TRUE ~ NA_real_),
		
	see = case_when(
		grepl("Not selected", sl_hs008d10) ~ as.numeric(0),
		grepl("Selected", sl_hs008d10) ~ as.numeric(1),
		grepl("Refusal", sl_hs008d10) ~ as.numeric(-2),
		grepl("Don't know",sl_hs008d10) ~ as.numeric(-1),
		TRUE ~ NA_real_),
		
	no_ch_disease = case_when(
		grepl("Not selected", sl_hs008dno) ~ as.numeric(0),
		grepl("Selected", sl_hs008dno) ~ as.numeric(1),
		grepl("Refusal", sl_hs008dno) ~ as.numeric(-2),
		grepl("Don't know",sl_hs008dno) ~ as.numeric(-1),
		TRUE ~ NA_real_),
		
	headache = case_when(
		grepl("Not selected", sl_hs009d1) ~ as.numeric(0),
		grepl("Selected", sl_hs009d1) ~ as.numeric(1),
		grepl("Refusal", sl_hs009d1) ~ as.numeric(-2),
		grepl("Don't know",sl_hs009d1) ~ as.numeric(-1),
		TRUE ~ NA_real_), 
		
	epilepsy = case_when(
		grepl("Not selected", sl_hs009d2) ~ as.numeric(0),
		grepl("Selected", sl_hs009d2) ~ as.numeric(1),
		grepl("Refusal", sl_hs009d2) ~ as.numeric(-2),
		grepl("Don't know",sl_hs009d2) ~ as.numeric(-1),
		TRUE ~ NA_real_),
		
	psychiatric = case_when(
		grepl("Not selected", sl_hs009d3) ~ as.numeric(0),
		grepl("Selected", sl_hs009d3) ~ as.numeric(1),
		grepl("Refusal", sl_hs009d3) ~ as.numeric(-2),
		grepl("Don't know",sl_hs009d3) ~ as.numeric(-1),
		TRUE ~ NA_real_),
		
	bones = case_when(
		grepl("Not selected", sl_hs009d4) ~ as.numeric(0),
		grepl("Selected", sl_hs009d4) ~ as.numeric(1),
		grepl("Refusal", sl_hs009d4) ~ as.numeric(-2),
		grepl("Don't know",sl_hs009d4) ~ as.numeric(-1),
		TRUE ~ NA_real_),
		
	appendix = case_when(
		grepl("Not selected", sl_hs009d5) ~ as.numeric(0),
		grepl("Selected", sl_hs009d5) ~ as.numeric(1),
		grepl("Refusal", sl_hs009d5) ~ as.numeric(-2),
		grepl("Don't know",sl_hs009d5) ~ as.numeric(-1),
		TRUE ~ NA_real_),
		
	diabet = case_when(
		grepl("Not selected", sl_hs009d6) ~ as.numeric(0),
		grepl("Selected", sl_hs009d6) ~ as.numeric(1),
		grepl("Refusal", sl_hs009d6) ~ as.numeric(-2),
		grepl("Don't know",sl_hs009d6) ~ as.numeric(-1),
		TRUE ~ NA_real_),
		
	heart = case_when(
		grepl("Not selected", sl_hs009d7) ~ as.numeric(0),
		grepl("Selected", sl_hs009d7) ~ as.numeric(1),
		grepl("Refusal", sl_hs009d7) ~ as.numeric(-2),
		grepl("Don't know",sl_hs009d7) ~ as.numeric(-1),
		TRUE ~ NA_real_),
		
	leukaemia = case_when(
		grepl("Not selected", sl_hs009d8) ~ as.numeric(0),
		grepl("Selected", sl_hs009d8) ~ as.numeric(1),
		grepl("Refusal", sl_hs009d8) ~ as.numeric(-2),
		grepl("Don't know",sl_hs009d8) ~ as.numeric(-1),
		TRUE ~ NA_real_),
		
	cancer = case_when(
		grepl("Not selected", sl_hs009d9) ~ as.numeric(0),
		grepl("Selected", sl_hs009d9) ~ as.numeric(1),
		grepl("Refusal", sl_hs009d9) ~ as.numeric(-2),
		grepl("Don't know",sl_hs009d9) ~ as.numeric(-1),
		TRUE ~ NA_real_),
		
	no_ch_disease = case_when(
		grepl("Not selected", sl_hs009dno) ~ as.numeric(0),
		grepl("Selected", sl_hs009dno) ~ as.numeric(1),
		grepl("Refusal", sl_hs009dno) ~ as.numeric(-2),
		grepl("Don't know",sl_hs009dno) ~ as.numeric(-1),
		TRUE ~ NA_real_),
		
	other_disease = case_when(
		grepl("Not selected", sl_hs009dot) ~ as.numeric(0),
		grepl("Selected", sl_hs009dot) ~ as.numeric(1),
		grepl("Refusal", sl_hs009dot) ~ as.numeric(-2),
		grepl("Don't know",sl_hs009dot) ~ as.numeric(-1),
		TRUE ~ NA_real_)
		)

health <- health %>%
dplyr:: select(-c(sl_hs008d1:sl_hs009dot))%>%
	mutate(
	n_resp = apply(health[,c("asthma","respir_probl","allergies")],1, function(x) sum(x[x > 0], na.rm = TRUE)), #asthma, and other respiratory problems and allergies. 
	n_inf  = apply(health[,c("Infect_dis","polio","diarrhoea","meningitis","appendix")],1, function(x) sum(x[x > 0], na.rm = TRUE)), #Infectious diseases include  polio, severe diarrhea, meningitis/encephalitis, and appendicitis
	n_cardio= apply(health[,c("diabet","heart")],1, function(x) sum(x[x > 0], na.rm = TRUE)), #diabetes or high blood sugar, and heart trouble
	n_neuro= apply(health[,c("headache","epilepsy","psychiatric")],1, function(x) sum(x[x > 0], na.rm = TRUE)), #severe headaches or migraines, and epilepsy, fits or seizures, and emotional, nervous or psychiatric problems. 
	n_organ= apply(health[,c("ear_prob","speech","see")],1, function(x) sum(x[x > 0], na.rm = TRUE)), #chronic ear problems, speech impairment, and difficulty in seeing even with eyeglasses. 
	n_neopla= apply(health[,c("leukaemia","cancer","other_disease")],1, function(x) sum(x[x > 0], na.rm = TRUE))
	) #serious health conditions are combined with neoplastic diseases.
 
#Ever missed school and hospitalization
health <- health %>%
	mutate(sl_hs004_= as.character(sl_hs004_)) %>% 
	mutate(
	ever_missed = case_when(
		grepl("No", sl_hs004_, fixed = TRUE) ~ 0,
		grepl("Yes", sl_hs004_,fixed = TRUE) ~ 1,
		grepl("Refusal", sl_hs004_, fixed = TRUE) ~ -2,
		grepl("Don't know", sl_hs004_,fixed = TRUE) ~ -1,
		TRUE ~ NA_real_), 
	ever_hospital = case_when(
		grepl("No", sl_hs006_,fixed = TRUE) ~ 0,
		grepl("Yes", sl_hs006_,fixed = TRUE) ~ 1,
		grepl("Refusal", sl_hs006_, fixed = TRUE) ~ -2,
		grepl("Don't know", sl_hs006_,fixed = TRUE) ~ -1,
		TRUE ~ NA_real_),
	ever_vaccinations = case_when(
		grepl("No", sl_hc002_,fixed = TRUE) ~ 0,
		grepl("Yes", sl_hc002_,fixed = TRUE) ~ 1,
		grepl("Refusal",sl_hc002_, fixed = TRUE) ~ -2,
		grepl("Don't know", sl_hc002_,fixed = TRUE) ~ -1,
		TRUE ~ NA_real_),
	regular_dentist = case_when(
		grepl("No", sl_hc015_,fixed = TRUE) ~ 0,
		grepl("Yes", sl_hc015_,fixed = TRUE) ~ 1,
		grepl("Refusal", sl_hc015_, fixed = TRUE) ~ -2,
		grepl("Don't know", sl_hc015_,fixed = TRUE) ~ -1,
		TRUE ~ NA_real_),
	ever_bloodpressure = case_when(
		grepl("No", sl_hc040_,fixed = TRUE) ~ 0,
		grepl("Yes", sl_hc040_,fixed = TRUE) ~ 1,
		grepl("Refusal", sl_hc040_, fixed = TRUE) ~ -2,
		grepl("Don't know", sl_hc040_,fixed = TRUE) ~ -1,
		TRUE ~ NA_real_),
	# continuity_bloodpressure = case_when(
		# grepl("No", sl_hc042_,fixed = TRUE) ~ 0,
		# grepl("Yes", sl_hc042_,fixed = TRUE) ~ 1,
		# grepl("Refusal", sl_hc042_, fixed = TRUE) ~ -2,
		# grepl("Don't know", sl_hc042_,fixed = TRUE) ~ -1,
		# TRUE ~ NA_real_),
	# when_no_bloodpressure = case_when(
		# grepl("Selected", sl_hc042ad1,fixed = TRUE) ~ "0-15" ,
		# grepl("Selected", sl_hc042ad2,fixed = TRUE) ~ "16-25",
		# grepl("Selected", sl_hc042ad3,fixed = TRUE) ~ "26-40",
		# grepl("Selected", sl_hc042ad4,fixed = TRUE) ~ "41-55",
		# grepl("Selected", sl_hc042ad5,fixed = TRUE) ~ "56-65",
		# grepl("Selected", sl_hc042ad6,fixed = TRUE) ~ "66-75",
		# grepl("Selected", sl_hc042ad7,fixed = TRUE) ~ "+75",
		# TRUE ~ NA_character_),
	# reason_no_bloodpressure = case_when(
		# grepl("Selected", sl_hc050d1,fixed = TRUE) ~ "Not_affordable" ,
		# grepl("Selected", sl_hc050d2,fixed = TRUE) ~ "Not_covered",
		# grepl("Selected", sl_hc050d3,fixed = TRUE) ~ "No_insurance ",
		# grepl("Selected", sl_hc050d4,fixed = TRUE) ~ "Time_constraints ",
		# grepl("Selected", sl_hc050d5,fixed = TRUE) ~ "Lack_info",
		# grepl("Selected", sl_hc050d6,fixed = TRUE) ~ "Not_usual_get",
		# grepl("Selected", sl_hc050d7,fixed = TRUE) ~ "No_place",
		# grepl("Selected", sl_hc050d8,fixed = TRUE) ~ "Not_necessary",
		# grepl("Selected", sl_hc050dot,fixed = TRUE) ~ "Other",
		# TRUE ~ NA_character_)
	#age_start_bloop = ifelse((year_start_bloodp == -1 |year_start_bloodp == -2), year_start_bloodp, year_start_bloodp - yrbirth)
	) %>%
	mutate_if(is.character,as.factor)

#Subjective childhood health
health <- health %>%
	mutate(
	shr = case_when(
		grepl("Excellent", sl_hs003_, fixed = TRUE) ~ 4,
		grepl("Very good", sl_hs003_,fixed = TRUE) ~ 3,
		grepl("Good", sl_hs003_,fixed = TRUE) ~ 2,
		grepl("Fair", sl_hs003_,fixed = TRUE) ~ 1,
		grepl("Poor", sl_hs003_,fixed = TRUE) ~ 1,
		grepl("Health varied a great deal (spontaneous)", sl_hs003_,fixed = TRUE) ~ 1,
		grepl("Don't know", sl_hs003_,fixed = TRUE) ~ -1,
		grepl("Refusal", sl_hs003_,fixed = TRUE) ~ -2,
		TRUE ~ NA_real_)
		)%>%
	mutate(shr = as.factor(shr))

#parents attitude: Not include since not present in sharelifeW7
health <- health %>%
 mutate(par_smoke = case_when(
grepl("Not selected", sl_hs045d1, fixed =TRUE) ~ 0,
grepl("Selected",sl_hs045d1,fixed =TRUE) ~ 1,
TRUE ~ NA_real_), par_drink = case_when(
grepl("Not selected", sl_hs045d2,fixed =TRUE) ~ 0,
grepl("Selected", sl_hs045d2,fixed =TRUE) ~ 1,
TRUE ~ NA_real_),
par_mental = case_when(
grepl("Not selected", sl_hs045d3,fixed =TRUE) ~ 0,
grepl("Selected", sl_hs045d3,fixed =TRUE) ~ 1,
TRUE ~ NA_real_),
none_parent = case_when(
grepl("Not selected", sl_hs045dno,fixed =TRUE) ~ 0,
grepl("Selected", sl_hs045dno,fixed =TRUE) ~ 1,
TRUE ~ NA_real_))

health <- health %>%
 dplyr::select(-c(
        sl_hs003_, #  CHILDHOOD HEALTH STATUS  
        sl_hs004_, # CHILDHOOD HEALTH MISSED SCHOOL FOR 1 MONTH+  
		sl_hs006_, # CHILDHOOD HEALTH: IN HOSPITAL FOR 1 MONTH+ 
		sl_hc002_, # VACCINATIONS DURING CHILDHOOD 
		# sl_hc005_, # USUAL SOURCE OF CARE
		sl_hc015_, # EVER REGUALR DENTIST
		# sl_hc052_, # REGULAR BLOOD TEST
	    sl_hc040_, # Regular blood pressure checks             
	    # sl_hc076_, # Regular vision test  
        sl_hc040_, # Regular blood pressure checks 
        # sl_hc041_, # In which year did you start
        # sl_hc042_, # continuity blood pressure
        # sl_hc042ad1:sl_hc042ad7,#When no blood pressure
        # sl_hc050d1:sl_hc050dot, #Reason for no regular_bloodpressure, 	
		epilepsy,
		diabet,
		heart,
		leukaemia,
		cancer,
		psychiatric,
		allergies,
		speech,
		see,
		polio,
		ear_prob,
		headache,
		no_ch_disease,
		diarrhoea,
		meningitis,
		asthma
		))	

summary(health)
stargazer(health)

#MERGING HEALTH AND SOCIO ECONOMIC STATUS

df_child <- merge(ses, health, by =c("mergeid"), all = TRUE)
summary(df_child)
kable(psych::describeBy(df_child, df_child$country), format = "latex")

save(df_child,file="Dataset_constructed/df_child_new.RData")

df_child %>%
 tidyr::gather(Variable,Value,-c(mergeid,age,country,gender))%>%
 mutate_if(is.character, as.factor)%>%
 filter(!is.na(Value)) %>%
 ggplot(.,aes((Value)))+ geom_bar()+facet_wrap(~Variable,scales="free")+ ggtitle("Childhood Predictors")
 