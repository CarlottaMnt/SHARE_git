setwd("C:/Users/carlotta/Desktop/Dropbox/SHARE_new/R")
#setwd("//crc/Team_work/Caralpvk/SHARE_new/R")

#PACKAGES

library(caret)
library(tidyverse)
library(dplyr)
library(fastDummies)
library(naniar)
######################################################################################################
##################################### children.RData #######################################################
######################################################################################################
rm(list = ls())
load("Dataset_constructed/sharelife_new.RData")
load("MyData_file/covariate_imputed_june.RData")

load("panel/chdf.RData")

load("panel/CVdf.RData")

#Accomodation_financial
#frequency tables
#Children panel/children

chSL_panel <- chdf %>%
	filter(
		mergeid %in% sharelife_new$mergeid,
	)

CVSL_panel <- CVdf %>%
  filter(
    mergeid %in% sharelife_new$mergeid,
  )

chcv <- chSL_panel %>% left_join(CVSL_panel[,c("mergeid","fam_resp","relrpers","mergeidp","hhid","country","language","wave")]) %>% select(mergeid, fam_resp, wave, everything())
#The family respondent responde on behalf of the household in the children section

#Which Sharelife respondents have not imputed values in lifesat_panel?
summary(sharelife_new[which(!(sharelife_new$mergeid %in% chSL_panel$mergeid)),]$firstwave)

#Cross-sectional SL dataset
#Here we select the respondent responses closer to the sharelife questionnaire and we merge the data set with the SL cross sectional weights

chSL <- chcv %>%
 dplyr::group_by(mergeid) %>%
 dplyr::slice(n()) %>% #select the last observation available and wih less uncertainy
 ungroup() %>%
 merge(., sharelife_new[,c("mergeid","cciw")],by = "mergeid", all = FALSE) %>%
 dplyr::select(mergeid, cciw, fam_resp, everything()) %>%
 dplyr::select(-wave)

Never_endNa <-  function(x) replace(x, x %in% c(9997,9996,9777) , NA)
#Children history

children <- sharelife_new %>%
		left_join(chSL[,!(names(chSL) %in% c("country", "mergeidp", "coupleid", "language"))])%>%
		dplyr::select(
		mergeid,
		gender,
		country,
		fam_resp,
		relrpers,
		yrbirth,
		hhid,
		sl_rc022_, # EVER HAD OTHER NON_MENTIONED CHILDREN ,
		sl_rc023_, #NUMBER OF OTHER CHILDREN 
		sl_rc038_, #OTHER ADOPTED CHILDREN 
		sl_rc039_, # NUMBER OF OTHER ADOPTED
		sl_rc026_1:sl_rc026_15, #GENDER 
		sl_rc024_1:sl_rc024_15, #YEAR OF BIRTH 
		sl_rc027_1: sl_rc027_15,  #OTHER CHILD STILL ALIVE 
		sl_rc028_1:sl_rc028_4,  #WHEN CHILDREN DIE
		sl_rc031d1_1:sl_rc031dot_4,
		sl_rc032_1:sl_rc032_4,#maternal benefit
		ch007_1:ch007_4, #where children live,
		ch011_1:ch011_4, #Own child,
		ch012_1:ch012_4, #marital status,
		ch014_1:ch014_4, #contact with children
		ch015_1:ch015_4, #when children move out from home
		ch016_1:ch016_4 #children occupation
	   ) %>%
	   select(
    where(
      ~!all(is.na(.x))
		)
	) %>% 
  mutate(
    sl_rc024_4 = ifelse(sl_rc024_4 < sl_rc024_3, sl_rc024_3, sl_rc024_4),
    sl_rc024_3 = ifelse(sl_rc024_4 < sl_rc024_3, sl_rc024_4, sl_rc024_3),
    sl_rc024_2 = ifelse(sl_rc024_3 < sl_rc024_2, sl_rc024_3, sl_rc024_2),
    sl_rc024_1 = ifelse(sl_rc024_2 < sl_rc024_1, sl_rc024_2, sl_rc024_1)
  )%>% 
  droplevels()


setDT(children)[,ch007_1:=ch007_1[!is.na(ch007_1)][1L] , by = hhid]
setDT(children)[, ch007_2:=ch007_2[!is.na(ch007_2)][1L] , by = hhid]
setDT(children)[,ch007_3:=ch007_3[!is.na(ch007_3)][1L] , by = hhid]
setDT(children)[, ch007_4:=ch007_4[!is.na(ch007_4)][1L] , by = hhid]

setDT(children)[, ch012_1:=ch012_1[!is.na(ch012_1)][1L] , by = hhid]
setDT(children)[, ch012_2:=ch012_2[!is.na(ch012_2)][1L] , by = hhid]
setDT(children)[, ch012_3:=ch012_3[!is.na(ch012_3)][1L] , by = hhid]
setDT(children)[, ch012_4:=ch012_4[!is.na(ch012_4)][1L] , by = hhid]

setDT(children)[, ch014_1:=ch014_1[!is.na(ch014_1)][1L] , by = hhid]
setDT(children)[, ch014_2:=ch014_2[!is.na(ch014_2)][1L] , by = hhid]
setDT(children)[, ch014_3:=ch014_3[!is.na(ch014_3)][1L] , by = hhid]
setDT(children)[, ch014_4:=ch014_4[!is.na(ch014_4)][1L] , by = hhid]

setDT(children)[, ch016_1:= ch016_1[!is.na(ch016_1)][1L] , by = hhid]
setDT(children)[, ch016_2:= ch016_2[!is.na(ch016_2)][1L] , by = hhid]
setDT(children)[, ch016_3:= ch016_3[!is.na(ch016_3)][1L] , by = hhid]
setDT(children)[, ch016_4:= ch016_4[!is.na(ch016_4)][1L] , by = hhid]

setDT(children)[, sl_rc027_1:=sl_rc027_1[!is.na(sl_rc027_1)][1L] , by = hhid]
setDT(children)[, sl_rc027_2:=sl_rc027_2[!is.na(sl_rc027_2)][1L] , by = hhid]
setDT(children)[, sl_rc027_3:=sl_rc027_3[!is.na(sl_rc027_3)][1L] , by = hhid]
setDT(children)[, sl_rc027_4:=sl_rc027_4[!is.na(sl_rc027_4)][1L] , by = hhid]

setDT(children)[, ch015_1:=ch015_1[!is.na(ch015_1)][1L] , by = hhid]
setDT(children)[, ch015_2:=ch015_2[!is.na(ch015_2)][1L] , by = hhid]
setDT(children)[, ch015_3:=ch015_3[!is.na(ch015_3)][1L] , by = hhid]
setDT(children)[, ch015_4:=ch015_4[!is.na(ch015_4)][1L] , by = hhid]

notna1 <- children %>% 
  select(mergeid,ch016_1,ch016_2,ch016_3,ch016_4) %>%
  mutate(var4 = rowSums(!is.na(select(., -mergeid))))

notna2 <- children %>% 
  select(mergeid,ch007_1,ch007_2,ch007_3,ch007_4) %>%
  mutate(var5 = rowSums(!is.na(select(., -mergeid))))

children <- children %>% 
  left_join(notna1) %>%
  left_join(notna2)

children <- children %>% 
  mutate(
		#Number of child
		n_natchild = ifelse(sl_rc022_ == "No", 0, sl_rc023_),
		n_adopt = ifelse(sl_rc038_ %in% c("No","Refusal","Don't know "), 0,sl_rc039_),
		n_child = ifelse(is.na(n_adopt),n_natchild, n_natchild + n_adopt)) %>% 
		mutate(
     n_child = ifelse(is.na(n_child), max(var4,var5), n_child)
     ) %>% 
    select(-var4,-var5) %>% 
    mutate(
		ever_children_1 = case_when(
		  grepl("No", sl_rc022_, fixed =TRUE) ~ as.numeric(0),
		  grepl("Yes",sl_rc038_, fixed =TRUE) ~ as.numeric(1),
			grepl("Yes", sl_rc022_,fixed =TRUE) ~ as.numeric(1),
			grepl("Refusal", sl_rc022_,fixed = TRUE) ~ as.numeric(-2),
			grepl("Don't know", sl_rc022_,fixed = TRUE) ~ as.numeric(-1),
			TRUE ~ NA_real_)
		) %>% 
    mutate(
		ever_children_1 = ifelse((is.na(ever_children_1)|ever_children_1 %in% c(-1,-2) | n_child > ever_children_1),1,ifelse((is.na(ever_children_1)|ever_children_1 %in% c(-1,-2) | n_child == 0),0, ever_children_1)),
		ever_children_2 = ifelse(ever_children_1 == 0, 0,ifelse(n_child %in% c(-1,-2), n_child, ifelse(n_child >= 2,1,0))),
		ever_children_3 = ifelse(ever_children_1 == 0, 0,ifelse(n_child %in% c(-1,-2), n_child, ifelse(n_child >= 3,1,0))),		
		ever_children_4 = ifelse(ever_children_1 == 0, 0,ifelse(n_child %in% c(-1,-2), n_child, ifelse(n_child >= 4,1,0))),	
		ever_children_3_plus = ifelse(ever_children_1 == 0, 0,ifelse(n_child %in% c(-1,-2), n_child, ifelse(n_child >= 3,1,0))),	
  )

                             
children <- children %>% 
  mutate(
		#Where children live
		
		live_ch_1 = ifelse(ever_children_1 == 0|n_child < 1, "No_child_1",as.character(ch007_1)),
		live_ch_2 = ifelse(ever_children_1 == 0|n_child < 2, "No_child_2",as.character(ch007_2)),
		live_ch_3 = ifelse(ever_children_1 == 0|n_child < 3, "No_child_3",as.character(ch007_3)),
		live_ch_4 = ifelse(ever_children_1 == 0|n_child < 4, "No_child_4",as.character(ch007_4)),
		
		#Children marital status
		
		mstat_ch_1 = ifelse(ever_children_1 == 0|n_child < 1, "No_child_1",as.character(ch012_1)),
		mstat_ch_2 = ifelse(ever_children_1 == 0|n_child < 2, "No_child_2",as.character(ch012_2)),
		mstat_ch_3 = ifelse(ever_children_1 == 0|n_child < 3, "No_child_3",as.character(ch012_3)),
		mstat_ch_4 = ifelse(ever_children_1 == 0|n_child < 4, "No_child_4",as.character(ch012_4)),
		
		#Children Contact
		
		contact_ch_1 = ifelse(ever_children_1 == 0|n_child < 1, "No_child_1",as.character(ch014_1)),
		contact_ch_2 = ifelse(ever_children_1 == 0|n_child < 2, "No_child_2",as.character(ch014_2)),
		contact_ch_3 = ifelse(ever_children_1 == 0|n_child < 3, "No_child_3",as.character(ch014_3)),
		contact_ch_last = ifelse(ever_children_1 == 0|n_child < 4, "No_child_4",as.character(ch014_4)),
		
		#Children ISCO
		
		isco_ch_1 = ifelse(ever_children_1 == 0|n_child < 1, "No_child_1",as.character(ch016_1)),
		isco_ch_2 = ifelse(ever_children_1 == 0|n_child < 2 ,"No_child_2",as.character(ch016_2)),
		isco_ch_3 = ifelse(ever_children_1 == 0|n_child < 3, "No_child_3",as.character(ch016_3)),
		isco_ch_last = ifelse(ever_children_1 == 0|n_child < 4, "No_child_4",as.character(ch016_4)),
		
		
		ever_adopted_ch = case_when(
			grepl("Yes", sl_rc038_,fixed = TRUE) ~ as.numeric(1),
			grepl("No", sl_rc038_,fixed = TRUE) ~ as.numeric(0),
			grepl("Refusal", sl_rc038_,fixed = TRUE) ~ as.numeric(-2),
			grepl("Don't know", sl_rc038_,fixed = TRUE) ~ as.numeric(-1),
			TRUE ~ NA_real_),		
		
		gender_child_1 = case_when(
			grepl("Refusal", sl_rc026_1) ~ as.character(-2),
			grepl("Don't know", sl_rc026_1) ~ as.character(-1),
			TRUE ~ as.character(sl_rc026_1)),
		
		gender_child_2 = case_when(
			grepl(1, n_child) ~ "No_child_2",
			grepl("Refusal", sl_rc026_2) ~ as.character(-2),
			grepl("Don't know", sl_rc026_2) ~ as.character(-1),
			TRUE ~ as.character(sl_rc026_2)),
		
		gender_child_3 = case_when(
			grepl(1|2, n_child) ~ "No_child_3",
			grepl("Refusal", sl_rc026_3) ~ as.character(-2),
			grepl("Don't know", sl_rc026_3) ~ as.character(-1),
			TRUE ~ as.character(sl_rc026_3)),
		
		gender_child_4 = case_when(
			grepl(2|3, n_child) ~ "No_child_4",
			grepl("Refusal", sl_rc026_4) ~ as.character(-2),
			grepl("Don't know", sl_rc026_4) ~ as.character(-1),
			TRUE ~ as.character(sl_rc026_4)),
		
		death_ch_1 = case_when(
			grepl("Yes", sl_rc027_1) ~ as.numeric(0),
			grepl("No",sl_rc027_1) ~ as.numeric(1),
			grepl("5", sl_rc027_1) ~ as.numeric(1),
			grepl("1",sl_rc027_1) ~ as.numeric(0),
			grepl("Don't know",sl_rc027_1) ~ as.numeric(-1),
			grepl("Refusal",sl_rc027_1) ~ as.numeric(-2),
			grepl("1959",sl_rc027_1) ~ as.numeric(1),
			TRUE ~ NA_real_),
		
		death_ch_2 = case_when(
			grepl("Yes", sl_rc027_2) ~ as.numeric(0),
			grepl("No",sl_rc027_2) ~ as.numeric(1),
			grepl("5", sl_rc027_2) ~ as.numeric(1),
			grepl("1",sl_rc027_2) ~ as.numeric(0),
			grepl("Don't know",sl_rc027_2) ~ as.numeric(-1),
			grepl("Refusal",sl_rc027_2) ~ as.numeric(-2),
			grepl("1960",sl_rc027_2) ~ as.numeric(1),
			TRUE ~ NA_real_),
		death_ch_3 = case_when(
			grepl("Yes", sl_rc027_3) ~ as.numeric(0),
			grepl("No",sl_rc027_3) ~ as.numeric(1),
			grepl("5", sl_rc027_3) ~ as.numeric(1),
			grepl("1",sl_rc027_3) ~ as.numeric(0),
			grepl("Don't know",sl_rc027_3) ~ as.numeric(-1),
			grepl("Refusal",sl_rc027_3) ~ as.numeric(-2),
			grepl("1960",sl_rc027_3) ~ as.numeric(1),
			TRUE ~ NA_real_),
		death_ch_4 = case_when(
			grepl("Yes", sl_rc027_4) ~ as.numeric(0),
			grepl("No",sl_rc027_4) ~ as.numeric(1),
			grepl("5", sl_rc027_4) ~ as.numeric(1),
			grepl("1",sl_rc027_4) ~ as.numeric(0),
			grepl("Don't know",sl_rc027_4) ~ as.numeric(-1),
			grepl("Refusal",sl_rc027_4) ~ as.numeric(-2),
			grepl("1960",sl_rc027_4) ~ as.numeric(1),
			TRUE ~ NA_real_),
		
		#Age when events occured
		#Respondent Age when child born
		age_when_child_1 = ifelse((sl_rc024_1 %in% c(-2,-1)),sl_rc024_1, sl_rc024_1- yrbirth),
		age_when_child_2 = ifelse((sl_rc024_2 %in% c(-2,-1)),sl_rc024_2, sl_rc024_2 - yrbirth),
		age_when_child_3 = ifelse((sl_rc024_3 %in% c(-2,-1)),sl_rc024_3, sl_rc024_3 - yrbirth),
		
		year_when_child_1 = sl_rc024_1,
		year_when_child_2 = sl_rc024_2,
		year_when_child_3 = sl_rc024_3) %>% 
    mutate(year_when_child_last = coalesce(sl_rc024_4,
                                           sl_rc024_5,
                                           sl_rc024_6,
                                           sl_rc024_7,
                                           sl_rc024_8,
                                           sl_rc024_9,
                                           sl_rc024_10,
                                           sl_rc024_11,
                                           sl_rc024_12,
                                           sl_rc024_13,
                                           sl_rc024_14,
                                           sl_rc024_15),
    age_when_child_last =  year_when_child_last - yrbirth
     ) %>% 
		mutate(
		when_child_die_1 = ifelse((sl_rc028_1 %in% c(-2,-1)),sl_rc028_1, sl_rc028_1- yrbirth),
		when_child_die_2 = ifelse((sl_rc028_2 %in% c(-2,-1)),sl_rc028_2, sl_rc028_2- yrbirth),
        
		#Respondent age when child move out home
		when_ch_moveout_1 = ifelse(ch015_1 %in% c(-2,-1,2999),ch015_1, ch015_1 - yrbirth),
		when_ch_moveout_2 = ifelse(ch015_2 %in% c(-2,-1,2999),ch015_2, ch015_2 - yrbirth),
		when_ch_moveout_3 = ifelse(ch015_3 %in% c(-2,-1,2999),ch015_3, ch015_3 - yrbirth),
		when_ch_moveout_4 = ifelse(ch015_4 %in% c(-2,-1,2999),ch015_4, ch015_4 - yrbirth),
		) %>%
		
		#Transform the variable when there is a designed missing value
		mutate(
		#Respondent age when child die
		death_ch_1 = ifelse(ever_children_1 == 0, 0, death_ch_1),
		death_ch_2 = ifelse(ever_children_2 == 0, 0, death_ch_2),
		
		gender_child_1 = ifelse(ever_children_1 == 0, "No_child_1", gender_child_1),
		gender_child_2 = ifelse(ever_children_1 == 0|n_child < 2, "No_child_2", gender_child_2),
		gender_child_3 = ifelse(ever_children_1 == 0|n_child < 3, "No_child_3",gender_child_3),
		gender_child_4 = ifelse(ever_children_1 == 0|n_child < 4, "No_child_4", gender_child_4),
		
		#child die: never child never die
		# when_child_die_1 = ifelse(death_ch_1 == 0, mean(when_child_die_1 , na.rm = T),ifelse(when_child_die_1 < 0, NA_real_, when_child_die_1)),
		# when_child_die_2 = ifelse(death_ch_2 == 0, mean(when_child_die_2 , na.rm = T),ifelse(when_child_die_2 < 0, NA_real_, when_child_die_2)),
		# when_child_die_3 = ifelse(death_ch_3 == 0, mean(when_child_die_3 , na.rm = T),ifelse(when_child_die_3 < 0, NA_real_, when_child_die_3)),
		# when_child_die_4 = ifelse(death_ch_3 == 0, mean(when_child_die_4 , na.rm = T),ifelse(when_child_die_4 < 0, NA_real_, when_child_die_4)),
        
		#Sustituing with the mean when children never born
		
		age_when_child_1 = ifelse(ever_children_1 == 0, mean(age_when_child_1 , na.rm = T), ifelse(age_when_child_1 < 13, NA_real_, age_when_child_1)),
		age_when_child_2  =  ifelse(ever_children_1 == 0|n_child < 2, mean(age_when_child_2, na.rm = T), ifelse(age_when_child_2 < 13, NA_real_, age_when_child_2)),
		age_when_child_3  =  ifelse(ever_children_1 == 0|n_child < 3, mean(age_when_child_3, na.rm = T), ifelse(age_when_child_3 < 13, NA_real_, age_when_child_3)),
		#age_when_child_last  =  ifelse(ever_children_1 == 0|n_child < 4, mean(age_when_child_4, na.rm = T), ifelse(age_when_child_4 < 13, NA_real_, age_when_child_4)),
		
		when_ch_moveout_1 = ifelse(when_ch_moveout_1 < -2 , NA_real_,when_ch_moveout_1),		
		when_ch_moveout_2 = ifelse(when_ch_moveout_2 < -2 , NA_real_, when_ch_moveout_2),	
		when_ch_moveout_3 = ifelse(when_ch_moveout_3 < -2 , NA_real_, when_ch_moveout_3),	
		when_ch_moveout_4 = ifelse(when_ch_moveout_4 < -2 , NA_real_, when_ch_moveout_4),	
		)%>%	
	dplyr::select(-c(sl_rc022_,sl_rc038_,sl_rc023_,sl_rc039_,sl_rc026_1:sl_rc026_15,
	sl_rc027_1:sl_rc027_15,sl_rc024_1:sl_rc024_15,sl_rc028_1:sl_rc028_2))%>%
	dplyr::select(-starts_with("ch")) %>% 
  as.data.frame()


# children$age_when_first_ch_moveout = ifelse((children$age_when_first_ch_moveout > 100 & children$age_when_first_ch_moveout <2999) ,NA_real_,children$age_when_first_ch_moveout)

# children$age_when_second_ch_moveout = ifelse((children$age_when_second_ch_moveout > 100 & children$age_when_second_ch_moveout < 2999), NA_real_, children$age_when_second_ch_moveout)

#who lose the children?
children %>%
	filter(death_ch_1 == 1, !gender_child_1 %in% c(-1,-2))%>%
	mutate(age_children = when_child_die_1 - age_when_child_1)%>%
	filter(age_children > 0) %>% 
	ggplot(.,aes(age_children))+geom_histogram() + facet_wrap(vars(country))

#Distribution of skills 
children %>%
	filter(ever_children_1 == 1, isco_ch_1 == "Unemployed ")%>%
	mutate(age_children =  age_when_child_1) %>%
	filter(age_children > 0) %>% 
	ggplot(.,aes(age_children))+geom_histogram() + facet_wrap(vars(country))

#MATERNAL BENEFIT
xe <- children %>% dplyr::select(starts_with("sl_rc032")) %>% select_if(., is.numeric) %>% names()
children$sl_MBenefit = apply(children[,which(names(children) %in% xe)],1, function(x) mean(x[x > 0],na.rm = T))

rm(chdf)
rm(chSL)
rm(chSL_panel)
rm(sharelife_new)
#maternity benefit source
children <-  children %>% dplyr::select(-starts_with("sl_rc032")) 

benefit <- children %>%
	dplyr::select(starts_with("sl_rc031"),mergeid,gender,ever_children_1) %>%
	tidyr::gather(key = "Variable",value = "Value",-c(mergeid,gender,ever_children_1))%>%
	mutate(Variable = gsub("sl_rc031d1", "MB_Income_from_employment", Variable))%>%
	mutate(Variable = gsub("sl_rc031d2", "MB_spousefinance", Variable))%>%
	mutate(Variable = gsub("sl_rc031d3", "MB_state",Variable))%>%
	mutate(Variable = gsub("sl_rc031d4", "MB_Statechildbenefit", Variable))%>%
	mutate(Variable = gsub("sl_rc031d5", "MB_familyfinance",Variable))%>%
	mutate(Variable = gsub("sl_rc031d6", "MB_banks", Variable))%>%
	mutate(Variable = gsub("sl_rc031dot", "MB_others", Variable))%>%
	mutate(Value = case_when(
	grepl("Selected",Value)~ as.numeric(1),
	grepl("Not selected",Value)~ as.numeric(0),
	grepl("Refusal",Value)~ as.numeric(-2),
	grepl("Don't know",Value)~ as.numeric(-1),
	grepl("Male",gender)~ as.numeric(0),
	grepl(0,ever_children_1)~ as.numeric(0),
	TRUE ~ NA_real_
	)) %>% 
	tidyr::spread(Variable, Value)

xe <- children %>% dplyr::select(starts_with("sl_rc031")) %>% names()
children <- children[,-which(names(children) %in% xe)] %>% full_join(benefit)

#Left job for work(not included)
# xe <- children %>% dplyr::select(starts_with("sl_rc030")) %>% names()
# stopwork <- children %>%
	# dplyr::select(starts_with(c("sl_rc030_1")),mergeid,gender,ever_children) %>%
	# tidyr::gather(key = "Variable",value = "Stop_work",-c(mergeid,gender,ever_children))%>%
	# mutate(Stop_work = case_when(
	# grepl("3 years or longer, but worked at some point later",Stop_work)~ ">=3y",
	# grepl("More than 1 month but less than 3 months",Stop_work) ~ "(1m_3m]",
	# grepl("1 month or less",Stop_work)~ "<=1m",
	# grepl("More than 6 months but less than 1 year",Stop_work) ~ "(6m_1y]",
	# grepl("More than 1 year but less than 3 years",Stop_work)~  "(1y_3y]",
	# grepl("More than 3 months but less than 6 months",Stop_work) ~ "(3m,6m]",
	# TRUE ~ Stop_work),
	# ) %>%
	# droplevels() %>%
	# dummy_cols(.,"Stop_work",remove_selected_columns= TRUE)%>%
	# clean_names()%>%
	# mutate(
	# stop_work_3m_6m = case_when(
	# grepl("Male",gender)~ 0,
	# grepl(0,ever_children) ~ 0,
	# TRUE ~ as.numeric(stop_work_3m_6m)),
	# stop_work_1m_3m = case_when(
	# grepl("Male",gender)~ 0,
	# grepl(0,ever_children)~ 0,
	# TRUE ~ as.numeric(stop_work_1m_3m )),
	# stop_work_1y_3y = case_when(
	# grepl("Male",gender)~ 0,
	# grepl(0,ever_children)~0,
	# TRUE ~ as.numeric(stop_work_1y_3y)),
	# stop_work_6m_1y = case_when(
	# grepl("Male",gender)~ 0,
	# grepl(0,ever_children)~ 0,
	# TRUE ~ as.numeric(stop_work_6m_1y)),
	# stop_work_1m = case_when(
	# grepl("Male",gender)~ 0,
	# grepl(0,ever_children)~ 0,
	# TRUE ~ as.numeric(stop_work_1m)),
	# stop_work_3y = case_when(
	# grepl("Male",gender)~ 0,
	# grepl(0,ever_children)~ 0,
	# TRUE ~ as.numeric(stop_work_3y))
	# )%>%
	# remove_constant()
#clean from near zero variance

NZV<- nearZeroVar(children, saveMetrics = TRUE)
NZV[NZV[,"zeroVar"] > 0, ] 
NZV[NZV[,"zeroVar"] + NZV[,"nzv"] > 0, ]	

xnzv <- rownames(NZV[NZV[,"zeroVar"] + NZV[,"nzv"] > 0, ]) 
xnzv <- xnzv[-which(xnzv %in% c("death_ch_1","death_ch_2","n_adopt","age_when_child_2",
                                "age_when_child_3","age_when_child_4"))]
xe <- children %>% dplyr::select(starts_with("sl")) %>% names()
children <- children[,-which(names(children) %in% c(xe,xnzv))] 

summary(children)	
#Replace missing value for respondent of same hhid
#Replace NA for individual in the same household for children information


gg_miss_var(children)
stargazer(children)	
save(children, file = "Dataset_constructed/children.RData")

#Frequency tables

children %>%
 tidyr::gather(Variable,Value,-c(mergeid,yrbirth,country,gender))%>%
 mutate(Value= as.factor(Value))%>%
 ggplot(.,aes((Value)))+ geom_bar()+facet_wrap(~Variable,scales="free")