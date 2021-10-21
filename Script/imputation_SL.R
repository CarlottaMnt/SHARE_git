setwd("C:/Users/carlotta/Desktop/Dropbox/SHARE_new/R")
setwd("//crc/Team_work/Caralpvk/SHARE_new/R")

library(knitr)
library(xtable)
library(plyr)  
library(gtable)
library(naniar)
library(gridExtra)
library(foreign)
library(data.table)
library(stargazer)
################################################################################
################################################################################
############################ imputation.RData ###############################
################################################################################
################################################################################
rm(list=ls())

load("Dataset_constructed/sharelife_new.RData")

impW1 <- read.dta("Data/sharew1_rel7-1-0_gv_imputations.dta") %>%
	dplyr::select(
		mergeid,
        gender,
        country,
		implicat,
		yedu,
		isced,
		thinc,
		hnetw,
		fahc,
		fohc,
		mstat,
		nchild,
		ngrchild
		)%>%
	filter(mergeid %in% sharelife_new$mergeid) %>%
	mutate(lifesat =-11, lifehap = -11, wave = 1)

impW2 <- read.dta("Data/sharew2_rel7-1-0_gv_imputations.dta")%>%
	dplyr::select(
		mergeid,
        gender,
        country,
		implicat,
		yedu,
		isced,
		thinc,
		hnetw,
		fahc,
		fohc,
		mstat,
		nchild,
		ngrchild,
		lifesat,
		lifehap
		)%>%
	filter(mergeid %in% sharelife_new$mergeid)%>%
	mutate(wave =2)

impW4 <- read.dta("Data/sharew4_rel7-1-0_gv_imputations.dta")%>%
 dplyr::select(
		mergeid,
        gender,
        country,
		implicat,
		yedu,
		isced,
		thinc,
		hnetw,
		fahc,
		fohc,
		mstat,
		nchild,
		ngrchild,
		lifesat,
		lifehap
		)%>%
	filter(mergeid %in% sharelife_new$mergeid)%>%
	mutate(wave = 4)

impW5 <- read.dta("Data/sharew5_rel7-1-0_gv_imputations.dta")%>%
 dplyr::select(
		mergeid,
        gender,
        country,
		implicat,
		yedu,
		isced,
		thinc,
		hnetw,
		fahc,
		fohc,
		mstat,
		nchild,
		ngrchild,
		lifesat,
		lifehap
		)%>%
	filter(mergeid %in% sharelife_new$mergeid)%>%
	mutate(wave = 5)

impW6 <- read.dta("Data/sharew6_rel7-1-0_gv_imputations.dta")%>%
 dplyr::select(
		mergeid,
        gender,
        country,
		implicat,
		yedu,
		isced,
		thinc,
		hnetw,
		fahc,
		fohc,
		mstat,
		nchild,
		ngrchild,
		lifesat,
		lifehap
		)%>%
	filter(mergeid %in% sharelife_new$mergeid)%>%
	mutate(wave = 6)

impW7 <- read.dta("Data/sharew7_rel7-1-0_gv_imputations.dta")%>%
 dplyr::select(
		mergeid,
        gender,
        country,
		implicat,
		yedu,
		isced,
		thinc,
		hnetw,
		fahc,
		fohc,
		mstat,
		nchild,
		ngrchild,
		lifesat,
		lifehap
		)%>%
	filter(mergeid %in% sharelife_new$mergeid)%>%
	mutate(wave = 7)


#Fix the number of granchild!!

imputation_SL <- rbind(impW1,impW2,impW4,impW5,impW6,impW7) %>%
		mutate(ngrchild = case_when(
		grepl(-99, ngrchild) ~ NA_real_,
		TRUE ~ as.numeric(ngrchild))
		)%>%
		dplyr::group_by(mergeid) %>% 
		dplyr::summarise(
		income = mean(thinc, na.rm=TRUE),
		wealth = mean(hnetw, na.rm=TRUE),
        con_in = mean(fahc, na.rm= TRUE),
		con_out = mean(fohc, na.rm= TRUE),
		yedu = mean(yedu, na.rm= TRUE),
		ngrchild = round(mean(ngrchild, na.rm = TRUE),0),
		isced = last(isced)
		)%>%
	mutate(
	ngrchild = ifelse(is.nan(ngrchild),NA_real_,ngrchild))

save(imputation_SL,file ="Dataset_constructed/imputation_SL.RData")

#3559 respondents have NA ngrchild



