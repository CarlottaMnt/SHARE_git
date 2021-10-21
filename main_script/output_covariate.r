setwd("C:/Users/carlotta/Desktop/Dropbox/SHARE_new/R")
#####################################################################################################
#####################OUTPUT_COVARIATE#######################################################
#####################################################################################################

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
 install.packages("stargazer")
 install.packages("car")
 install.packages("tables")
 install.packages("Blackmore")
 install.packages("fastDummies")
 install.packages("tmaptools")
 install.packages("tmap") 
 install.packages("sf")
#install.packages("ggcorrplot")
#library(ggcorrplot)
#library(MCMCpack)
library(fastDummies)
#library(ggmap)
library(psych)
library(car)
library(tables)
data(Blackmore)
library(qwraps2)
library(tidyverse)# This package is very useful for data analysis
#library(tidybayes)# This package can be useful when performign bayesian analysis
#library(spdep)   #This package may be useful for conducting spatial exploratory data analysis
#install.packages("data.table")

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
library(sf)
library(tmap)
library(tmaptools)
library(stargazer)
#import the data
rm(list =ls())
source("Functions.R")
#Import all the dataset of interest
#OUTPUTS
load("Dataset_constructed/mentalSL.RData")#mental_SHARELIFE
load("Dataset_constructed/frailtySL.RData")#frailty_SHARELIFE
load("Dataset_constructed/lifesatSL.RData")#life_satisfaction_SHARELIFE
load("Dataset_constructed/happySL.RData")#happiness_SHARELIFE
load("Dataset_constructed/sphusSL.RData")#self perceived health_SHARELIFE

#COVARIATES
load(file = "MyData_file/demographic.RData")
 
load(file = "MyData_file/current_df.RData")
 
load(file = "MyData_file/childhood.RData")
 
load(file = "MyData_file/job_df.RData")
 
load( file = "MyData_file/df_partner.RData")
 
load(file = "MyData_file/children_df.RData")
 
load( file = "MyData_file/gl_df.RData")
 
load( file = "MyData_file/ac_fn_df.RData")
 
load(file = "MyData_file/work_env_df.RData")	

#Write down the output of interest

#Two outcome
output1 <- merge(frailtySL, mentalSL, by = c("mergeid","country","cciw","gender"))
colnames(output1) <- gsub("\\.x", "_frailty", colnames(output1))
colnames(output1) <- gsub("\\.y", "_mental", colnames(output1))
output2 <- merge(sphusSL,lifesatSL, by =  c("mergeid","country","cciw","gender"))
colnames(output2) <- gsub("\\.x", "_sphus", colnames(output2))
colnames(output2) <- gsub("\\.y", "_lifesat", colnames(output2))
output3 <- merge(output1,output2, by =  c("mergeid","country","cciw","gender"))
output <- merge(output3,happySL, by = c("mergeid","country","cciw","gender")) %>% 
  dplyr::rename(season_happy = season, age_int_happy = age_int, int_year_happy = int_year, int_month_happy = int_month, wave_happy = wave)


save(output,file="MyData_file/output.RData")

#COVARIATES--------------------------------------------------------
#Current situation

cov1 <- demographic %>% left_join(current_df)
cov2 <- df_partner %>% left_join(ac_fn_df[,-which(names(ac_fn_df) %in% "cohort")])
cov3 <-children_df %>% left_join(work_env_df)
cov4 <- job_df %>% left_join(gl_df)
cov <- cov1 %>% left_join(cov2) %>% left_join(cov3) %>% left_join(cov4) %>% left_join(childhood)
		
head(cov %>% arrange(hhid))
View(head(cov %>% arrange(hhid),20))


save(cov,file = "MyData_file/cov.RData")

ggplot(output %>% filter(ac012_ >= 0), aes(ac012_, color = season_lifesat, fill = season_lifesat ))+ geom_bar(position = "dodge")

#Summary tables analysis Output. LifeSatisfaction is a factor!!!!
d <- list(frailtySL,lifesatSL,mentalSL)
table <- list()
for (i in c(1:length(d))) 
{
 sumTable1 <- merge(d[[i]], cov[,c("mergeid","age")], by = c("mergeid"))%>%
    tidyr::gather(Variable,Value,-c(1:10, age))%>% 
	filter(age >= 49, age <= 89, !(Value %in% c(-1,-2,-11)),!is.na(Value))%>%
	tidyr::spread(Variable,Value)

 table[[i]] <- stargazer(sumTable1, omit.summary.stat = c("p25","p75"))
}

#FRAILTY INDEX
DF.wide <- list()
for (i in c("frailty_index","lifesat","mental_index"))
{ 
 sumTable1 <- left_join(output[,which(names(output) %in% c(i,"mergeid"))], cov ,by = "mergeid") %>%
	dplyr::select(age,cciw,country,gender,i,age_group)%>%
	droplevels() %>%
	filter(age >= 49, age <= 89,!is.na(cciw)) %>%
	mutate(output = .[,which(names(.) %in% c(i))])%>%
	mutate(output = as.numeric(as.character(output)))%>%
		dplyr::group_by(country) %>%
		dplyr::mutate(
			n_weight = sum(cciw,na.rm= TRUE),
			n_unweight = n()
			) %>%
		mutate(
			nu = cciw/n_weight,
			weight_out = nu * output
			) %>%
		group_by(country) %>%
		dplyr::summarise(
			output_weighted_av = round(sum(weight_out, na.rm = TRUE),2),
			output_av = round(mean(output, na.rm = TRUE),2),
			)
		DF.wide[[i]] <- cbind(dcast(sumTable1, country ~ ., value.var= "output_av"),dcast(sumTable1, country ~., value.var= "output_weighted_av")[,-1])
        kable(DF.wide[[i]],format="latex", digits=2)
}

#Mental Index

ggplot(left_join(output, cov[,c("mergeid","income")]), aes(y = frailty_index,x = log(income), shape = gender, color = country)) + 
geom_point(alpha = 0.3, color = "blue") + 
geom_smooth(alpha=1,level=0.90, span = 3) + 
facet_wrap(vars(country))
xlab("Log of income")
ggsave("frailty_smoothSL.png")

#Mental

ggplot(left_join(output, cov[,c("mergeid","n_child")]), aes(y= n_child, x= as.integer(mental_index),color = gender))+
geom_smooth(alpha =1.4,level=0.90) + 
geom_point(alpha = 0.3) + 
scale_x_discrete(limits=0:12)+
facet_wrap(vars(country))+
ylab("Number of children")+
xlab("Depression scale")
ggsave("mental.png")
ggplot(mental, aes(y = mental_index,x = age,color = gender))+ geom_point(alpha = 0.3) + geom_smooth(alpha =1.4)+facet_wrap(vars(country))
ggsave("mental_smooth.png")

ggplot(cbind(output,cov$income), aes(x=mental_index, y=as.factor(country), fill = gender))+
geom_boxplot(alpha=0.8, position = "dodge")+
scale_x_discrete(limits=0:12)+
xlab("Depression scale")+
ylab("Country")+
guides(fill = guide_legend(reverse = TRUE))

ggsave("mental_boxplot.png")
cov$income = ifelse(cov$income == 0, NA, cov$income)

scaleFUN <- function(x) sprintf("%.2f", x)
ggplot(cbind(output,cov$yedu), aes(x=mental_index,fill = gender, color = gender))+
geom_histogram(aes(fill = gender), stat ="bin",bins = 25, alpha=0.6)  +
facet_wrap(vars(country))+
scale_x_continuous(
  labels = scales::number_format(accuracy = 0.1,
                                 decimal.mark = '.'))+
ylab("Frequency")+
xlab("Depression Scale")
ggsave("mentalSL.png")

#More summary tables

summary <- output %>%
 group_by(country, gender) %>%
 dplyr::summarise(mean = mean(mental_index, na.rm = TRUE), sd= sd(mental_index, na.rm = TRUE),
 min = min(mental_index, na.rm = TRUE), max =max(mental_index, na.rm = TRUE), IQR = IQR(frailty_index, na.rm = TRUE))
 
summary_lsat <- left_join(output[,c("mergeid","lifesat")], cov ,by = "mergeid") %>%
 dplyr::select(age_group,age,cciw,country,gender,lifesat)%>%
	droplevels() %>%
	filter(age >= 49, age <= 89,!(lifesat %in% c("-1","-2","-11")),!is.na(lifesat))%>%
	mutate(lifesat = as.numeric(as.character(lifesat)))%>%
    group_by(country, gender) %>%
	dplyr::summarise(mean = mean(lifesat, na.rm = TRUE), sd= sd(lifesat, na.rm = TRUE),
	min = min(lifesat, na.rm = TRUE), max =max(lifesat, na.rm = TRUE), IQR = IQR(lifesat, na.rm = TRUE))

summary_lsat <- left_join(output[,c("mergeid","lifesat")], cov ,by = "mergeid") %>%
 dplyr::select(age_group,age,cciw,country,gender,lifesat)%>%
	droplevels() %>%
	filter(age >= 50, age <= 89,!(lifesat %in% c("-1","-2","-11")),!is.na(lifesat))%>%
	mutate(lifesat = as.numeric(as.character(lifesat)))%>%
    group_by(country, age_group) %>%
	dplyr::summarise(mean = mean(lifesat, na.rm = TRUE),n = n(), sd= sd(lifesat, na.rm = TRUE),
	min = min(lifesat, na.rm = TRUE), max =max(lifesat, na.rm = TRUE), IQR = IQR(lifesat, na.rm = TRUE))
	
summary_lsat <- left_join(output[,c("mergeid","lifesat")], cov ,by = "mergeid") %>%
 dplyr::select(age_group,age,cciw,country,cohort,gender,lifesat)%>%
	droplevels() %>%
	filter(age >= 48, age <= 89,!(lifesat %in% c("-1","-2","-11")),!is.na(lifesat))%>%
	mutate(lifesat = as.numeric(as.character(lifesat)))%>%
    group_by(country, cohort) %>%
	dplyr::summarise(mean = mean(lifesat, na.rm = TRUE),n = n(), sd= sd(lifesat, na.rm = TRUE),
	min = min(lifesat, na.rm = TRUE), max =max(lifesat, na.rm = TRUE), IQR = IQR(lifesat, na.rm = TRUE))

#Comparing quantile and cumulative distribution functions of FRAIILTY INDEX
for (i in c("frailty_index","lifesat","mental_index","happiness"))
{ 
 for(v in c("age_group","country_birth_3","ever_married","isced_2"))  
 {
    df <- left_join(output[,which(names(output) %in% c(i,"mergeid"))], cov ,by = "mergeid") %>%
	dplyr::select(age,cciw,country,gender,i,v)%>%
	droplevels() %>%
	filter(age >= 49, age <= 89) %>%
	mutate(Variable = .[,which(names(.) %in% c(i))], predictor = .[,which(names(.) %in% c(v))]) %>%
	mutate(Variable = as.numeric(as.character(Variable)), predictor = as.factor(predictor))%>%
	filter(!(predictor %in% c(-2,-1,-11)),!(Variable %in% c(-1,-2,-11)),!is.na(Variable), !is.na(predictor))
    
	#Box_plot
	
	xb <- sprintf("Figures/boxplot_%s.png",i)

	ggplot(df , aes(y = Variable, x = as.factor(country), fill = gender))+
	geom_boxplot(alpha=0.8, position = "dodge")+
	coord_flip()+
	xlab("")+
	ylab(paste0(i))
	theme_classic()
	
	ggsave(filename= xb)
	
	
	
	##Country quantile comparison
	xc <- sprintf("Figures/countryquantile_%s.png",i)
	df %>%
	group_by(country,gender)%>%
	dplyr::summarise(Variable_qq = quantile(Variable,c(.2,0.4,.5,.6,.8), na.rm = TRUE),quantile = names(quantile(Variable,c(.2,0.4,.5,.6,.8), na.rm = TRUE)),n=n()) %>%
	ggplot(., aes(y = Variable_qq, x = quantile, 
	color = as.factor(country), 
	linetype = gender,
	group = interaction(country,gender)
	)) +
	geom_point()+
	geom_line()+
    labs( title=paste0("Empirical quantile Function:",i),
    x = paste0("quantile"), y=paste0(i), color = "country") + 
	theme_classic()
	ggsave(filename= xc)
	#Histogram
	xh <- sprintf("Figures/histogram_%s.png",i)
	
	ggplot(df, aes(x= Variable, fill = gender, color = gender))+
    geom_histogram(aes(fill = gender), stat ="bin",bins = 25, alpha=0.6)  +
	facet_wrap(vars(country))+
	scale_x_continuous(
	labels = scales::number_format(accuracy = 0.1,
                                 decimal.mark = '.'))+
	ylab("Frequency")+
	theme_classic()+
	xlab(paste0(i))
	
	ggsave(filename = xh)
	
	
    #Cumulative density function
	
	xc <- sprintf("Figures/cumulative_%s_%s.png",i,v)
	
	ggplot(df, aes(Variable, color = gender, lty= as.factor(predictor))) + stat_ecdf(n = 10)+
    labs(title=paste0("Empirical Cumulative \n Density Function:",i),
    y = paste0("F(",i,")"), x= paste0(i), lty = paste0(v)) + 
	theme_classic()+
	facet_wrap(vars(country))
	
	ggsave(filename=xc)
	
	#Quantile function
	
	xq <- sprintf("Figures/quantile_%s_%s.png",i,v)
	
	df %>%
	dplyr::group_by(gender,predictor,country) %>%
	dplyr::summarise(Variable_qq = quantile(Variable,c(.025,0.25,.5,.75,.95), na.rm = TRUE),quantile = names(quantile(Variable,c(.025,0.25,.5,.75,.95), na.rm = TRUE)),n=n()) %>%
	ggplot(., aes(y = Variable_qq, x = quantile, 
	color = gender, 
	linetype = as.factor(predictor),
	group = interaction(predictor,gender)
	)) +
	facet_wrap(vars(country))+
	geom_point()+
	geom_line()+
    labs( title=paste0("Empirical quantile Function:",i),
    x = paste0("quantile"), y=paste0(i),lty = paste0(v)) + 
	theme_classic()
	ggsave(filename=xq)
	
	
	}
}



ggplot(df, aes(y = log_income, x= yedu, color = sphus_cat))+
geom_point() +
xlab("Year of education")+
ylab("Log(total income)")+
guides(color=guide_legend(title="Self Assessed Health"))
ggsave("sphusSL.png")

ggplot(cbind(output,cov$gender), aes(x=sphus_cat, color=gender, fill = gender))+
geom_bar()+
facet_wrap(vars(country))+
xlab("Self Assessed Health")+
ylab("count")+
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
guides(fill=guide_legend(title="Gender"),color=FALSE)
ggsave("sphus.png")

graphdf <- output %>%
		dplyr::select(ac012_,frailty_index,sphus,mental_index,happiness,gender, age_int) %>%
		gather(Variable, Value,-c(age_int,gender))%>%
		mutate(Variable = case_when(
		grepl("ac012",Variable) ~ "lifesat",
		TRUE ~ Variable))
		
ggplot(graphdf, aes(y=Value, x= age_int,color= gender))+ 
geom_smooth()+
facet_wrap(~Variable, scales = "free" )+
theme_classic()+
geom_vline(xintercept = c(49,89), lty= "dotted")
#Life Satisfaction
sumTable <- merge(base[,c("mergeid","cciw","age","yrbirth","gender")], current_adultJEP, by = "mergeid") %>%      
	dplyr::select(mergeid,age,yrbirth,cciw,country_birth_2,gender,yedu)%>%
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