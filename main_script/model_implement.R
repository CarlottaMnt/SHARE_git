---
title: "Model Implementation"
author: "Carlotta Montorsi"
date: "10/21/2021"
---
library(psych)
#Main packages$s
library(devtools)
library(ggfortify); 
library(ggplot2)
library(glmnet)
library(pROC)
library(rpart)
library(rpart.plot) 
library(rattle)
library(tidyverse)
library(caret)
library(randomForest)
#--------------
library(tables)
library(qwraps2)
library(ggbiplot)
library(janitor)
library(party)
library(neuralnet)
library(knitr)
library(modelr)
library(broom)
library(ggpubr)
library(kernlab)
library(ggthemes)
library(partykit)
library(bestNormalize)
library(xtable)
library(plyr)    
library(gtable)
library(grid)
library(corrplot)
library(rpart.plot)
library(here)
library(naniar)
library(forcats)
library(gridExtra)
library(foreign)
library(data.table)
library(sf)
library(fastDummies)
library(stargazer)
library(VIM)
library(MLmetrics)
library(caTools)

source("Functions.R")

#Import the output and the predictor set (imputed and not imputed)
#Output
load("MyData_file/output.RData") 

#Predictors imputed
load("MyData_file/covariate_imputed_reduced.RData") 

#Predictors not imputed
load("MyData_file/cov.RData") 

#Extract relevant covariates from the predictor set: 
#Not include current circumstances and imputation indicator columns

covariate <- covariate_imputed_reduced %>%
  clean_names() %>% 
  dplyr::select(-c(
    "yrbirth",
    "int_year",
    "hhid",
    "age_int",
    "nr_book",
	"wave",
    "country_birth_2",
    "isco_par",
    "shr",
    "cur_retired",
    "fam_resp",
    "fam_resp_imp",
    "country_birth",
    "con_in",
    "con_out",
    "income",
    "cur_married",
    "marry_1",
    "cur_employed",
    "age_when_retired_imp",
    "cur_age_youngest_child_imp",
    "cur_age_youngest_child",
    "age_when_start_fin_stressperiod_imp",
    "age_when_start_happyperiod_imp",
    "age_when_start_stressperiod_imp",
    "age_when_start_hungerperiod_imp",
    "age_when_stop_fin_stressperiod_imp",
    "age_when_stop_stressperiod_imp",
    "age_when_stop_happyperiod_imp",
    "age_when_stop_hungerperiod_imp",
    "age_when_start_hungerperiod_imp",
    "age_when_stop_fin_stressperiod_imp",
    "age_when_stop_stressperiod_imp",
    "age_when_stop_happyperiod_imp",
    "age_when_stop_hungerperiod_imp",
    "age_immigration_2_imp",
    "age_immigration_3_imp",
    "age_immigration_4_imp",
    "age_immigration_5_imp",
    "age_outmigration_1_imp",
    "age_outmigration_3_imp",
    "age_outmigration_2_imp",                           
    "age_outmigration_4_imp",
    "age_outmigration_5_imp", 
    "age_outmigration_6_imp",
  ),
  -ends_with(c("never_end","never_end_imp","_na_imp")),
  -starts_with(c(
    "country_1",
    "country_2",
    "country_3",
    "country_4",
    "country_5",
    "country_6",
    "region_1",
    "region_2",
    "region_3",
    "region_4",
    "region_5",
    "region_6"
  ))
  ) %>% 
  names()
  
#Which countries are we anlyse?
countries <- c( 
    "Austria",
		"Belgium",
		"Switzerland",
		"Czech_Republic",
		"Germany",
		"Denmark",
		"Spain",
		"France",
		"Greece",
		"Italy",
		"Poland",
		"Sweden"
)

#which outcome?
output_name <- c("frailty_index","income","lifesat","mental_index")

#Build the cleaned dataframes for all the countries. 
#The function below creates a list of dataframe for each sample you state above.

dataframe = builData(
			target= output_name,  #provide the output_name
			id = "mergeid",
			dfy = output,	     #Provide the output data set
			x_names = covariate, #Provide the vector of covariates names from the predictor set
			dfx = covariate_imputed_reduced,  #Provide the predictor dataset
			pooled = T, #Create pooled sample?(TRUE/FALSE)
			cnt = TRUE, #Create countries samples?(TRUE/FALSE)
			adj = F, #Adjust the frailty index to remove age and gender effect?(TRUE/FALSE)
			VIF = F, #Remove Variance inflator factors? (TRUE/FALSE)
			wave = FALSE) #Create the wave sample?

#Perform principal component on the regressor matrix for two sample
summary_pc <- list()
pca <- list()
set.seed(134)
for(t in c("pooled","Poland")){
    df1 <- dataframe[[t]] %>% select_if(is.numeric) %>%    dplyr::select(-output_name)
    hc <- findCorrelation(cor(df1), cutoff = 0.9, verbose = FALSE)
    hc = names(df1[,hc])
   
    if(t == "pooled"){
    pc <- prcomp(df1, center = TRUE,scale. = TRUE)
    pca[[t]] <- autoplot(pc, data = dataframe[[t]],
                              colour  = "country",
                              loadings = TRUE,
                              loadings.label = FALSE,
                              loadings.colour = 'blue') +
                              theme_tufte() 
     #Five top correlated
    topN <- 5
    load.rot <- pc$rotation
    names(load.rot[,1][order(abs(load.rot[,1]),decreasing=TRUE)][1:topN])
    
    }else{
    pc <- prcomp(df1, center = TRUE,scale. = TRUE)
    pca[[t]] <- autoplot(pc, data = dataframe[[t]],
                              loadings = TRUE,
                              loadings.label = F,
                              loadings.colour = 'blue') +
                              theme_tufte() 
     #Five top correlated
    topN <- 5
    load.rot <- pc$rotation
    names(load.rot[,1][order(abs(load.rot[,1]),decreasing=TRUE)][1:topN])
     }
    }
pca[["pooled"]]
pca[["Poland"]]

#Creating train and test

trtestdf <- vector(mode="list", length = length(output_name))
names(trtestdf) <- output_name

#We now apply the function to create the train and test dataset. 

for(s in output_name){
 trtestdf[[s]] <- dataprep(
			df = dataframe, 		#provide all the dataset in a list format
			test = c("pooled", countries), #provide names for training/test sample
			target = s,                  #provide name for response variables
			norm = ifelse(s == "lifesat", F,T) , #normalizing numerical target?
			id = "mergeid",                      #provide id identifier
			cat = ifelse(s != "mental_index",F,T) #transform into category? 
			) 
}
			
#The piece of code below create the plot of the train and test sample for the selected target.

##Create list to store graphs
train <- list()

#Which are the target you wnat to graphically explore?

target_name <- c("frailty_index","lifesat", "income","mental_index")

#Define which outcome to treat as numerical and which to be treated as classes. 
###This should be in line with what you define in the databuilding function.

target_name_num <- c("frailty_index","lifesat")
target_name_cat <- c("mental_index","lifesat")

for(i in target_name){
  for(t in c("pooled",countries)){
    
  trainData <- trtestdf[[i]][[1]][[t]] #we are selecting the i output, train, sample t

  #Train Output
  
  Y_train = trainData %>% select(output)
  
  #Mean and standard deviation in the train sample of numerical target
  if(i %in% target_name_num){
  mean_train = mean(Y_train$output)
  sd_train =   sd(Y_train$output)
  }
  #Percentage of observation in each class of categorical target
  
  if(i %in% c(target_name_cat)){
  perc_train = Y_train %>%
    dplyr::mutate(n = n()) %>% 
    dplyr::group_by(output) %>%
    dplyr::summarise(count = n(), pct = round(count/n,4)) %>% 
    mutate(sample = "train")
  }
  
  Y_train$sample <- "train"

  #TEST output

  testData <- trtestdf[[i]][[2]][[t]]
  Y_test = testData %>% select(output)
  
  if(i %in% target_name_num){
  mean_test = mean(Y_test$output)
  sd_test = sd(Y_test$output)
  }
  
  if(i %in% target_name_cat){
  perc_test = Y_test %>%
  dplyr::mutate(n = n()) %>% 
  dplyr::group_by(output) %>%
  dplyr::summarise(count = n(), pct = round(count/n,4)) %>% 
  mutate(sample = "test")
  }
  Y_test$sample = "test"

#Explore the distribution of the target in the train and test sample in the three target

df_plot <- rbind(Y_train,Y_test)
stats <- matrix(c(round(mean_test,2), round(mean_train,2),round(sd_train,2), round(sd_test,2)), nrow=2, ncol = 2) %>% as.data.frame()
colnames(stats) <- c("mean","sd")
stats$sample <- c("train","test")

if(i %in% target_name_cat){
df_perc <- rbind(perc_train, perc_test)
 if(i == "mental_index"){
 df_perc$output <- plyr::revalue(df_perc$output, c(X1= "Not Depressed",X2 ="Depressed"))
 }
 train[[i]][[t]] <- ggplot(df_perc,  
                            aes(x = as.factor(output),
                                y = pct,
                                color = sample,
                                fill = sample, 
                                label = scales::percent(round(pct,2)))) +
  geom_col(alpha = 0.5, position = "dodge")+
  scale_color_manual(
      name = "Sample",
      breaks = c("train", "test"),
      values = c("#9370DB","#014421")
    )+
    scale_fill_manual(
      name = "Sample",
      breaks = c("train", "test"),
      values = c("#9370DB","#014421")
    )+
    scale_y_continuous(labels = scales::percent)+
    scale_x_discrete()+
  labs(fill = NULL)+
  xlab(paste0(i))+
  theme_tufte()+
  ggtitle(paste0(i))
  if(i == "mental_index"){
  train[[i]][[t]] <-  train[[i]][[t]]  +
      geom_text(position = position_dodge(width = .5),    # move to center of bars
              vjust = -0.5,    # nudge above top of bar
              size = 4)
  }
 if(i == "lifesat"){
  train[[i]][[t]] <- train[[i]][[t]] +
      geom_vline(xintercept = mean_train, colour = "black", linetype = "longdash")+
      geom_vline(xintercept = mean_test, colour = "#014421", linetype = "dotted")+
    geom_text(data = stats %>%  filter(sample == "train"), aes(y = 0.12, x= 2, label =paste0("Mean: ", mean)),
            color = "#9370DB",
            alpha = 1,
            vjust = 1, 
            size = 4
    )+
    geom_text(data = stats %>%  filter(sample == "train"), aes(y = 0.1,x= 2, label = paste0("Sd: " ,sd)),
            color = "#9370DB",
            alpha = 1,
            position = position_dodge(width = 1.5),
            vjust = 1, 
            size = 4
    )+
    geom_text(data = stats %>%  filter(sample == "test"), aes(y = 0.12, x= 4.3, label =paste0("Mean: ", mean)),
            position = position_dodge(width = 1.5),
            alpha = 1,
            color = "#014421",
            vjust = 1, 
            size = 4
    )+
    geom_text(data = stats %>%  filter(sample == "test"), aes(y =0.1, x= 4, label = paste0("Sd: " ,sd)),
            position = position_dodge(width = 1.5),
            color = "#014421",
            alpha = 1,
            vjust = 1,
            size = 4
    ) 
 }
}
else {
    
  train[[i]][[t]] <- ggplot(df_plot, aes(output, fill = sample, color = sample)) +
    theme_tufte()+
    geom_histogram(aes(y=..count../sum(..count..) * 100), bins = 30, alpha = 0.8, position = "dodge") +
    
    geom_text(data = stats %>%  filter(sample == "train"), aes(y = 4, x= -2, label =paste0("Mean: ", mean)),
            color = "#9370DB",
            vjust = 1, 
            size = 4,
            alpha = 1
    )+
    geom_text(data = stats %>%  filter(sample == "train"), aes(y = 3,x= -2, label = paste0("Sd: " ,sd)),
            color = "#9370DB",
            position = position_dodge(width = 1.5),
            vjust = 1,
            size = 4,
            alpha = 1
    )+
    geom_text(data = stats %>%  filter(sample == "test"), aes(y = 4, x= 2, label =paste0("Mean: ", mean)),
            position = position_dodge(width = 1.5),
            color = "#014421",
            vjust = 1, 
            size = 4,
            alpha = 1
    )+
    geom_text(data = stats %>%  filter(sample == "test"), aes(y = 3, x= 2, label = paste0("Sd: " ,sd)),
            position = position_dodge(width = 1.5),
            color = "#014421",
            vjust = 1, size = 4,
            alpha = 1
    ) +
    geom_vline(xintercept = mean_train, colour = "black", linetype = "longdash")+
    geom_vline(xintercept = mean_test, colour = "#014421", linetype = "dotted") +
    scale_color_manual(
      name = "Sample",
      breaks = c("train", "test"),
      values = c("#9370DB", "#014421")
    )+
    scale_fill_manual(
      name = "Sample",
      breaks = c("train", "test"),
      values = c("#9370DB","#014421")
    )+
  labs(fill = NULL)+
  xlab(paste0(i))+
  ylab("Frequencies")+
  ggtitle(paste0(i))
  }
  }
}

ggarrange(train[["frailty_index"]][["pooled"]],
          nrow = 1,
          ncol = 1,
          common.legend = TRUE, legend="bottom")
          
ggarrange(train[["lifesat"]][["pooled"]],
          nrow = 1,
          ncol = 1,
          common.legend = TRUE, legend="bottom")

ggarrange(train[["mental_index"]][["pooled"]],
          nrow = 1,
          ncol = 1,
          common.legend = TRUE, 
          legend="bottom"
          )          
          

target_name <- c("frailty_index","mental_index","income","lifesat")

Y_train <- vector(mode= "list", length = length(target_name))
Y_test <-  vector(mode= "list", length = length(target_name))
names(Y_train) <- target_name
names(Y_test) <- target_name

X_test <- vector(mode = "list", length = length(target_name))
names(X_test) <- target_name

trainData <- vector(mode= "list", length = length(target_name))
names(trainData) <- target_name

#Extract output and covariate from in the training and test sample------------------------

##Now we separately define predictor set and target in the test sample. 
#And trainData for each target.

#i: iterate the targets
#t: interate the samples

for(i in target_name){
  for(t in c("pooled",countries)){
  
  #TRAIN SET 
  train <- trtestdf[[i]][[1]][[t]] #we are selecting the i output, train, sample

  trainData[[i]][[t]] <- train
  n = nrow(trainData[[i]][[t]] )


  #TEST output

  testData <- trtestdf[[i]][[2]][[t]]
  Y_test[[i]][[t]] = testData %>% select(output)
  X_test[[i]][[t]] = testData %>% select(-output)
  }
}

#save train and test for reproducing results

save(Y_test,file = "Output_file/y_test.RData")
save(X_test,file = "Output_file/x_test.RData")
save(trainData, file = "Output_file/traindata.RData")

# Supervised Machine learning models----------------------------------------------------
##We will implement seven main algorithm on the training data we have just create:

# 1. MULTIPLE LINEAR REGRESSION
# 2. LASSO
# 3. RIDGE
# 4. ELASTIC NET
# 5. TREE
# 6. RANDOM FOREST
# 7. NEURAL NETWORK

#Fitting multiple linear regression for NUMERICAL TARGET with least squares method

lm <- list()
validation <- list()  #store the validation errors
predictions <- list() #store the hold out set predictions
test_measure <- list()#store the test errors
Rsquared <- list()    #store the R2 for all the algorithms
RMSE <- list()        #store the RMSE
MAE <- list()         #store the MAE
p <- list()           #store the plot for the 

for(i in c("frailty_index","lifesat","income")){
  for(t in c("pooled", countries)){
 
    lm[[i]][[t]] = train(
      form = output ~ .,
      data = trainData[[i]][[t]],
       trControl = trainControl(method = "cv", number = 10),
      method = "lm"
      )
  }
}
 #Make and store predictions: for income and frailty, predictions with no transformation.
 #for lifesat, ceiling the predictions to be integer . 
p <- list()
for(i in c("frailty_index","lifesat","income")){
  for(t in c("pooled", countries)){    
        if(i %in%  c("frailty_index","income")){
              predictions[[i]][[t]][["lm"]] <- lm[[i]][[t]] %>% predict(X_test[[i]][[t]]) 
              validation[[i]][[t]][["lm"]] <- lm[[i]][[t]]$resample%>% 
              as.data.frame() %>% 
              mutate(model = "lm")
        }
          else{
              predictions[[i]][[t]][["lm"]] <- lm[[i]][[t]] %>% predict(X_test[[i]][[t]]) 
              validation[[i]][[t]][["lm"]] <- lm[[i]][[t]]$resample %>% 
              as.data.frame() %>% 
              mutate(model = "lm")
              }
  }
}

