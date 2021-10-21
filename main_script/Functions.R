
# PLPP <- function(data, id, period, direction = c("period", "level")) {
  # ## Data Checking and Verification Steps
  # stopifnot(is.matrix(data) || is.data.frame(data))
  # stopifnot(c(id, period) %in% c(colnames(data), 1:ncol(data)))
  
  # if (any(is.na(data[, c(id, period)]))) {
    # stop("PLPP cannot currently handle missing data in the id, period, or event variables")
  # }
  
  # ## Do the conversion
  # switch(match.arg(direction),
         # period = {
           # index <- rep(1:nrow(data), data[, period])
           # idmax <- cumsum(data[, period])
           # dat <- data[index, ]
           # dat[, period] <- ave(dat[, period], dat[, id], FUN = seq_along)},
         # level = {
           # tmp <- cbind(data[, c(period, id)], i = 1:nrow(data))
           # index <- as.vector(by(tmp, tmp[, id],
                                 # FUN = function(x) x[which.max(x[, period]), "i"]))
           # dat <- data[index, ]
         # })
  
  # rownames(dat) <- NULL
  # return(dat)
# }

# require(rcompanion)


# Calculate a pairwise association between all variables in a data-frame. In particular nominal vs nominal with Chi-square, numeric vs numeric with Pearson correlation, and nominal vs numeric with ANOVA.
# Adopted from https://stackoverflow.com/a/52557631/590437

mixed_assoc = function(df, cor_method="spearman", adjust_cramersv_bias=TRUE){
    df_comb = expand.grid(names(df), names(df),  stringsAsFactors = F) %>% set_names("X1", "X2")

    is_nominal = function(x) class(x) %in% c("factor", "character")
    # https://community.rstudio.com/t/why-is-purr-is-numeric-deprecated/3559
    # https://github.com/r-lib/rlang/issues/781
    is_numeric <- function(x) { is.integer(x) || is_double(x)}

    f = function(xName,yName) {
        x =  pull(df, xName)
        y =  pull(df, yName)

        result = if(is_nominal(x) && is_nominal(y)){
            # use bias corrected cramersV as described in https://rdrr.io/cran/rcompanion/man/cramerV.html
            cv = cramerV(as.character(x), as.character(y), bias.correct = adjust_cramersv_bias)
            data.frame(xName, yName, assoc=cv, type="cramersV")

        }else if(is_numeric(x) && is_numeric(y)){
            correlation = cor(x, y, method=cor_method, use="complete.obs")
            data.frame(xName, yName, assoc=correlation, type="correlation")

        }else if(is_numeric(x) && is_nominal(y)){
            # from https://stats.stackexchange.com/questions/119835/correlation-between-a-nominal-iv-and-a-continuous-dv-variable/124618#124618
            r_squared = summary(lm(x ~ y))$r.squared
            data.frame(xName, yName, assoc=sqrt(r_squared), type="anova")

        }else if(is_nominal(x) && is_numeric(y)){
            r_squared = summary(lm(y ~x))$r.squared
            data.frame(xName, yName, assoc=sqrt(r_squared), type="anova")

        }else {
            warning(paste("unmatched column type combination: ", class(x), class(y)))
        }

        # finally add complete obs number and ratio to table
        result %>% mutate(complete_obs_pairs=sum(!is.na(x) & !is.na(y)), complete_obs_ratio=complete_obs_pairs/length(x)) %>% rename(x=xName, y=yName)
    }

    # apply function to each variable combination
    map2_df(df_comb$X1, df_comb$X2, f)
}
mse <- function(sm) 
    mean(sm$residuals^2)


##builData
	
###This function has been created to construct the cleaned dataframes for the four training sample: pooled, wave3, wave7 and countries.
#The function requires to have:
#1: the dataset where target items are stored (dfy), 
#2: a dataset of imputed covariate (dfx),
#3: a character vector of covariates that we want to include from dfx. Default is all. 
#4: a character vector of countries to be analyised. 
#5 The default option is to create the pooled df. If you want wave and countries sample turn the default option on. 

builData <- function(
				target = NULL,
				id = NULL,
				dfy = NULL,				
				dfx = NULL,
				x_names = NULL, 
				cnt_name = NULL,
				adj = TRUE,
				VIF = FALSE,
				pooled = TRUE, #pooled sample
				wave = FALSE,  #wave sample
				cnt = FALSE    #countries sample
				){	
	
	dataframe  <- list(
					pooled = data.frame(), 
					wave3  = data.frame(),
					wave7 = data.frame(),
					cnt = data.frame()
					)
	dfx <- dfx %>% clean_names()
	x_names <- if(is.null(x_names)) colnames(dfx) else x_names
	season <- dfy %>% dplyr::select(starts_with("season")) %>% names()
	mydata.country <- list()
	xx <- list()
	set.seed(101)
	data <- merge(dfy[,which(names(dfy) %in% c(id,target,season))],
                dfx[,x_names],by = id)%>%
				gather(key = "Variable",value="Value", target) %>% 
				mutate(Value = as.numeric(as.character(Value)))%>%
				dplyr::filter(!(Value %in% c(-2,-11,-1,"Refusal","Don't know")),!(is.na(Value)))%>%
				tidyr::spread(Variable,Value)%>%
				filter(!is.na(frailty_index),!is.na(mental_index),!is.na(lifesat), !is.na(happiness),!is.na(sphus))%>%
				mutate_if(is.character,as.factor) %>%
                mutate_all(type.convert)%>%
				dplyr::select(target,id,age,cohort,country,wave,everything())%>%
			   dummy_cols(.,
			   c("cohort",
				"isced_2",
				"race",
				"country_birth_3"),
				remove_selected_columns = TRUE)%>%  
				clean_names() %>% 
				droplevels()%>%
				mutate_if(is.logical,as.numeric) %>%
				remove_constant()
	
	#Removing unecessary object
	#Ensure the dummy variable are stored as integer object
	xa <- data %>% dplyr::select(starts_with(c(
	"age_own",
	"adequate_salary",
	"season",
	"work_with")),
	ends_with("work"),
	-ends_with(c("_imp","_na_imp")),
	-ends_with((c("job_title_no_work",
	"industry_no_work"))),
	-starts_with("industry"),
	-age_group) %>%
	names()
	data <- data %>%
	 dummy_cols(.,xa,remove_selected_columns = T)

	set.seed(101)
	
	#Start the cleaning of the dataset
	print("NZV predictors")
	
	#1: indentify near zero variance predictors
	
	x = nearZeroVar(data, saveMetrics = TRUE)
	X <- rownames(x[x[,"nzv"] > 0, ])
	rm(x)
	
	#2: Remove near zero variance predictors	
	data  <- data[,-which(names(data) %in% X)]	
	
	
	#3: adjust the frailty controlling for age and gender
	if(adj == TRUE) {	
	#Best normalizing frailty	
	i = "frailty_index"
	print("Normalizing frailty_index")
	data[,i] <- bestNormalize(data[,i])$x.t	
	#Adjusting the frailty index for correcting age and gender	
	ggplot(data,aes(x = age,y = frailty_index,color=wave))+geom_smooth()+ggtitle("Frailty before adjustment")+facet_wrap(vars(country))	
	df.adj <- data[,c(i,"age","gender_male","gender_female","country","wave")] %>%
	mutate(agesq=age^2,
	ageth=age^3,
	ageft=age^4,
	agefif= age^5) 

	hit_elnet = lm(
	frailty_index ~ age + agesq + ageth + age*gender_female + age*gender_male + 
	agesq * gender_female +agesq * gender_male + ageth * gender_female+ ageth * gender_male + ageft * gender_female + country * wave + country -1 + wave +
	wave* gender_male + wave * age + country *age + wave*agesq +wave*ageth + country*gender_female + country*gender_male +
	gender_female + gender_male,
	data = df.adj,
	)	
	res.adj <- residuals(hit_elnet)
 
	#ggplot(data %>% left_join(res.adj),aes(age,res.adj,color=gender))+geom_smooth() + ggtitle("Frailty after adjustment")+facet_wrap(vars(country))

	data <-  cbind(data,res.adj)  %>%
				select(res.adj,frailty_index, everything())%>%
				dplyr::select(-c(age))%>%
				mutate(frailty_index = res.adj) %>%	
				dplyr::select(-c(res.adj))
    rm(list = c("res.adj","df.adj","hit_elnet"))
	}
	#3: Identify PERFECTLY COLLINEAR variable across all the response variables	
	print("Collinearity 0.9")
	
	df1 <- data %>% select_if(is.numeric) %>% dplyr::select(-target)
	hc <- findCorrelation(cor(df1), cutoff = 0.9, verbose = FALSE)
	hc = names(df1[,hc])[grep(pattern = 'imp|no_child|no_work', names(df1[,hc]))]
	
	data = data[,-which(names(data) %in% hc)]
	cat("\r", "Reduce data", dim(data))
	
	
	model1 <- lm(data[,sample(target)[1]] ~ ., data =data[,!(names(data) %in% c(target,id,"cciw","wave"))]) 
	
	#Which are the multicollinear predictors? 	
	
	xx <- model1$coefficients[is.na(model1$coefficients)][c(-1)]
	rm(model1)
	
	#they are the same for the five outcomes ?
	xx_na <- names(xx) %>% make_clean_names()
	xx_na <- gsub("_true","",xx_na)
	
	#clean names to have a map with current data
	#select only the non collinear variable
	print("Aliased coefficients")
	
	data <- data %>% dplyr::select(-starts_with(c(xx_na)),target,id,"cciw","wave")
	cat("\r", "Data minus aliased", dim(data))
	
	if(VIF == TRUE) {
	for(i in target) {
	print("Remove Variance Inflator Factor predictors")
	
	model2 <- lm(data[,i] ~ ., data = data[,!(names(data) %in% c(target,id,"cciw","wave"))])
	VIF <- car::vif(model2)
	data <- data[,-which(names(data) %in% names(which(VIF[,3] > 20)))]
	#Doeas the model improved
	model3 <- lm(data[,i] ~ ., data = data[,!(names(data) %in% c(i,id,"cciw","wave"))])
	rm(model2)
	rm(model3)
	}
	}
	#Save the data
	if (pooled == TRUE) {	
	#5: Save the output
	print("Save the output")
	
	dataframe[["pooled"]]= data
	}
	#Standardize the datasets
	# mydata_norm[[i]] <- as.data.frame(lapply(mydata[[i]][,-which(names(mydata[[i]]) %in% c(id,"cciw",i,"wave","country"))],arm::rescale,binary.inputs="0/1"))
	# mydata_norm[[i]] <- cbind(mydata[[i]][,which(names(mydata[[i]]) %in% c(id,"cciw",i,"country"))],mydata_norm[[i]])
	#almost zero variance predictors and perfectly collinear predictors 
	
	if (wave == TRUE) {
	#Creating dataset for w3 and w7
	wave3  <- data %>%
				filter(wave == "SL_3")%>%
				select(-c(wave))
	#SAVE W3
	dataframe[["wave3"]]= wave3
	
	wave7  <- data %>%
				filter(wave == "SL_7") %>%
				select(-c(wave))
	
	#SAVE W7
	dataframe[["wave7"]]= wave7
	}
	if (cnt == TRUE) {
	for (j in country)
	{ 
		if(j != "Germany")
		{
		mydata.country[[j]] <- data %>%
                filter(country == j)%>%
                dplyr::select(-c(country,west_germany))%>%
				mutate_if(is.character,as.factor) %>%
 				droplevels()%>%
			    remove_constant()
		}else
		{  
		mydata.country[[j]] <- data %>%
                filter(country == j)%>%
                dplyr::select(-c(country))%>%
				mutate_if(is.character,as.factor) %>%
 				droplevels()%>%
			    remove_constant()		
		}
	}
    dataframe[["cnt"]] <- mydata.country	
	}
 return(dataframe)
}

###dataprep

###This function creates the test and training data set on which to implement the ML models.
##From the cleaned data frame the data set provide three training and test sample, depending on the df of input. 
#Moreover, additional cleaning is made within the function after having created the model matrix. A matrix where 
#all variables have been converted into numeric values. We remove indeed variance inflator factor as well.
#Thus, by setting the option cat = T, the target variable are transformed into categorical feature (default option),
#otherwise they are treated as numeric continuous feature.
#All the predictor are then standardize.

dataprep <- function(
			df = NULL, 		#provide all the dataset in a list format
			test = NULL, 	#provide name for training/test sample
			target = NULL,  #provide names for response variable
			id = NULL,      #provide id identifier
			cciw = NULL ,   #provide cross sectional weights
			cat = T) {


traindf  <- list(pooled = data.frame(),#trainData : 80% pooled
                 wave3  = data.frame(),#trainData: wave3
                 wave7 = data.frame(),	#trainData: wave7 
				         cnt = data.frame()  #TrainData:country
				 )

traindf[["cnt"]] <- vector("list",length = length(country))
names(traindf[["cnt"]]) <- country			 

testdf <-  list(pooled = data.frame(),#test data: 20% pooled
				wave3 = data.frame(), #test data: wave 7
				wave7 = data.frame(),  #test data: wave 3
				cnt = data.frame() 
				)		
testdf[["cnt"]] <- vector("list",length = length(country))
names(testdf[["cnt"]]) <- country
	
for (i in target) {
	output_name <- c("lifesat","frailty_index","mental_index","sphus","happiness")
  t <- test
	# try(if(sum(!(test %in% names(df))) > 0) stop("Train not in data"))
	# 
	# try(if(sum(!(target %in% names(df[[t]]))) > 0) stop("target item is not present in data"))
    
	
	
	set.seed(101)
	try(if(!(i %in% names(df[[t]]))) stop("Target not in data"))
		
	#Create the data frame and rename the response variable
	if(t %in% c("pooled","wave3","wave7")){
	data <- df[[t]][sample(nrow(df[[t]])),] %>%
			dplyr::select(-c(output_name[which(!(output_name %in% target))]))%>%
			dplyr::rename(output = i)%>%
			dplyr::select(-target[target != i])%>%
	    dummy_cols(.,
	             c("country"
	              ),
	             remove_selected_columns = TRUE)%>% 
		  na.omit() %>% 
			droplevels()
    
	output <- data[,"output"]
	mergeid <- as.character(data[,id])
	weight <- data[,cciw]
	print("Create Model matrix")
	#cretaing the model matrix
	data <- model.matrix(output ~., data = data[,!(names(data) %in% c(id,cciw))],sep = "_")[,-1] %>% as.data.frame() %>% clean_names()
	cat("\r", "Model matrix dimension", dim(data))
	#Remove near zero variance predictor
	
	print("NZV predictors")
	x = nearZeroVar(data, saveMetrics = TRUE)
	X <- rownames(x[x[,"nzv"] > 0, ])
	if(length(X) > 0)
		data <- data[,-which(colnames(data) %in% X)]
	data <- cbind(mergeid,cciw = "weight", output, data)
	cat("\r", "Model matrix - NZV", dim(data))
	
	#Create categories for the responses:
		if(cat == TRUE) {
		print("Create categories")
		if(i == "frailty_index")
		
		#1 frailty: that takes the value of 1 if the frailty of individual ih higher than median frailty in overall sample and -1 otherwise
		data$output = ifelse(data$output > median(data$output),1,-1) 		
		
		#2 Create the dummy class for depression
		if(i == "mental_index")
			data$output <- ifelse(data $output >= 4, 1, 0)	
		
		#3 Create the classes for life satisfaction
		if(i == "lifesat")
			data  <- data  %>%
				mutate(output = case_when(
			grepl(c("0|1|2|3|4"), output)~ "1",
			grepl("5|6", output)~"2",
			grepl("7|8", output)~ "3",
			grepl("9|10", output) ~ "4",
			TRUE~ as.character(output)
			))			
	#Create the factor class for all the categorical/ordinal responses
		if (i %in% c(
			"frailty_index",
			"mental_index",
			"lifesat",
			"happiness",
			"sphus"))	
		data$output <- as.factor(data$output)
		}
	}
	if(t == "pooled"){
	#1- Create the partition: validation and test samples	
	
	trainRowNumbers <- createDataPartition(data[,"output"], p=0.7, list=FALSE)
	
	#2- Create the training  dataset: we remove the id identification and the cross sectional weight from the predictor set
	
	trainData <- data[trainRowNumbers,!(names(data) %in% c(target))]
	
	#3- normalizing the predictors but not the outcome, leave "0/1" as they are.
	
	trainData[,-which(names(trainData) %in% c(id,cciw,"output"))] <- as.data.frame(lapply(trainData[,-which(names(trainData) %in% c(id,cciw,"output"))],arm::rescale,binary.input = "0/1"))
	
	#4- Create the test dataset
	testData <- data[-trainRowNumbers,!(names(data) %in% c(target))]
	#5- Normalizing the test data
	testData[,-which(names(testData) %in% c(id,cciw,"output"))] <- as.data.frame(lapply(testData[,-which(names(testData) %in% c(id,cciw,"output"))],arm::rescale,binary.input = "0/1"))
	
	traindf[[t]] <- trainData
	testdf[[t]] <- testData
	
	}
	if(t == "wave3") {
	
	trainData <- data
	
	trainData[,-which(names(trainData) %in% c(id,cciw,"output"))] <- as.data.frame(lapply(trainData[,-which(names(trainData) %in% c(id,cciw,"output"))],arm::rescale,binary.input = "0/1"))
	
	# Step 3: Create the test dataset
	
	testData <- dataframe[["wave7"]]%>%
			dplyr::rename(output = i) %>%
			dplyr::select(-target[target != i])%>%
			dplyr::select(output, everything()) %>%
			na.omit() 
	
	try(if(!(id %in% names(testData))) stop("ID not in data"))
	test_mergeid <- testData[,id]
	
	try(if(!(cciw %in% names(testData))) stop("Cross sectional weight not in data"))
	test_cciw <- testData[,cciw]
	
	test_output <-  testData[,"output"]
	
	testData <- model.matrix(output ~., data = testData[,!(names(testData) %in% c(id,cciw))])[,-1] %>% as.data.frame()
	
	#selecting predictor in the two sets	
	testData <- testData[,names(testData) %in% names(trainData)]
	
	testData <- cbind("mergeid"= test_mergeid, "cciw" = test_cciw, output = test_output, testData)
	
	#Normalizing data (input-output) for neural network
	testData[,-which(names(testData) %in% c(id,cciw,"output"))] <- as.data.frame(lapply(testData[,-which(names(testData) %in% c(id,cciw,"output"))],arm::rescale,binary.input = "0/1"))
	
		if(cat == TRUE) {
			#1 Create the dummy class for frailty
			if(i == "frailty_index")
			testData$output <- ifelse(testData$output > median(testData$output),1,-1)
		
			#2 Create the dummy class for depression
		
			if(i == "mental_index")
			testData$output <- as.numeric(I(testData$output >= 3))
		
			#3 Create the class for lifesat
		
			if(i == "lifesat") {
			testData <- testData %>%
			mutate(output = case_when(
			grepl(c("0|1|2|3|4"), output)~ "1",
			grepl("5|6", output)~"2",
			grepl("7|8", output)~ "3",
			grepl("9|10", output) ~ "4",
			TRUE~ as.character(output)
			))
			}
	#Create the factor class for all the class response
		testData$output <- as.factor(testData$output)
		}		
	traindf[[t]] <- trainData
	testdf[[t]] <- testData
	}
	if(t == "wave7") {
	
	trainData <- data
	
	trainData[,-which(names(trainData) %in% c(id,cciw,"output"))] <- as.data.frame(lapply(trainData[,-which(names(trainData) %in% c(id,cciw,"output"))],arm::rescale,binary.input = "0/1"))
	
	# Step 3: Create the test dataset
	
	testData <- dataframe[["wave3"]]%>%
			dplyr::rename(output = i) %>%
			dplyr::select(-target[target != i])%>%
			dplyr::select(output, everything()) %>%
			na.omit() 
	
	try(if(!(id %in% testData)) stop("ID not in data"))
	test_mergeid <- testData[,id]
	
	try(if(!(cciw %in% testData)) stop("Cross sectional weight not in data"))
	test_cciw <- testData[,cciw]
	
	test_output <-  testData[,"output"]
	
	testData <- model.matrix(output ~., data = testData[,!(names(testData) %in% c(id,cciw))])[,-1] %>% as.data.frame()
	
	#selecting predictor in the two sets	
	testData <- testData[,names(testData) %in% names(trainData)]
	
	testData <- cbind("mergeid"= test_mergeid, "cciw" = test_cciw, output = test_output, testData)
	
	#Normalizing data (input-output) for neural network
	testData[,-which(names(testData) %in% c(id,cciw,"output"))] <- as.data.frame(lapply(testData[,-which(names(testData) %in% c(id,cciw,"output"))],arm::rescale,binary.input = "0/1"))
	
		if(cat == TRUE) {
			#1 Create the dummy class for frailty
			if(i == "frailty_index")
			testData$output <- ifelse(testData$output > median(testData$output),1,-1)
		
			#2 Create the dummy class for depression
		
			if(i == "mental_index")
			testData$output <- as.numeric(I(testData$output >= 3))
		
			#3 Create the class for lifesat
		
			if(i == "lifesat") {
			testData <- testData %>%
			mutate(output = case_when(
			grepl(c("0|1|2|3|4"), output)~ "1",
			grepl("5|6", output)~"2",
			grepl("7|8", output)~ "3",
			grepl("9|10", output) ~ "4",
			TRUE~ as.character(output)
			))
			}
	#Create the factor class for all the class response
		testData$output <- as.factor(testData$output)
		}		
	traindf[[t]] <- trainData
	testdf[[t]] <- testData
	}
	#make sure features names are right in the training and test data
	if (t %in% c( "pooled","wave3","wave7")){
		
		feature.names=names(traindf[[t]])
		
		for (f in feature.names){
		if (class(traindf[[t]][[f]])=="factor"){
		levels <- unique(sort(c(traindf[[t]][[f]])))
		traindf[[t]][[f]] <- factor(traindf[[t]][[f]],
                   labels= make.names(levels))
		colnames(traindf[[t]]) <- make.names(colnames(traindf[[t]]))
		}
		if (class(testdf[[t]][[f]])=="factor"){
			levels <- unique(sort(c(testdf[[t]][[f]])))
			testdf[[t]][[f]] <- factor(testdf[[t]][[f]],
                   labels= make.names(levels))
			colnames(testdf[[t]]) <- make.names(colnames(testdf[[t]]))
			}
		}
	}
	if(t == "cnt"){
	 country <- c(
	    "Austria",
	    "Belgium",
	    "Switzerland",
	    "Czech Republic",
	    "Germany",
	    "Denmark",
	    "Spain",
	    "France",
	    "Greece",
	    "Italy",
	    "Poland",
	    "Sweden")
	 for (j in country){
	 data <- df[[t]][[j]][sample(nrow(df[[t]][[j]])),] %>%
	    dplyr::select(-c(output_name[which(!(output_name %in% target))]))%>%
	    dplyr::rename(output = i)%>%
	    dplyr::select(-target[target != i])%>%
	    na.omit() %>% 
	    droplevels()
	  
	  output <- data[,"output"]
	  
	  mergeid <- as.character(data[,id])
	  weight <- data[,cciw]
	  print("Create Model matrix")
	  #creating the model matrix
	  data <- model.matrix(output ~., data = data[,!(names(data) %in% c(id,cciw))],sep = "_")[,-1] %>% as.data.frame() %>% clean_names()
	  cat("\r", "Model matrix dimension", dim(data))
	  #Remove near zero variance predictor
	  
	  print("NZV predictors")
	  x = nearZeroVar(data, saveMetrics = TRUE)
	  X <- rownames(x[x[,"nzv"] > 0, ])
	  if(length(X) > 0)
	    data <- data[,-which(colnames(data) %in% X)]
	  data <- cbind(mergeid,cciw = "weight", output, data)
	  cat("\r", "Model matrix - NZV", dim(data))
	  
	  #Create categories for the responses:
	  if(cat == TRUE) {
	    print("Create categories")
	    if(i == "frailty_index")
	      
	      #1 frailty: that takes the value of 1 if the frailty of individual ih higher than median frailty in overall sample and -1 otherwise
	      data$output = ifelse(data$output > median(data$output),1,-1) 		
	    
	    #2 Create the dummy class for depression
	    if(i == "mental_index")
	      data$output <- ifelse(data $output >= 4, 1, 0)	
	    
	    #3 Create the classes for life satisfaction
	    if(i == "lifesat")
	      data  <- data  %>%
	        mutate(output = case_when(
	          grepl(c("0|1|2|3|4"), output)~ "1",
	          grepl("5|6", output)~"2",
	          grepl("7|8", output)~ "3",
	          grepl("9|10", output) ~ "4",
	          TRUE~ as.character(output)
	        ))			
	    #Create the factor class for all the categorical/ordinal responses
	    if (i %in% c(
	      "frailty_index",
	      "mental_index",
	      "lifesat",
	      "happiness",
	      "sphus"))	
	      data$output <- as.factor(data$output)
	  }
	  trainRowNumbers <- createDataPartition(data[,"output"], p=0.7, list=FALSE)
	  
	  #2- Create the training  dataset: we remove the id identification and the cross sectional weight from the predictor set
	  
	  trainData <- data[trainRowNumbers,!(names(data) %in% c(target))]
	  
	  #3- normalizing the predictors but not the outcome, leave "0/1" as they are.
	  
	  trainData[,-which(names(trainData) %in% c(id,cciw,"output"))] <- as.data.frame(lapply(trainData[,-which(names(trainData) %in% c(id,cciw,"output"))],arm::rescale,binary.input = "0/1"))
	  
	  #4- Create the test dataset
	  testData <- data[-trainRowNumbers,!(names(data) %in% c(target))]
	  #5- Normalizing the test data
	  testData[,-which(names(testData) %in% c(id,cciw,"output"))] <- as.data.frame(lapply(testData[,-which(names(testData) %in% c(id,cciw,"output"))],arm::rescale,binary.input = "0/1"))
	  
	  traindf[[t]][[j]] <- trainData
	  testdf[[t]][[j]] <- testData
	  feature.names=names(traindf[[t]][[j]])
	  for (f in feature.names){
	    if (class(traindf[[t]][[j]][[f]])=="factor"){
	      levels <- unique(sort(c(traindf[[t]][[j]][[f]])))
	      traindf[[t]][[j]][[f]] <- factor(traindf[[t]][[j]][[f]],
	                                  labels= make.names(levels))
	      colnames(traindf[[t]][[j]]) <- make.names(colnames(traindf[[t]][[j]]))
	    }
	    if (class(testdf[[t]][[j]][[f]])=="factor"){
	      levels <- unique(sort(c(testdf[[t]][[j]][[f]])))
	      testdf[[t]][[j]][[f]] <- factor(testdf[[t]][[j]][[f]],
	                                 labels= make.names(levels))
	      colnames(testdf[[t]][[j]]) <- make.names(colnames(testdf[[t]][[j]]))
	    }
	  }
	 }
	}
}
return(list(traindf,testdf))
}
