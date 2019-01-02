############################################################
#
# DATA ANALYSIS TEXTBOOK
# MODEL SELECTION
# ILLUSTRATION STUDY
# Airbnb London 2017 march 05 data
#
############################################################  
#
# WHAT THIS CODES DOES:
#
# Descriptive statistics and regressions

install.packages("caret")
install.packages("skimr")
install.packages("ggthemes")


# Clear environment
rm( list = ls())
#import libraries
library(data.table)

library(caret)
library(tidyverse)
library(skimr)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library(glmnet)


# CHECK WORKING DIRECTORY - CHANGE IT TO YOUR WORKING DIRECTORY
#dir <-  "C:/Users/GB/Dropbox (MTA KRTK)/bekes_kezdi_textbook/"

#location folders
#data_in <- paste0(dir,"cases_studies_public/airbnb/clean/")
#data_out <- paste0(dir,"textbook_work/ch14/airbnb/")
#func <- paste0(dir, "textbook_work/ch00_tech_prep/")
#output <- paste0(dir,"textbook_work/ch14/airbnb/")
#graphs <- paste0(dir,"textbook_work/ch14/airbnb/")


#call function
#source(paste0(func, "theme_bg.R"))

# Created a helper function with some useful stuff
#source(paste0(func, "da_helper_functions.R")) 
#options(digits = 3) 





################################
## Some basic functions 2 use ##
################################

# Means Squared Error for log models
mse_log <- function (pred, y,corr){
  (mean((exp(y) - exp(pred) * exp(corr^2/2))^2, na.rm=T ))
}

# Means Squared Error for simple models
mse_lev <- function(pred,y)
{
  (mean((pred-y)^2, na.rm=T))
}

#############
# Load data #
#############

# Used area
area <- "hackney"
data <- read.csv(paste0("data/airbnb_hackney_workfile_adj.csv")
                 ,stringsAsFactors = T)


# Change Infinite values with NaNs
for (j in 1:ncol(data) ) set(data, which(is.infinite(data[[j]])), j, NA)

######################
# Quick look at data #
######################
glimpse(data)

summary(data$n_accommodates) #How does n_accomodates look like? 


#where do we have missing variables now?
to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

#what to do with missing values? just an example:
data <- data %>%
  mutate(n_bathrooms =  ifelse(is.na(n_bathrooms), median(n_bathrooms, na.rm = T), n_bathrooms), #assume at least 1 bath
         n_beds = ifelse(is.na(n_beds), n_accommodates, n_beds)) #assume n_beds=n_accomodates


#what are those factor variables?
factor_cols <- data %>% Filter(f = is.factor) %>% names()
data %>%
  select(factor_cols) %>%
  skim()

#to_drop <- c("neighbourhood_cleansed", "f_neighbourhood_cleansed")
#data <- data %>%
#  select(-one_of(to_drop))

###################################
# Business logic- define our prediction problem
###################################

# Decision 1
# Size, we need a normal apartment
data <- data %>% 
  filter(n_accommodates < 8)

# Decision 2
# Remove missing data, that has no score rating
data %>%
  select(n_review_scores_rating) %>%
  is.na() %>%
  sum()
data <- data %>% 
  drop_na(n_review_scores_rating)





###################################
# Look at some descr stat
###################################



#How is the average price changing in my district by `property_type`, `room_type` and the `bed_type`? 
data %>%
  group_by(f_property_type, f_room_type) %>%
  summarize(Mean = mean(price, na.rm=TRUE))

data %>%
  group_by(f_bed_type) %>%
  summarize(Mean = mean(price, na.rm=TRUE))

#How does the price range look like?
## Distribution of price by type

plot1 <- ggplot(data, aes(price, fill = f_room_type)) + 
  ggtitle("Price Density, Hackney") +
  geom_density(alpha = 0.3) + theme_fivethirtyeight()

## Boxplot of price by room type
plot2 <- ggplot(data, aes(f_room_type, price)) + 
  ggtitle("Price in Hackney") +
  geom_boxplot()+ theme_fivethirtyeight()
grid.arrange(plot1, plot2, ncol=2, nrow=1)


plot5 <- ggplot(data, aes(x = factor(n_accommodates), y = price, fill = factor(f_property_type))) +
  geom_boxplot(alpha=0.8) +
  scale_x_discrete(name = "Accomodate Persons") +
  ggtitle("Price Per Accommodate in Hackney") + theme_fivethirtyeight()
plot6 <- ggplot(data = data) +
  geom_bar(data = data,
           aes(x = factor(n_accommodates),
               color = f_room_type, fill = f_room_type)) +
  ggtitle("# Accomodations and property types Hackney") +
  xlab('Accommodates') + theme_fivethirtyeight()
grid.arrange(plot5, plot6, nrow=2)


#####################
# Setting up models #
#####################

# Basic Variables
basic_lev  <- c("n_accommodates", "n_beds", "f_property_type", "f_room_type", "n_days_since") 
basic_log <- c("ln_accommodates", "ln_beds", "f_property_type", "f_room_type","ln_days_since") 

# Factorized variables
basic_add <- c("f_bathroom","f_cancellation_policy","f_bed_type") 
reviews <- c("f_number_of_reviews","n_review_scores_rating") 
# Higher orders
poly_lev <- c("n_accommodates2", "n_days_since2", "n_days_since2")
poly_log <- c("ln_accommodates2","ln_days_since2","ln_days_since3")

#not use p_host_response_rate due to missing obs

# Dummy variables: Extras -> collect all options and create dummies
amenities <-  names(data)[grep("^d_.*",names(data))] 


#################################################
# Look for interactions
################################################
# Looking for interactions.
# It is a function it takes 3 arguments: 1) Your dataframe, 
# 2) the factor variable (like room_type) 
# 3)the dummy variable you are interested in (like TV)

Make_Interaction_Plot <- function(df, FactorVar, DummyVar){
  #Process your data frame and make a new dataframe which contains the stats
  require("dplyr")
  require("ggplot2")
  FactorVar <- as.name(FactorVar)
  DummyVar <- as.name(DummyVar)
  
  stats <- df %>%
    group_by_(FactorVar, DummyVar) %>%
    summarize(Mean = mean(price, na.rm=TRUE),
              se = sd(price)/sqrt(n()))
  
  stats[,2] <- lapply(stats[,2], factor)
  
  #create your custom plot style
  dodge = position_dodge(width=0.9)
  apatheme=theme_bw()+
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.border=element_blank(),
          axis.line=element_line())  
  
  #Plot it!
  p1 <- ggplot(stats, aes_string(colnames(stats)[1], colnames(stats)[3], fill = colnames(stats)[2]))+
    geom_bar(stat='identity', position=dodge)+
    geom_errorbar(aes(ymin=Mean-(1.96*se),ymax=Mean+(1.96*se)) , position=dodge, width=0.25)+
    ylab('Mean Price')+
    apatheme +
    scale_fill_grey()  
  
  return(p1)
}

#Look up room type interactions
p1 <- Make_Interaction_Plot(data, "f_room_type", "d_familykidfriendly")
p2 <- Make_Interaction_Plot(data, "f_room_type", "f_property_type")
#Look up canelation policy
p3 <- Make_Interaction_Plot(data, "f_cancellation_policy", "d_familykidfriendly")
p4 <- Make_Interaction_Plot(data, "f_cancellation_policy", "d_tv")
#Look up property type
p5 <- Make_Interaction_Plot(data, "f_property_type", "d_cats")
p6 <- Make_Interaction_Plot(data, "f_property_type", "d_dogs")
p7 <- Make_Interaction_Plot(data, "f_property_type", "d_dryer")
p8 <- Make_Interaction_Plot(data, "f_property_type", "d_smokedetector")

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, ncol=2, nrow=4)

# dummies suggested by graphs
X1  <- c("f_room_type*f_property_type",  "f_room_type*d_familykidfriendly") 

# Additional interactions of factors and dummies
X2  <- c("d_airconditioning*f_property_type", "d_cats*f_property_type", "d_dogs*f_property_type") 
X3  <- c(paste0("(f_property_type + f_room_type + f_cancellation_policy + f_bed_type) * (",
                paste(names(data)[19:67],collapse=" + "),")"))

# Create models in levels models: 1-8
modellev1 <- " ~ n_accommodates"
modellev2 <- paste0(" ~ ",paste(basic_lev,collapse = " + "))
modellev3 <- paste0(" ~ ",paste(c(basic_lev, basic_add,reviews),collapse = " + ")) 
modellev4 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,poly_lev),collapse = " + "))
modellev5 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,poly_lev,X1),collapse = " + "))
modellev6 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,poly_lev,X1,X2),collapse = " + "))
modellev7 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,poly_lev,X1,X2,amenities),collapse = " + "))
modellev8 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,poly_lev,X1,X2,amenities,X3),collapse = " + "))

# Create models in logs, models: 1-8
modellog1 <- " ~ ln_accommodates"
modellog2 <- paste0(" ~ ",paste(basic_log,collapse = " + "))
modellog3 <- paste0(" ~ ",paste(c(basic_log, basic_add),collapse = " + ")) 
modellog4 <- paste0(" ~ ",paste(c(basic_log,basic_add,reviews,poly_log),collapse = " + "))
modellog5 <- paste0(" ~ ",paste(c(basic_log,basic_add,reviews,poly_log,X1),collapse = " + "))
modellog6 <- paste0(" ~ ",paste(c(basic_log,basic_add,reviews,poly_log,X1,X2),collapse = " + "))
modellog7 <- paste0(" ~ ",paste(c(basic_log,basic_add,reviews,poly_log,X1,X2,amenities),collapse = " + "))
modellog8 <- paste0(" ~ ",paste(c(basic_log,basic_add,reviews,poly_log,X1,X2,amenities,X3),collapse = " + "))


#################################
# Create test and train samples #
#################################

# create test and train samples (90% of observations in train sample)
smp_size <- floor(0.9 * nrow(data))

# Set the random number generator: It will make results reproducable
set.seed(20180122)

# create ids: 
# 1) seq_len: generate regular sequences
# 2) sample: select random rows from a table
train_ids <- sample(seq_len(nrow(data)), size = smp_size)

# create a new variable for train/test sample
data$train <- 0
data$train[train_ids] <- 1
# Create train and test sample variables

#Training Set
data_train <- data %>%
  filter(train == 1)

#Test Set
data_test <- data %>%
  filter(train == 0)

####################################
#         Compare models           #
####################################

#mse_lev(prediction_train, data_train[,"price"])

# Create list to save model results
model_results <- list()

# For each level and logs
for (type in c("lev","log")) {
  # for each model
  for (i in ( 1 : 8 ) ) {
    
    # Get the proper model names
    model_name <- paste0("model",type,i)
    # Get the proper target variable
    yvar <- ifelse(type=="lev","price","ln_price")
    # Get the depedent variables
    xvars <- eval(parse(text = model_name))
    # Create the appropriate formula
    formula <- formula(paste0(yvar,xvars))
    # Estimate on the training data
    model <- lm(formula,data = data_train)
    # Predict on the training sample (in-sample)
    prediction_train <- predict(model, newdata = data_train)
    # Predict on the testing sample (out-of--sample)
    prediction_test <- predict(model, newdata = data_test)
    
    # Estimate the appropriate Criteria
    if (type=="lev") {
      mse_train <- mse_lev(prediction_train, data_train[,yvar])
      mse_test <- mse_lev(prediction_test, data_test[,yvar])
    } else {
      rmselog <- mse_lev(prediction_train, data_train[,yvar])**(1/2)
      mse_train <- mse_log(prediction_train, data_train[,yvar],rmselog)
      mse_test <- mse_log(prediction_test, data_test[,yvar],rmselog)
    }
    # Bayesian Criteria
    BIC <- BIC(model)
    # Save into model results
    model_results[[model_name]] <- list(yvar=yvar,xvars=xvars,formula=formula,model=model,
                                        prediction_train = prediction_train,prediction_test = prediction_test,
                                        mse_train = mse_train,mse_test = mse_test,BIC = BIC)
  }
}

## Example for levels:
vals <- matrix(rep(NaN,3*8),nrow=3,ncol=8)
for ( modelNum in 1 : 8 ){
  for ( crit in c("mse_train","mse_test","BIC") ){
    if ( modelNum == 1 ) dt <- model_results$modellev1
    if ( modelNum == 2 ) dt <- model_results$modellev2
    if ( modelNum == 3 ) dt <- model_results$modellev3
    if ( modelNum == 4 ) dt <- model_results$modellev4
    if ( modelNum == 5 ) dt <- model_results$modellev5
    if ( modelNum == 6 ) dt <- model_results$modellev6
    if ( modelNum == 7 ) dt <- model_results$modellev7
    if ( modelNum == 8 ) dt <- model_results$modellev8
    if ( crit == "mse_train" ) i <- 1 
    vals[i,modelNum] <- dt$mse_train
    if ( crit == "mse_test" ) i <- 2
    vals[i,modelNum] <- dt$mse_test
    if ( crit == "BIC" ) i <- 3
    vals[i,modelNum] <- dt$BIC
  }
}
vals

##############################
#      cross validation      #
##############################

## K/N = 10
n_folds=10
# Create the folds
folds_i <- sample(rep(1:n_folds, length.out = nrow(data) ))
# Create results
model_results_cv <- list()

for (type in c("lev","log")) {
  for (i in (1:8)){
    model_name <- paste0("model",type,i)
    
    yvar <- ifelse(type=="lev","price","ln_price")
    xvars <- eval(parse(text = model_name))
    formula <- formula(paste0(yvar,xvars))
    
    # Initialize values
    rmse_train <- c()
    rmse_train<- c()
    BIC<- c()
    
    # Do the k-fold estimation
    for (k in 1:n_folds) {
      test_i <- which(folds_i == k)
      # Train sample: all except test_i
      data_train <- data[-test_i, ]
      # Test sample
      data_test <- data[test_i, ]
      # Estimation and prediction
      model <- lm(formula,data = data_train)
      prediction_train <- predict(model, newdata = data_train)
      prediction_test <- predict(model, newdata = data_test)
      
      # Criteria evaluation
      if (type=="lev") {
        mse_train[k] <- mse_lev(prediction_train, data_train[,yvar])
        mse_test[k] <- mse_lev(prediction_test, data_test[,yvar])
      } else {
        rmselog <- mse_lev(prediction_train, data_train[,yvar])**(1/2)
        mse_train[k] <- mse_log(prediction_train, data_train[,yvar],rmselog)
        mse_test[k] <- mse_log(prediction_test, data_test[,yvar],rmselog)
      }
      
      BIC[k] <- BIC(model)
    }
    
    model_results_cv[[model_name]] <- list(yvar=yvar,xvars=xvars,formula=formula,model=model,
                                           prediction_train = prediction_train,prediction_test = prediction_test,
                                           mse_train = mse_train,mse_test = mse_test,BIC = BIC)
  }
}

## Example for levels:
vals_CV <- matrix(rep(NaN,3*8),nrow=3,ncol=8)
for ( modelNum in 1 : 8 ){
  for ( crit in c("mse_train","mse_test","BIC") ){
    if ( modelNum == 1 ) dt <- model_results_cv$modellev1
    if ( modelNum == 2 ) dt <- model_results_cv$modellev2
    if ( modelNum == 3 ) dt <- model_results_cv$modellev3
    if ( modelNum == 4 ) dt <- model_results_cv$modellev4
    if ( modelNum == 5 ) dt <- model_results_cv$modellev5
    if ( modelNum == 6 ) dt <- model_results_cv$modellev6
    if ( modelNum == 7 ) dt <- model_results_cv$modellev7
    if ( modelNum == 8 ) dt <- model_results_cv$modellev8
    if ( crit == "mse_train" ) i <- 1 
    vals_CV[i,modelNum] <- mean( dt$mse_train )
    if ( crit == "mse_test" ) i <- 2
    vals_CV[i,modelNum] <- mean( dt$mse_test )
    if ( crit == "BIC" ) i <- 3
    vals_CV[i,modelNum] <- mean( dt$BIC )
  }
}
vals_CV


#################################
#           LASSO               #
#################################

#modelLasso <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,amenities),collapse = " + "))

formula <- formula(paste0("price", modellev7))
x <- model.matrix(formula,data_train)

set.seed(42)
model_lasso_1 <- cv.glmnet(x, data_train$price, alpha = 1)
#MSE scores of Lasso:
bestlamd <- model_lasso_1$lambda.min

#pred on training set
newx <- model.matrix(formula, data_train) 
model_lasso_1.pred <- predict(model_lasso_1, newx = newx, s=bestlamd)
model_lasso_1.mseCV <- mse_lev(model_lasso_1.pred, data_train$price)
model_lasso_1.mseCV

#pred on testing set
newx <- model.matrix(formula, data_test) 
model_lasso_1.pred <- predict(model_lasso_1, newx = newx, s=bestlamd)
model_lasso_1.mseTest <- mse_lev(model_lasso_1.pred, data_test$price)
model_lasso_1.mseTest

###################################################
# Post Lasso OLS #
###################################################  
# keep vars from lasso, do OLS. show predicted mse

'%ni%' <- Negate('%in%')

#What are the variables of the lasso model we created?
c <- coef(model_lasso_1, s='lambda.min', exact=TRUE) #get the one with min score to extract its variables
inds <- which(c!=0)
variables <- row.names(c)[inds]
variables <- variables[variables %ni% '(Intercept)']

variables

#We want to use these variable names coming from lasso in a linear model!
#LV stands for Lasso Variables
LV = c()
for (i in variables) { #for all the variables in this lasso
  for (j in names(data)) { #for all the variables in the original df
    if (substring(i, 1, 10) == substring(j, 1, 10)) { #if the first 10chars are equal 
      if (!(j %in% LV)) { # if it is not already in the list
        LV <- c(LV, j) #then use the name in the original df
      }
    }
  }
}
LV

LassoVars <- paste(LV, collapse="+") 

fit_control <- trainControl(method = "cv", number = 10)
set.seed(42)
model_PostLassoOLS <- train(as.formula(paste("price ~ ", LassoVars)), 
                                  data = data_train, 
                                  method = "lm", 
                                  trControl = fit_control)

PostLassoMSEcv <- round((model_PostLassoOLS$results[["RMSE"]])^2,2)
PostLassoMSEcv #MSE on CV

PostLassoPredtest <- predict(model_PostLassoOLS, newdata = data_test)
PostLassoMSEtest <- round(mse_lev(pred = PostLassoPredtest, y = data_test$price),2)
PostLassoMSEtest

#Compare it here with the previous test set scores:

result_table <- as.data.frame(vals_CV) %>%
  select(V4,V5,V6,V7,V8) %>%
  rename(modellevel4 = V4,
         modellevel5 = V5,
         modellevel6 = V6,
         modellevel7 = V7,
         modellevel8 = V8) %>%
  mutate(lasso = c(model_lasso_1.mseCV, model_lasso_1.mseTest, NA),
         postlasso = c(PostLassoMSEcv, PostLassoMSEtest, NA)) %>%
  round(2)
  
row.names(result_table) <- c("CV", "Test", "BIC")
result_table



###################################################
# FIGURES FOR FITTED VS ACTUAL OOUTCOME VARIABLES #
###################################################  

# Choose model 7  
m <-7
# Get test sample logicals
test_i <- which(folds_i == 5)
data_train <- data[-test_i, ]
# Test sample
data_test <- data[test_i, ]
# Target variable
Ylev <- data_test[,price]
meanY <-mean(Ylev)
sdY <- sd(Ylev)
meanY_m2SE= meanY -2* sdY 
meanY_p2SE= meanY +2* sdY 
Y5p = quantile(Ylev,0.05)
Y95p = quantile(Ylev,0.95)

# Logged target variable
Ylog <- data_test[,ln_price]

## Model 1) modellev7 on price
formula <- formula(paste0("price",modellev7))
modellev <- lm(formula,data = data_train)
# Predicted values
predictionlev_test <- predict(modellev, newdata = data_test)

## Model 2) modellog7 on log price
formula <- formula(paste0("ln_price",modellog7))
modellog <- lm(formula,data = data_train)
predictionlog_test <- predict(modellog, newdata = data_test)
rmselog <- mse_lev(predict(modellog, newdata = data_train),data_train[,ln_price])**(1/2)
# Predicted values
predictionlog_test2 <- exp(predictionlog_test) * exp((rmselog)^2/2)

# Create data frame with the real and predicted values
d <- data.frame( ylev=Ylev , ylog=Ylog , 
                 predlev=predictionlev_test , predlog=predictionlog_test2)
# Check the differences
d$elev <- d$ylev - d$predlev

# Plot the Level and Log Prediction less than 400
ggplot(data = d[(d$ylev<400),], aes(x=ylev, y=predlev)) +
  geom_point(size=2, colour="orange",shape=4)+
  geom_point(aes(x=ylev, y=predlog), size=2, colour="blue",shape=4)+
  geom_smooth(method = "lm", se = F, colour="orange") +
  geom_smooth(aes(x=ylev, y=predlog),method = "lm", se = F, colour="blue") +
  theme_bw()
#ggsave("log_vs_lin_all.png")

# Plot the Level and Log Prediction within 0.5% and 95%
ggplot(data = d[(d$ylev>Y5p) & (d$ylev<Y95p),], aes(x=ylev, y=predlev)) +
  geom_point(size=2, colour="orange",shape=4)+
  geom_point(aes(x=ylev, y=predlog), size=2, colour="blue",shape=4)+
  geom_smooth(method = "lm", se = F, colour="orange") +
  geom_smooth(aes(x=ylev, y=predlog),method = "lm", se = F, colour="blue") +
  theme_bw()
#ggsave("log_vs_lin_95.png")

# Plot the log aginst the 45 line
ggplot(data = d[(d$ylev>Y5p) & (d$ylev<Y95p),], aes(x=ylev, y=predlev)) +
  geom_point(size=2, colour="orange",shape=4)+
  geom_smooth(method = "lm", se = F) +
  geom_smooth(aes(x=ylev,y=ylev),method = "lm", se = F, color="black") +
  theme_bw()
#ggsave("log_vs_45.png")

# Level prediction against the errors
ggplot(data = d, aes(x=ylev, y=elev)) +
  geom_point(size=2, colour="orange",shape=4)+
  geom_smooth(method = "lm", se = F) +
  theme_bw()
#ggsave("F14_preerr1.png")