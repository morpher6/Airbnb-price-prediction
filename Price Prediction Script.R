### Process: ###
# - Treat NA values
# - Cleanse Data and transform
# - EDA and Histograms
# - fit initial model
# - feature engineering
# - More modeling


### Step 1:  Load libraries  ###

debug(utils:::unpackPkgZip) #get past firewall 

library(randomForest)
library(magrittr)
library(dplyr)
library(lubridate)
library(zipcode)
library(ggplot2)
library(randomForest)
library(Metrics)
library(caret)
library(glmnet)
library(iterators)
library(parallel)
library(xgboost)
library(zoo)
library(corrplot)
library(mlr)

### Load data ###

path = "C:/Users/spnelson/SF/Personal Folders/Airbnb/"
train <- read.csv(paste0(path,"trainCleanZip.csv"), header = T, stringsAsFactors = F)
test <- read.csv(paste0(path,"test.csv"), header = T, stringsAsFactors = F)

### Save the ID column so that we can drop it from merged dataset (combi) ###

train_ID = train$id
test_ID = test$id
test$log_price <- NA

test$isTest <- rep(1,nrow(test))
train$isTest <- rep(0,nrow(train))
fullSet <- rbind(train,test)

# test.new <- fullSet[fullSet$isTest==1,]
# train.new <- fullSet[fullSet$isTest==0,]



sapply(X = fullSet, FUN = function(x) sum(is.na(x))) # number of missing values by column


### Exploratory Questions ###

# - Break up amenities into dummy vars?
# - Figure out variables to remove: We could probably remove picture URL 
# - how do we handle missing values?
# - should we remove description because it adds little value?
# - do amenities affect price? If so, binary values can be used by creating one column per amenity (see code below)


### Data transformation ###

### Cleanse Data ###

# convert cleaning fee to boolean
fullSet$cleaning_fee <- as.integer(as.logical((fullSet$cleaning_fee)))
# convert host_has profile pic to boolean 
fullSet$host_has_profile_pic = ifelse(fullSet$host_has_profile_pic =="t",1,0)
# convert host_identity_verified to boolean 
fullSet$host_identity_verified = ifelse(fullSet$host_identity_verified =="t",1,0)
# convert instant_bookable to boolean CHANGE
fullSet$instant_bookable = ifelse(fullSet$instant_bookable =="t",1,0)
fullSet$number_of_reviews <- as.numeric(fullSet$number_of_reviews )

### transform data ###

# -do amenities effect price? If so, binary values can be used by creating one column per amenity (see code below)

#remove extra characters
drops <- c("description", "first_review", "host_since", "last_review", "name", "thumbnail_url")
full_sub<-fullSet[,!(names(fullSet) %in% drops)] 


amenities<-strsplit(fullSet$amenities, ",")
amenities<-unlist(amenities)
amenities<-gsub("[{]","", amenities)
amenities<-gsub("[}]","", amenities)
amenities<-gsub("[^A-Za-z0-9,;._-]"," ", amenities)
amenities<-trimws(amenities)


#create a data frame with new attributes
col_name<-names(sort(table(amenities), decreasing = T))
col_name[c(17, 20, 23, 26, 40, 54, 67, 68, 75, 89, 103, 110, 115, 121)]<- c("Family/kid friendly", "translation missing: en.hosting_amenity_50", "translation missing: en.hosting_amenity_49", 
                                                                            "Buzzer/wireless intercom", "Dog(s)", "Cat(s)", "Children????Ts books and toys", "Pack ????Tn Play/travel crib", 
                                                                            "Children????Ts dinnerware", "Other pet(s)", "Wide clearance to shower & toilet", "Fixed grab bars for shower & toilet",
                                                                            "Washer,Dryer", "Ski in/Ski out")
col_name<-trimws(col_name)

temp_df<-data.frame(matrix(ncol = 131, nrow = nrow(fullSet)))
colnames(temp_df) <- col_name
fullSet<-cbind(full_sub, temp_df)

#populate binary values for new amenities columns
for(i in 24:ncol(fullSet)){
  rownums <- grep(names(fullSet)[i], fullSet$amenities)
  fullSet[rownums,i]<- 1
}

#sort(sapply(train_df, FUN = function(x) sum(is.na(x))))
fullSet[,24:ncol(fullSet)][is.na(fullSet[,24:ncol(fullSet)])]<-0



### Treating NAs ###

# fill missing zip codes using the 'zipcode' package and joining the data:
# data("zipcode")
# str(zipcode)
# zipcode$zip <- as.numeric(zipcode$zip)
# train_df$zipcode <- as.numeric(train_df$zipcode)
# 
# trainzip <- train_df[,c("latitude", "longitude", "zipcode", "city")]
# trainzipna <- trainzip[is.na(trainzip$zipcode),] #keep just na zips
# new_zip <- left_join(trainzipna, zipcode, c('latitude' = 'latitude', 'longitude' = 'longitude'))
#write.csv(new_zip, "new_zip.csv")


#used google reverse geocode to impute missing zip codes:
# path2 <- "~/Airbnb-price-prediction/"
# new_zip2 <- read.csv(paste0(path2,"new_zip2.csv"), header = T, stringsAsFactors = F)

# #round lat and long to make merge effective
# new_zip2$latitude<-round(new_zip2$latitude, 5)
# new_zip2$longitude<-round(new_zip2$longitude, 5)
# train_df$latitude<-round(train_df$latitude, 5)
# train_df$longitude<-round(train_df$longitude, 5)
# train_merge<- merge(train_df, new_zip2, by = c("latitude", "longitude"), all.x = TRUE)
# 
# #replace NA values in zipcode with valid values from Zip
# train_merge$Zip<-as.numeric(as.character(train_merge$Zip))
# train_merge$zipcode<-as.numeric(as.character(train_merge$zipcode))
# for(i in 1:nrow(train_merge)){
#   train_merge[i,21]<-ifelse(is.na(train_merge[i,21])==TRUE, train_merge[i,157], train_merge[i,21])
# }


#train_df$zipcode <- substr(train_df$zipcode, 0, 5) # make zipcode trimmed to 5 digits only

#fix some columns:
# train_merge <- train_merge[ , -which(names(train_merge) %in% c("city.y","Zip", "Address"))] #remove unnecessary columns
# names(train_merge)[names(train_merge) == 'city.x'] <- 'city'
# train_df <- train_merge



# fill in missing value by the median
# for bathrooms, bedrooms, beds, accommodates


fullSet$bathrooms <- na.aggregate(fullSet$bathrooms, FUN = median)
fullSet$bedrooms <- na.aggregate(fullSet$bedrooms, FUN = median)
fullSet$beds <- na.aggregate(fullSet$beds, FUN = median)
fullSet$accommodates <- na.aggregate(fullSet$accommodates, FUN = median)



# Fix reviews_scores_rating by making it categorical and binning

fullSet$review_scores_rating <- as.numeric(fullSet$review_scores_rating)
fullSet$ReviewCategory <- fullSet$review_scores_rating %>% cut(fullSet$review_scores_rating, breaks=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 85, 90, 95, Inf),
                                                               labels=c("0-9","10-19","20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-84", "85-89", "90-94", "95-100"),
                                                               include.lowest = TRUE,
                                                               right = FALSE)


fullSet$ReviewCategory <- as.character(fullSet$ReviewCategory)
fullSet$ReviewCategory[is.na(fullSet$ReviewCategory)] <- "No Reviews" # turn NaN scores with 0 reviews into 'No Reviews'
fullSet$review_scores_rating <- as.numeric(fullSet$review_scores_rating)
fullSet$review_scores_rating[is.na(fullSet$review_scores_rating)] <- 0
fullSet$number_of_reviews[is.na(fullSet$number_of_reviews)] <- 0
fullSet$cleaning_fee[is.na(fullSet$cleaning_fee)] <- 0
fullSet$ReviewCategory <- as.factor(fullSet$ReviewCategory)

### Variables to drop ###
# amenities, latitude, longitude, neighborhood, diff_first_last_review, review_score_category
fullSet <- fullSet[ , -which(names(fullSet) %in% c("latitude", "longitude", "neighbourhood", 'amenities', 'host_response_rate'))] 


### EDA and Histograms ###

table(fullSet$review_scores_rating)
table(fullSet$property_type)
table(fullSet$room_type)
table(fullSet$city)


# visualize distribution of log price (target variable)
p1 <- ggplot(fullSet) 
p1 + geom_histogram(aes(x = log_price))

#visualize beds
p1 + geom_bar(aes(x = beds))

#visualize 
p1 + geom_bar(aes(review_scores_rating))
p1 + geom_bar(aes(bathrooms))
p1 + geom_bar(aes(bedrooms))
p1 + geom_bar(aes(beds))
p1 + geom_bar(aes(accommodates))
p1 + geom_bar(aes(room_type))
p1 + geom_bar(aes(cancellation_policy))
p1 + geom_bar(aes(cleaning_fee))
p1 + geom_bar(aes(city))




### Model Running ###

#Initial Linear model
# train_df_small <-  sample_frac(train.new, size = .2, replace = FALSE) # for running on smaller subsets
test.lm <- fullSet[fullSet$isTest==1,]
train.lm <- fullSet[fullSet$isTest==0,]
train.lm$log_price <- na.aggregate(train.lm$log_price, FUN = mean)

set.seed(222)

inTrain <- createDataPartition(y = train.lm$log_price, p = 0.7, list = FALSE)
Training <- train.lm[inTrain, ]
Validation <- train.lm[-inTrain, ]
training <- train.lm
testing <- train.lm

lm_fit_1 <- lm(log_price ~ bathrooms + bedrooms + beds + 
                 #property_type + 
                 room_type  + accommodates +
                 bed_type + cancellation_policy + cleaning_fee + host_has_profile_pic + host_identity_verified + instant_bookable +
                 number_of_reviews + ReviewCategory, data=Training)
summary(lm_fit_1)

preds_lm_1 <- predict(lm_fit_1, Validation[, -2])
rmse(Validation$log_price, preds_lm_1) #rmse = .4866
df_lm_1 <- data.frame(id = test_ID, log_price = preds_lm_1) #have to do this on full dataset, but prediction is not good enough


### random forest model ###

fullRF <- fullSet

# have to convert to factor since RF only takes numeric and factors (no char)

fullRF$property_type <- as.factor(fullRF$property_type)
fullRF$room_type <- as.factor(fullRF$room_type)
fullRF$bed_type <- as.factor(fullRF$bed_type)
fullRF$cancellation_policy <- as.factor(fullRF$cancellation_policy)
fullRF$city <- as.factor(fullRF$city)
fullRF$zipcode <- as.factor(fullRF$zipcode)


test.rf <- fullRF[fullRF$isTest==1,]
train.rf <- fullRF[fullRF$isTest==0,]
train.rf$log_price <- na.aggregate(train.rf$log_price, FUN = mean)

set.seed(222)

inTrain <- createDataPartition(y = train.rf$log_price, p = 0.7, list = FALSE)
Training <- train.rf[inTrain, ]
Validation <- train.rf[-inTrain, ]
training <- train.rf
testing <- test.rf


# Create a random forest with 1000 trees

rf_fit_1 <- randomForest(log_price ~ bathrooms + bedrooms + beds + property_type + room_type  + accommodates +
                           bed_type + cancellation_policy + cleaning_fee + host_has_profile_pic + host_identity_verified + instant_bookable +
                           number_of_reviews + ReviewCategory, data = Training, importance = TRUE, ntree=1000)


# How many trees are needed to reach the minimum error estimate? 
which.min(rf_fit_1$mse) #980
# Using the importance()  function to calculate the importance of each variable
imp <- as.data.frame(sort(importance(rf_fit_1)[,1],decreasing = TRUE),optional = T)
names(imp) <- "% Inc MSE"
imp

# most important are:

# % Inc MSE
# room_type              278.323545
# bathrooms              164.443585
# property_type          161.049953
# cancellation_policy    119.134574
# accommodates           119.095396
# cleaning_fee            92.223927
# beds                    85.948142
# bedrooms                72.231796
# ReviewCategory          70.574991
# number_of_reviews       62.982561
# instant_bookable        56.617034
# host_identity_verified  44.851922
# bed_type                35.708424
# host_has_profile_pic    -3.852709

# As usual, predict and evaluate on the test set
pred_rf_1 <- predict(rf_fit_1,Validation[,-2]) #make sure log_price is taken out
RMSE.forest <- rmse(Validation$log_price, pred_rf_1)
RMSE.forest #rmse is .47



df_rf_1 <- data.frame(id = ids, log_price = pred_rf_1)


### Dummy encoding for XGBoost ###

# first get data type for each feature
feature_classes <- sapply(names(fullSet), function(x) {
  class(fullSet[[x]])
})
fullSet$id <- as.numeric(fullSet$id)
fullSet$accommodates <- as.numeric(fullSet$accommodates)
numeric_feats <- names(feature_classes[feature_classes == "numeric"])

# get names of categorical features
categorical_feats <- names(feature_classes[feature_classes == "character"])

# use caret dummyVars function for hot one encoding for categorical
# features

dummies <- dummyVars(~., fullSet[categorical_feats])
categorical_1_hot <- predict(dummies, fullSet[categorical_feats])


fullSet <- cbind(fullSet[numeric_feats], categorical_1_hot)

test.xg <- fullSet[fullSet$isTest==1,]
train.xg <- fullSet[fullSet$isTest==0,]
train.xg$log_price <- na.aggregate(train.xg$log_price, FUN = mean)

set.seed(222)

inTrain <- createDataPartition(y = train.xg$log_price, p = 0.7, list = FALSE)
Training <- train.xg[inTrain, ]
Validation <- train.xg[-inTrain, ]
training <- train.xg
testing <- test.xg


### LASSO Model ###


set.seed(123)
cv_lasso = cv.glmnet(as.matrix(Training[, -1]), Training[, 1])

## Predictions
pred_lasso_1 <- predict(cv_lasso, newx = as.matrix(Validation[, -1]), s = "lambda.min")
rmse(Validation$log_price, pred_lasso_1)

df_lasso_1 <- data.frame(id = test_ID, log_price = pred_lasso_1)

### Ridge Model ###
set.seed(123)
cv_ridge = cv.glmnet(as.matrix(Training[, -1]), Training[, 1],family = "gaussian", alpha = 0)

## Predictions
pred_ridge_1 <- predict(cv_ridge, newx = as.matrix(Validation[, -1]), s = "lambda.min")

rmse(Validation$log_price, pred_ridge_1)

df_ridge_1 <- data.frame(id = test_ID, log_price = pred_ridge_1)

### Elastic Net ###

cv_elnet <- cv.glmnet(as.matrix(Training[, -1]), Training[, 1],family = "gaussian", alpha = 0.5)
## Predictions
pred_elnet_1 <- predict(cv_elnet, newx = as.matrix(Validation[, -1]), s = "lambda.min")
rmse(Validation$log_price, pred_elnet_1)

df_elnet_1 <- data.frame(id = test_ID, log_price = pred_elnet_1)

### 10-fold Cross validation for each alpha = 0, 0.1, ... , 0.9, 1.0 ###

for (i in 0:10) {
  assign(paste("fit", i, sep=""), cv.glmnet(as.matrix(Training[, -1]), Training[, 1], type.measure="mse", 
                                            alpha=i/10,family="gaussian"))
}

# Plot solution paths:
par(mfrow=c(3,2))
# For plotting options, type '?plot.glmnet' in R console
plot(fit.lasso, xvar="lambda")
plot(fit10, main="LASSO")

plot(fit.ridge, xvar="lambda")
plot(fit0, main="Ridge")

plot(fit.elnet, xvar="lambda")
plot(fit5, main="Elastic Net")

yhat0 <- predict(fit0, s=fit0$lambda.1se, newx=as.matrix(Validation[, -1]))
yhat1 <- predict(fit1, s=fit1$lambda.1se, newx=as.matrix(Validation[, -1]))
yhat2 <- predict(fit2, s=fit2$lambda.1se, newx=as.matrix(Validation[, -1]))
yhat3 <- predict(fit3, s=fit3$lambda.1se, newx=as.matrix(Validation[, -1]))
yhat4 <- predict(fit4, s=fit4$lambda.1se, newx=as.matrix(Validation[, -1]))
yhat5 <- predict(fit5, s=fit5$lambda.1se, newx=as.matrix(Validation[, -1]))
yhat6 <- predict(fit6, s=fit6$lambda.1se, newx=as.matrix(Validation[, -1]))
yhat7 <- predict(fit7, s=fit7$lambda.1se, newx=as.matrix(Validation[, -1]))
yhat8 <- predict(fit8, s=fit8$lambda.1se, newx=as.matrix(Validation[, -1]))
yhat9 <- predict(fit9, s=fit9$lambda.1se, newx=as.matrix(Validation[, -1]))
yhat10 <- predict(fit10, s=fit10$lambda.1se, newx=as.matrix(Validation[, -1]))

mse0 <- mean((Validation$log_price - yhat0)^2)
mse1 <- mean((Validation$log_price - yhat1)^2)
mse2 <- mean((Validation$log_price - yhat2)^2)
mse3 <- mean((Validation$log_price - yhat3)^2)
mse4 <- mean((Validation$log_price - yhat4)^2)
mse5 <- mean((Validation$log_price - yhat5)^2)
mse6 <- mean((Validation$log_price - yhat6)^2)
mse7 <- mean((Validation$log_price - yhat7)^2)
mse8 <- mean((Validation$log_price - yhat8)^2)
mse9 <- mean((Validation$log_price - yhat9)^2)
mse10 <- mean((Validation$log_price - yhat10)^2)

alphas <- (c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))
MSEs <- (c(mse0, mse1, mse2, mse3, mse4, mse5, mse6, mse7, mse8, mse9, mse10))
whichReg <- cbind(alphas, MSEs)
whichReg #best alpha = 0

fullXB <- read.csv("C:/Users/spnelson/SF/Personal Folders/Airbnb/fullXB.csv") # cleaned dataset

### XGBOOST model ###


set.seed(123)
# Model parameters trained using xgb.cv function

# xgboost fitting with arbitrary parameters
xgb_params_1 = list(
  objective = "reg:linear",                                                    # linear reg
  eta = 0.01,                                                                  # learning rate
  max.depth = 3,                                                               # max tree depth
  eval_metric = "rmse",                                                         # evaluation/loss metric
  nrounds = 100,
  lambda = 0,
  alpha = 0
  
)



xgbFit_1 = xgboost(data = as.matrix(Training[, -1]), 
                   nfold = 5, 
                   label = as.matrix(Training$log_price), 
                   nrounds = 2200, 
                   verbose = FALSE, 
                   objective = "reg:linear", 
                   eval_metric = "rmse", 
                   #nthread = 8, # how many cores? keep as unrun
                   eta = 0.01, # Makes the model more robust by shrinking the weights on each step
                   gamma = 0.0468 
                   #max_depth = 6, #tune, typical values 3-10
                   #min_child_weight = 1.7817, #prevents overfitting, tune
                   #subsample = 0.5213, #Lower values make the algorithm more conservative and prevents overfitting but too small values might lead to under-fitting.
                   #colsample_bytree = 0.4603
                   )


xgbFit_2 = xgb.cv(data = as.matrix(Training[, -1]), 
                  nfold = 5, 
                  label = as.matrix(Training$log_price), 
                  nrounds = 2200, 
                  verbose = FALSE, 
                  objective = "reg:linear", 
                  eval_metric = "rmse", 
                  nthread = 8, 
                  eta = 0.01, 
                  gamma = 0.0468, 
                  max_depth = 6, 
                  min_child_weight = 1.7817 
                  #subsample = 0.5213, 
                  #colsample_bytree = 0.4603
)

# set up the cross-validated hyper-parameter search

print(xgbFit_1)
importance_matrix <- xgb.importance(colnames(Training[, -1]), model = xgbFit_1)
print(importance_matrix[1:10])

xgb.plot.importance(importance_matrix[1:10])
# Feature       Gain       Cover   Frequency
# 1: room_typeEntire home/apt 0.51415657 0.018126034 0.018814139
# 2:                bathrooms 0.14821902 0.010016251 0.044260350
# 3:                 bedrooms 0.04804708 0.022727051 0.039455007
# 4:                   citySF 0.02719492 0.022468114 0.010332069
# 5:        number_of_reviews 0.02095528 0.016508164 0.086600889
# 6:     room_typeShared room 0.01414356 0.009075901 0.007132385
# 7:                   cityLA 0.01413093 0.008518132 0.005852512
# 8:                     beds 0.01134495 0.013313181 0.033171992
# 9:                   cityDC 0.00886063 0.003681286 0.005864147
# 10:                 Elevator 0.00818832 0.005122266 0.013357224

## Predictions
pred_xgb_1 <- predict(xgbFit_1, newdata = as.matrix(Validation[, -1]))
rmse(Validation$log_price, pred_xgb_1) #.4078572

df_xgb_1 <- data.frame(id = test_ID, log_price = pred_xgb_1)

### retaining on whole dataset and producing output ###

#Ridge
set.seed(123)
cv_lasso = cv.glmnet(as.matrix(training[, -1]), training[, 1], alpha=0)

## Predictions
pred_ridge_2 = data.frame(predict(cv_lasso, newx = as.matrix(testing[, -1]), 
                               s = "lambda.min"))

df_cv_lasso <- data.frame(id = test_ID, log_price = pred_ridge_2)
write.csv(df_cv_lasso, "submission_4_Ridge_2.csv", row.names = FALSE)


### XGBoost ###
set.seed(123)
xgbFit_3 = xgboost(data = as.matrix(training[, -1]), 
                   nfold = 5, 
                   label = as.matrix(training$log_price), 
                   nrounds = 2200, 
                   verbose = FALSE, 
                   objective = "reg:linear", 
                   eval_metric = "rmse", 
                   #nthread = 8, # how many cores? keep as unrun
                   eta = 0.01, # Makes the model more robust by shrinking the weights on each step
                   gamma = 0.0468 
                   #max_depth = 6, #tune, typical values 3-10
                   #min_child_weight = 1.7817, #prevents overfitting, tune
                   #subsample = 0.5213, #Lower values make the algorithm more conservative and prevents overfitting but too small values might lead to under-fitting.
                   #colsample_bytree = 0.4603
)
## print(xgbFit)

## Predictions
pred_xgb_2 <- predict(xgbFit_3, newdata = as.matrix(testing[, -1]))

#write final submission (KEY: take log of pred, make sure column names are id and log_price)
df <- data.frame(id = test_ID, log_price = pred_xgb_2)
write.csv(df, "submission_3_xg_1.csv", row.names = FALSE)

df <- data.frame(id = test_ID, log_price = .7 * pred_xgb_2 + .3 * pred_ridge_2)
write.csv(df, "submission_5_xg_ridge_2.csv", row.names = FALSE)


### Further Hyperparameter tuning ###

# modeling with MLR
library(janitor)
newdataobject <- Training %>% clean_names()

# Tuning the parameters 

xgbGrid <- expand.grid(
  nrounds = c(10000),
  max_depth = seq(3,6,by=1),
  eta = seq(0.03,0.05,by=0.01),
  gamma = seq(0,1,by=1),
  colsample_bytree = seq(0.4,0.6,by = 0.1),
  min_child_weight = seq(1,1,by = 0.5),
  subsample = seq(0.4,0.6,by = 0.1)
)

rmseErrorsHyperparameters <- apply(xgbGrid, 1, function(parameterList){
  
  #Extract Parameters to test
  currentSubsampleRate <- parameterList[["subsample"]]
  currentColsampleRate <- parameterList[["colsample_bytree"]]
  currentMin_Child_Weight <- parameterList[["min_child_weight"]]
  currentGamma <- parameterList[["gamma"]]
  currentEta <- parameterList[["eta"]]
  currentMax_Depth <- parameterList[["max_depth"]]
  currentNrounds <- parameterList[["nrounds"]]
  
  params <- list(objective = "reg:linear", 
                 #booster = "gbtree", 
                 #eta = 2/currentNrounds,
                 eta = currentEta, 
                 gamma = currentGamma, 
                 max_depth = currentMax_Depth, 
                 min_child_weight = currentMin_Child_Weight, 
                 subsample = currentSubsampleRate, 
                 colsample_bytree = currentColsampleRate)
  
  xgbcv <- xgb.cv(params = params, 
                  data = as.matrix(Training[, -1]), label = as.matrix(Training$log_price),
                  nrounds = currentNrounds, nfold = 5, 
                  showsd = T, stratified = T, early_stopping_rounds = 20, maximize = F)
  
  testrmse <- xgbcv$evaluation_log$test_rmse_mean[xgbcv$best_iteration]
  trainrmse <- xgbcv$evaluation_log$train_rmse_mean[xgbcv$best_iteration]
  
  return(c(testrmse, trainrmse, currentSubsampleRate, currentColsampleRate,
           currentMin_Child_Weight,currentGamma,currentEta,
           currentMax_Depth,currentNrounds,xgbcv$best_iteration))
  
})

# select best parameters
simTrain <- as.data.frame(t(rmseErrorsHyperparameters))
colnames(simTrain) <- c('TestRMSE','TrainRMSE','SubSampleRate','ColSampleRate',
                        'MinChildWgt','Gamma','ETA','MaxDepth','NRound', 'Iteration')
simTrain$Diff <- simTrain$TestRMSE - simTrain$TrainRMSE
bestTrain <- simTrain[simTrain$TestRMSE == min(simTrain$TestRMSE),]
bestTrain

# validate the model again

params <- list(objective = "reg:linear", 
               #booster = "gbtree", 
               eta = 0.1, 
               gamma = 0, 
               max_depth = 6
               #min_child_weight = 1, 
               #subsample = bestTrain$SubSampleRate, 
               #colsample_bytree = bestTrain$ColSampleRate
)
fullXB <- read.csv("C:/Users/spnelson/SF/Personal Folders/Airbnb/fullXB.csv")
drops <- c("id")
fullXB<-fullXB[,!(names(fullXB) %in% drops)] 

fullXB <- data.frame(lapply(fullXB, function(x) as.numeric(x)))

test.xg <- fullXB[fullXB$isTest==1,]
train.xg <- fullXB[fullXB$isTest==0,]
train.xg$log_price <- na.aggregate(train.xg$log_price, FUN = mean)

set.seed(222)

inTrain <- createDataPartition(y = train.xg$log_price, p = 0.7, list = FALSE)
Training <- train.xg[inTrain, ]
Validation <- train.xg[-inTrain, ]
training <- train.xg
testing <- test.xg

xgmodel <- xgboost(params = params, data = as.matrix(training[, -1]), label = as.matrix(training$log_price), nround = 3500)

xgBoostValidation <- predict(xgmodel,as.matrix(Validation[, -1])) 
predicted <- data.frame(SalePrice=Validation$log_price, Prediction=xgBoostValidation)

#plot results
ggplot(predicted, aes(x=log_price, y=Prediction)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)   # Add linear regression line

# see important features
mat <- xgb.importance (feature_names = colnames(Training),model = xgmodel)
xgb.plot.importance (importance_matrix = mat[1:20]) 


rmse(Validation$log_price, xgBoostValidation) # evaluation metric

xgmodel_tuned <- xgboost(params = params, data = as.matrix(training[, -1]), label = as.matrix(training$log_price), nround = 3500)
## Predictions
pred_xgb_3 <- predict(xgmodel, newdata = as.matrix(testing[, -1]))

#write final submission (KEY: take log of pred, make sure column names are id and log_price)
df <- data.frame(id = test_ID, log_price = pred_xgb_3)
write.csv(df, "submission_7_xg_4.csv", row.names = FALSE) #final xgboost output