### Process: ###
# - Treat NA values
# - Cleanse Data and transform
# - EDA and Histograms
# - fit initial model
# - feature engineering
# - more modeling
# DUE MARCH 2ND

### Step 1:  Load libraries ###

debug(utils:::unpackPkgZip) #get past firewall (click 103 times)

library(randomForest)
library(magrittr)
library(dplyr)
library(lubridate)
library(zipcode)
library(mice)
library(ggplot2)

### Step 2: Load data ###

path = "C:/Users/spnelson/SF/Personal Folders/Airbnb/"
train <- read.csv(paste0(path,"trainCleanZip.csv"), header = T, stringsAsFactors = F)
test <- read.csv(paste0(path,"test.csv"), header = T, stringsAsFactors = F)
ids = test$id
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


#transform data
#do amenities effect price? If so, binary values can be used by creating one column per amenity (see code below)
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
path2 <- "~/Airbnb-price-prediction/"
new_zip2 <- read.csv(paste0(path2,"new_zip2.csv"), header = T, stringsAsFactors = F)

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


 


#use mice package to estimate
#bathrooms, bedrooms, beds:
imputed_bathrooms <- mice(as.data.frame(fullSet[,c("bathrooms", "bedrooms", "beds", "id")], m=5, maxit = 50, method = 'pmm', seed = 500))
completeData <- complete(imputed_bathrooms,2)
fullSet <- merge(completeData, fullSet, by = "id")

fullSet <- fullSet[ , -which(names(fullSet) %in% c("bathrooms.y","bedrooms.y", "beds.y"))] #remove duplicate columns
names(fullSet)[names(fullSet) == 'bathrooms.x'] <- 'bathrooms'
names(fullSet)[names(fullSet) == 'bedrooms.x'] <- 'bedrooms'
names(fullSet)[names(fullSet) == 'beds.x'] <- 'beds'


# Fix reviews_scores_rating by making it categorical and binning
# https://github.com/samuelklam/airbnb-pricing-prediction/blob/master/data-cleaning/data-cleaning-listings.ipynb

fullSet$review_scores_rating <- as.numeric(fullSet$review_scores_rating)
fullSet$ReviewCategory <- fullSet$review_scores_rating %>% cut(fullSet$review_scores_rating, breaks=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 85, 90, 95, Inf), 
                                                                 labels=c("0-9","10-19","20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-84", "85-89", "90-94", "95-100"))

fullSet$ReviewCategory <- as.character(fullSet$ReviewCategory)
fullSet$ReviewCategory[is.na(fullSet$ReviewCategory)] <- "No Reviews" # turn NaN scores with 0 reviews into 'No Reviews'


### Variables to get rid of ###
# amenities, latitude, longitude, neighborhood, diff_first_last_review, review_score_category
fullSet <- fullSet[ , -which(names(fullSet) %in% c("latitude", "longitude", "neighbourhood", "diff_first_last_review", "review_scores_rating", 'amenities', 'host_response_rate'))] 

# check for correlation between numeric predictions
fullSet$bathrooms <- as.numeric(fullSet$bathrooms)
fullSet$beds <- as.numeric(fullSet$beds)
fullSet$accommodates <- as.numeric(fullSet$accommodates)

numerical <- fullSet[ , which(names(fullSet) %in% c("bathroooms", "bedrooms", "beds", "accommodates", "number_of_reviews"))] 

descrCor <-  cor(numerical)
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .9) #no high corr
highCorr #no high correlation vars



df['lat_center']=df.apply(lambda row: lat_center(row), axis=1)
df['long_center']=df.apply(lambda row: long_center(row), axis=1)

lat_center <- function(row){
  if (row['city']=='NYC')
    return 40.72
}

long_center <- function(row){
  if (row['city']=='NYC')
    return -74
}

fullSet$lat_center <- apply(lat_center(row),1)
fullSet$long_center <- apply(long_center(row),1)

df$distance_to_center = sqrt((fullSet$lat_center]-fullSet$latitude)**2+(fullSet['long_center']-fullSet['longitude'])**2)


#resplit model
test.new <- fullSet[fullSet$isTest==1,]
train.new <- fullSet[fullSet$isTest==0,]

### EDA and Histograms ###

table(train.new$ReviewCategory)
table(train.new$property_type)
table(train.new$room_type)
table(train.new$city)


# visualize distribution of log price (target variable)
p1 <- ggplot(train.new) 
p1 + geom_histogram(aes(x = log_price))

#visualize beds
p1 + geom_bar(aes(x = beds))

#visualize 
p1 + geom_bar(aes(ReviewCategory))
p1 + geom_bar(aes(bathrooms))
p1 + geom_bar(aes(bedrooms))
p1 + geom_bar(aes(beds))
p1 + geom_bar(aes(accommodates))
p1 + geom_bar(aes(room_type))
p1 + geom_bar(aes(cancellation_policy))
p1 + geom_bar(aes(cleaning_fee))
p1 + geom_bar(aes(city))
p1 + geom_bar(aes(ReviewCategory))





#################### Model Running ###

# Initial Linear model
#reduce size of train_df for initial modelleling
train_df_small <-  sample_frac(train.new, size = .2, replace = FALSE)

m1 <- lm(log_price ~ bathrooms + bedrooms + beds + property_type + room_type  + accommodates + 
           bed_type + cancellation_policy + cleaning_fee + host_has_profile_pic + host_identity_verified + instant_bookable + 
            number_of_reviews + zipcode + ReviewCategory, data=train_df_small)
summary(m1)






#boost
library(gbm)
train_gbm <- train_df
boost.airbnb=gbm(log_price ~ bathrooms + bedrooms + beds + as.factor(property_type) + as.factor(room_type)  + accommodates + 
                   as.factor(bed_type) + as.factor(cancellation_policy) + cleaning_fee + host_has_profile_pic + host_identity_verified + instant_bookable + 
                   number_of_reviews + as.factor(zipcode) + as.factor(ReviewCategory), data=train_gbm,distribution="gaussian",n.trees=500,interaction.depth=4)
summary(boost.airbnb)
# As usual, predict and evaluate on the test set
test.pred.gbm <- predict(boost.airbnb,test_df)
RMSE.forest <- sqrt(mean((test.pred.forest-tes_xg_smallt$log_price)^2))
RMSE.forest




# random forest

fullSet_rf <- fullSet


fullSet$property_type <- as.factor(fullSet$property_type)
fullSet$room_type <- as.factor(fullSet$room_type)
fullSet$bed_type <- as.factor(fullSet$bed_type)
fullSet$cancellation_policy <- as.factor(fullSet$cancellation_policy)
fullSet$city <- as.factor(fullSet$city)
fullSet$zipcode <- as.factor(fullSet$zipcode)
fullSet$ReviewCategory <- as.factor(fullSet$ReviewCategory)

#split to train and test

test.rf <- fullSet[fullSet$isTest==1,]
train.rf <- fullSet[fullSet$isTest==0,]


training <- train.rf[1:37064, ]
testing <- train.rf[37065:74129, ]
set.seed(222)
inTrain <- createDataPartition(y = training$log_price, p = 0.7, list = FALSE)
Training <- training[inTrain, ]
Validation <- training[-inTrain, ]

#train_rf_small <-  sample_frac(train.rf, size = .1, replace = FALSE)

library(randomForest)
library(Metrics)
# Create a random forest with 1000 trees

rf <- randomForest(log_price ~ bathrooms + bedrooms + beds + property_type + room_type  + accommodates + 
                     bed_type + cancellation_policy + cleaning_fee + host_has_profile_pic + host_identity_verified + instant_bookable + 
                     number_of_reviews + ReviewCategory, data = train.rf, importance = TRUE, ntree=100)
# How many trees are needed to reach the minimum error estimate? 
# This is a simple problem; it appears that about 100 trees would be enough. 
which.min(rf$mse)
# Using the importance()  function to calculate the importance of each variable
imp <- as.data.frame(sort(importance(rf)[,1],decreasing = TRUE),optional = T)
names(imp) <- "% Inc MSE"
imp

# As usual, predict and evaluate on the test set
test.pred.forest <- predict(rf,train.rf[,-5])
RMSE.forest <- sqrt(mean((test.pred.forest-train.rf$log_price)^2))
RMSE.forest

test.pred.forest <- predict(rf,test.rf[,-5])

submission1_RF <- data.frame(id = ids, log_price = test.pred.forest)


#do xgboost using same formula as RF
#convert factors to numeric


### XGBosst ###
# for xgboost, we need to work with only numerical variables. Thus, for categorical variables, we will do one-hot encoding
# since we are recoding variables, let's call the dataset something else
set.seed(123)
library(xgboost)
library(Matrix)
library(caret)
library(data.table)
library(stringr)
library(car)
train_xg <- train_df
train_xg_small <-  sample_frac(train_xg, size = .1, replace = FALSE)

train_xg_small = as.data.frame(train_xg_small)
# ohe_feats = c('property_type', 'room_type', 'bed_type', 'cancellation_policy', 'city', 'host_response_rate', 'zipcode', 'ReviewCategory')
# dummies = dummyVars(~ property_type+ bed_type+ cancellation_policy+ city+ host_response_rate+ zipcode+ ReviewCategory+ room_type, data = train_xg_small)
# df_all_ohe <- as.data.frame(predict(dummies, newdata = train_xg_small))
# df_all_combined <- cbind(train_xg_small[,-c(which(colnames(train_xg_small) %in% ohe_feats))],df_all_ohe)

train_xg_small = as.data.table(df_all_combined)


# Numeric Variables
Num<-sapply(train_xg_small,is.numeric)
Num<-train_xg_small[,Num]

for(i in 1:77){
  if(is.factor(train_xg_small[,i])){
    train_xg_small[,i]<-as.integer(train_xg_small[,i])
  }
}


test_xg_small = as.data.table(df_all_combined)
#now that we have only numerical, we can start using the xgboost package.


train_xg_small<- as.matrix(train_xg_small, rownames.force=NA)
test_xg_small<- as.matrix(test_xg_small, rownames.force=NA)
train_xg_small <- as(train_xg_small, "sparseMatrix")
test_xg_small <- as(test_xg_small, "sparseMatrix")
# Never forget to exclude objective variable in 'data option'
train_xg_small <- xgb.DMatrix(data = train_xg_small[,-1], label = train[,"log_price"])

xgb <- xgboost(data = data.matrix(train_xg_small[,-1]), 
               #booster = "gblinear", 
               objective = "binary:logistic", 
               max.depth = 5, 
               nround = 2, 
               lambda = 0, 
               lambda_bias = 0, 
               alpha = 0)











# Create submission file

sample_submission <- data.frame(id = ids, log_price = test.pred.forest)
write.csv(sample_submission, "sample_submission.csv", row.names = FALSE)
