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
#ids = test$id
test$log_price <- NA

#all_data <- rbind(train,test)
#train_set = 1:nrow(train)
#test_set <- (nrow(train)+1):(nrow(train) + nrow(test))

sapply(X = train, FUN = function(x) sum(is.na(x))) # number of missing values by column


### Exploratory Questions ###

# - Break up amenities into dummy vars?
# - Figure out variables to remove: We could probably remove picture URL 
# - how do we handle missing values?
# - should we remove description because it adds little value?
# - do amenities affect price? If so, binary values can be used by creating one column per amenity (see code below)


### Data transformation ###

### Cleanse Data ###

# convert cleaning fee to boolean
train$cleaning_fee <- as.integer(as.logical((train$cleaning_fee)))
test$cleaning_fee <- as.integer(as.logical((test$cleaning_fee)))


# convert host_has profile pic to boolean 

train$host_has_profile_pic = ifelse(train$host_has_profile_pic =="t",1,0)
test$host_has_profile_pic = ifelse(test$host_has_profile_pic =="t",1,0)

# convert host_identity_verified to boolean 
train$host_identity_verified = ifelse(train$host_identity_verified =="t",1,0)
test$host_identity_verified = ifelse(test$host_identity_verified =="t",1,0)


# convert instant_bookable to boolean CHANGE
train$instant_bookable = ifelse(train$instant_bookable =="t",1,0)
test$instant_bookable = ifelse(test$instant_bookable =="t",1,0)

train$number_of_reviews <- as.numeric(train$number_of_reviews )
test$number_of_reviews <- as.numeric(test$number_of_reviews )

#transform data
#do amenities effect price? If so, binary values can be used by creating one column per amenity (see code below)
#remove extra characters
drops <- c("description", "first_review", "host_since", "last_review", "name", "thumbnail_url")
train_sub<-train[,!(names(train) %in% drops)] 
test_sub<-test[,!(names(test) %in% drops)] 

amenities<-strsplit(train$amenities, ",")
amenities<-unlist(amenities)
amenities<-gsub("[{]","", amenities)
amenities<-gsub("[}]","", amenities)
amenities<-gsub("[^A-Za-z0-9,;._-]"," ", amenities)
amenities<-trimws(amenities)


#create a data frame with new attributes
col_name<-names(sort(table(amenities), decreasing = T))
col_name[c(17, 20, 23, 26, 40, 54, 67, 68, 75, 89, 103, 110, 115, 121)]<- c("Family/kid friendly", "translation missing: en.hosting_amenity_50", "translation missing: en.hosting_amenity_49", 
                                                                             "Buzzer/wireless intercom", "Dog(s)", "Cat(s)", "Childrenâ€™s books and toys", "Pack â€™n Play/travel crib", 
                                                                             "Childrenâ€™s dinnerware", "Other pet(s)", "Wide clearance to shower & toilet", "Fixed grab bars for shower & toilet",
                                                                             "Washer,Dryer", "Ski in/Ski out")
col_name<-trimws(col_name)

temp_df<-data.frame(matrix(ncol = 131, nrow = nrow(train)))
colnames(temp_df) <- col_name
train_df<-cbind(train_sub, temp_df)

#populate binary values for new amenities columns
for(i in 24:ncol(train_df)){
  rownums <- grep(names(train_df)[i], train_df$amenities)
  train_df[rownums,i]<- 1
}

#sort(sapply(train_df, FUN = function(x) sum(is.na(x))))
train_df[,24:ncol(train_df)][is.na(train_df[,24:ncol(train_df)])]<-0





# do it for test:
amenities<-strsplit(test$amenities, ",")
amenities<-unlist(amenities)
amenities<-gsub("[{]","", amenities)
amenities<-gsub("[}]","", amenities)
amenities<-gsub("[^A-Za-z0-9,;._-]"," ", amenities)
amenities<-trimws(amenities)


#create a data frame with new attributes
col_name<-names(sort(table(amenities), decreasing = T))
col_name[c(17, 20, 23, 26, 40, 54, 67, 68, 75, 89, 103, 110, 115, 121)]<- c("Family/kid friendly", "translation missing: en.hosting_amenity_50", "translation missing: en.hosting_amenity_49", 
                                                                            "Buzzer/wireless intercom", "Dog(s)", "Cat(s)", "Childrenâ€™s books and toys", "Pack â€™n Play/travel crib", 
                                                                            "Childrenâ€™s dinnerware", "Other pet(s)", "Wide clearance to shower & toilet", "Fixed grab bars for shower & toilet",
                                                                            "Washer,Dryer", "Ski in/Ski out")
col_name<-trimws(col_name)

temp_df<-data.frame(matrix(ncol = 131, nrow = nrow(test)))
colnames(temp_df) <- col_name
test_df<-cbind(test_sub, temp_df)

#populate binary values for new amenities columns
for(i in 24:ncol(test_df)){
  rownums <- grep(names(test_df)[i], test_df$amenities)
  test_df[rownums,i]<- 1
}

#sort(sapply(test_df, FUN = function(x) sum(is.na(x))))
test_df[,24:ncol(test_df)][is.na(test_df[,24:ncol(test_df)])]<-0












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
imputed_bathrooms <- mice(as.data.frame(train_df[,c("bathrooms", "bedrooms", "beds", "id")], m=5, maxit = 50, method = 'pmm', seed = 500))
completeData <- complete(imputed_bathrooms,2)
train_df <- merge(completeData, train_df, by = "id")

train_df <- train_df[ , -which(names(train_df) %in% c("bathrooms.y","bedrooms.y", "beds.y"))] #remove duplicate columns
names(train_df)[names(train_df) == 'bathrooms.x'] <- 'bathrooms'
names(train_df)[names(train_df) == 'bedrooms.x'] <- 'bedrooms'
names(train_df)[names(train_df) == 'beds.x'] <- 'beds'


# Fix reviews_scores_rating by making it categorical and binning
# https://github.com/samuelklam/airbnb-pricing-prediction/blob/master/data-cleaning/data-cleaning-listings.ipynb

train_df$review_scores_rating <- as.numeric(train_df$review_scores_rating)
train_df$ReviewCategory <- train_df$review_scores_rating %>% cut(train_df$review_scores_rating, breaks=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 85, 90, 95, Inf), 
                                                                  labels=c("0-9","10-19","20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-84", "85-89", "90-94", "95-100"))

train_df$ReviewCategory <- as.character(train_df$ReviewCategory)
train_df$ReviewCategory[is.na(train_df$ReviewCategory)] <- "No Reviews" # turn NaN scores with 0 reviews into 'No Reviews'


### Variables to get rid of ###
# amenities, latitude, longitude, neighborhood, diff_first_last_review, review_score_category
train_df <- train_df[ , -which(names(train_df) %in% c("latitude", "longitude", "neighbourhood", "diff_first_last_review", "review_scores_rating", 'amenities'))] 

# check for correlation between numeric predictions
train_df$bathrooms <- as.numeric(train_df$bathrooms)
train_df$beds <- as.numeric(train_df$beds)
train_df$accommodates <- as.numeric(train_df$accommodates)

numerical <- train_df[ , which(names(train_df) %in% c("bathroooms", "bedrooms", "beds", "accommodates", "number_of_reviews"))] 

descrCor <-  cor(numerical)
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .9) #no high corr
highCorr


###### repeat above on test set ########

imputed_bathrooms <- mice(as.data.frame(test_df[,c("bathrooms", "bedrooms", "beds", "id")], m=5, maxit = 50, method = 'pmm', seed = 500))
completeData <- complete(imputed_bathrooms,2)
test_df <- merge(completeData, test_df, by = "id")

test_df <- test_df[ , -which(names(test_df) %in% c("bathrooms.y","bedrooms.y", "beds.y"))] #remove duplicate columns
names(test_df)[names(test_df) == 'bathrooms.x'] <- 'bathrooms'
names(test_df)[names(test_df) == 'bedrooms.x'] <- 'bedrooms'
names(test_df)[names(test_df) == 'beds.x'] <- 'beds'


# Fix reviews_scores_rating by making it categorical and binning
# https://github.com/samuelklam/airbnb-pricing-prediction/blob/master/data-cleaning/data-cleaning-listings.ipynb

test_df$review_scores_rating <- as.numeric(test_df$review_scores_rating)
test_df$ReviewCategory <- test_df$review_scores_rating %>% cut(test_df$review_scores_rating, breaks=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 85, 90, 95, Inf), 
                                                                 labels=c("0-9","10-19","20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-84", "85-89", "90-94", "95-100"))

test_df$ReviewCategory <- as.character(test_df$ReviewCategory)
test_df$ReviewCategory[is.na(test_df$ReviewCategory)] <- "No Reviews" # turn NaN scores with 0 reviews into 'No Reviews'


### Variables to get rid of ###
# amenities, latitude, longitude, neighborhood, diff_first_last_review, review_score_category
test_df <- test_df[ , -which(names(test_df) %in% c("latitude", "longitude", "neighbourhood", "diff_first_last_review", "review_scores_rating", 'amenities'))] 

# check for correlation between numeric predictions
test_df$bathrooms <- as.numeric(test_df$bathrooms)
test_df$beds <- as.numeric(test_df$beds)
test_df$accommodates <- as.numeric(test_df$accommodates)

numerical <- test_df[ , which(names(test_df) %in% c("bathroooms", "bedrooms", "beds", "accommodates", "number_of_reviews"))] 






### EDA and Histograms ###

table(train_df$ReviewCategory)
table(train_df$property_type)
table(train_df$room_type)
table(train_df$city)
table(train_df$host_response_rate)
table(train_df$neighbourhood)


# visualize distribution of log price (target variable)
p1 <- ggplot(train_df) 
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
train_df_small <-  sample_frac(train_df, size = .2, replace = FALSE)

m1 <- lm(log_price ~ bathrooms + bedrooms + beds + property_type + room_type  + accommodates + 
           bed_type + cancellation_policy + cleaning_fee + host_has_profile_pic + host_identity_verified + instant_bookable + 
           host_response_rate + number_of_reviews + zipcode + ReviewCategory, data=train_df_small)
summary(m1)

  

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
ohe_feats = c('property_type', 'room_type', 'bed_type', 'cancellation_policy', 'city', 'host_response_rate', 'zipcode', 'ReviewCategory')
dummies = dummyVars(~ property_type+ bed_type+ cancellation_policy+ city+ host_response_rate+ zipcode+ ReviewCategory+ room_type, data = train_xg_small)
df_all_ohe <- as.data.frame(predict(dummies, newdata = train_xg_small))
df_all_combined <- cbind(train_xg_small[,-c(which(colnames(train_xg_small) %in% ohe_feats))],df_all_ohe)

train_xg_small = as.data.table(df_all_combined)

#for test set:
test_xg <- test_df
test_xg_small <-  sample_frac(test_xg, size = .1, replace = FALSE)

test_xg_small = as.data.frame(test_xg_small)
ohe_feats = c('property_type', 'room_type', 'bed_type', 'cancellation_policy', 'city', 'host_response_rate', 'zipcode', 'ReviewCategory')
dummies = dummyVars(~ property_type+ bed_type+ cancellation_policy+ city+ host_response_rate+ zipcode+ ReviewCategory+ room_type, data = test_xg_small)
df_all_ohe <- as.data.frame(predict(dummies, newdata = test_xg_small))
df_all_combined <- cbind(test_xg_small[,-c(which(colnames(test_xg_small) %in% ohe_feats))],df_all_ohe)

test_xg_small = as.data.table(df_all_combined)
#now that we have only numerical, we can start using the xgboost package.







# Train a Random Forest model with cross-validation

cv_folds <- sample(1:3, size = nrow(train_df), replace = TRUE)

for(i in 1:3) {
  # Train the model using the training sets
  fit <- randomForest(log_price ~ .,
                      data = all_data[train_set[cv_folds !=i],],
                      ntree = 10)
  
  # Make predictions using the testing set
  preds <- predict(fit, all_data[train_set[cv_folds == i],])
  
  # Calculate RMSE for current cross-validation split
  print(mean((preds - all_data[train_set[cv_folds == i],'log_price'])^2)^.5)
}

# Create submission file

fit <- randomForest(log_price ~ ., data = all_data[train_set,], ntree = 1)
prediction <- predict(fit, all_data[test_set,])

sample_submission <- data.frame(id = ids, log_price = prediction)
write.csv(sample_submission, "sample_submission.csv", row.names = FALSE)
