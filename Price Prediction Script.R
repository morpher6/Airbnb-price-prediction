### Process: ###
# - Treat NA values
# - Cleanse Data and transform
# - EDA and Histograms
# - fit initial model
# - feature engineering
# - more modeling
# DUE MARCH 2ND

### Step 1:  Load libraries ###

debug(utils:::unpackPkgZip) #get past firewall

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

# convert first_review to date format CHANGE
train$first_review <- as.Date(train$first_review, format = "%Y-%m-%d")

# convert host_has profile pic to boolean 

train$host_has_profile_pic = ifelse(train$host_has_profile_pic =="t",1,0)

# convert host_identity_verified to boolean 
train$host_identity_verified = ifelse(train$host_identity_verified =="t",1,0)

# convert host_since to date CHANGE
train$host_since <- as.Date(train$host_since, format = "%Y-%m-%d")

# convert instant_bookable to boolean CHANGE
train$instant_bookable = ifelse(train$instant_bookable =="t",1,0)

# convert last_review to date CHANGE
train$last_review <- as.Date(train$last_review, format = "%Y-%m-%d")

# difference between first review and last review (measured in days) CHANGE
train$diff_first_last_review <- difftime(train$last_review, train$first_review, units = "days")
train_df$number_of_reviews <- as.numeric(train_df$number_of_reviews )


#transform data
#do amenities effect price? If so, binary values can be used by creating one column per amenity (see code below)
#remove extra characters
train_sub<-train[,-c(12,13,17,19,22,26)] 
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
train_df <- train_df[ , -which(names(train_df) %in% c("latitude", "longitude", "neighborhood", "diff_first_last_review", "review_scores_rating"))] 

# check for correlation between numeric predictions
train_df$bathrooms <- as.numeric(train_df$bathrooms)
train_df$beds <- as.numeric(train_df$beds)
train_df$accommodates <- as.numeric(train_df$accommodates)

numerical <- train_df[ , which(names(train_df) %in% c("bathroooms", "bedrooms", "beds", "accommodates", "number_of_reviews"))] 

descrCor <-  cor(numerical)
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .9) #no high corr
highCorr


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
