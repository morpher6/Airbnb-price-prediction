### Process: ###
# - Treat NA values
# - Cleanse Data and transform
# - EDA and Histograms
# - fit initial model
# - feature engineering
# - more modeling

### Step 1:  Load libraries ###

debug(utils:::unpackPkgZip) #get past firewall

library(randomForest)
library(magrittr)
library(dplyr)
library(lubridate)
library(zipcode)
library(mice)

### Step 2: Load data ###

path = "C:/Users/spnelson/SF/Personal Folders/Airbnb/"
train <- read.csv(paste0(path,"train.csv"), header = T, stringsAsFactors = F)
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

#used website to impute missing zip codes:
new_zip2 <- read.csv("~/Airbnb-price-prediction/new_zip2.csv")

# Struggling to figure out merge, so just merged via excel - zipcode is now accurate and no NAs





# Use lat/long to estimate bathroom, bedrooms, and beds; 

#use mice package

#bathrooms, bedrooms, beds:
imputed_bathrooms <- mice(as.data.frame(train_df[,c("bathrooms", "bedrooms", "beds", "id")], m=5, maxit = 50, method = 'pmm', seed = 500))
completeData <- complete(imputed_bathrooms,2)
train_df <- merge(completeData, train_df, by = "id")

train_df <- train_df[ , -which(names(train_df) %in% c("bathrooms.y","bedrooms.y", "beds.y"))] #remove duplicate columns
names(train_df)[names(train_df) == 'bathrooms.x'] <- 'bathrooms'
names(train_df)[names(train_df) == 'bedrooms.x'] <- 'bedrooms'
names(train_df)[names(train_df) == 'beds.x'] <- 'beds'




# reviews scores -> new tag, fix NA's
# 











### Cleanse Data ###

# convert cleaning fee to boolean
train_df$cleaning_fee <- as.logical(train_df$cleaning_fee)

# convert first_review to date format
train_df$first_review <- as.Date(train_df$first_review, format = "%Y-%m-%d")

# convert host_has profile pic to boolean
train_df$host_has_profile_pic <- as.logical(train_df$host_has_profile_pic)

# convert host_identity_verified to boolean
train_df$host_identity_verified <- as.logical(train_df$host_identity_verified)

# convert host_since to date
train_df$host_since <- as.Date(train_df$host_since, format = "%Y-%m-%d")

# convert instant_bookable to boolean
train_df$instant_bookable <- as.logical(train_df$instant_bookable)

# convert last_review to date
train_df$last_review <- as.Date(train_df$last_review, format = "%Y-%m-%d")

# difference between first review and last review (measured in days)
train_df$diff_first_last_review <- difftime(train_df$last_review, train_df$first_review, units = "days")

# make a nchar(name) column
train_df$namelength <- as.numeric(nchar(train_df$name))



### EDA and Histograms ###













  
# Train a Random Forest model with cross-validation

cv_folds <- sample(1:3, size = nrow(train), replace = TRUE)

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
