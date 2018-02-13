### Step 1:  Load libraries ###
debug(utils:::unpackPkgZip) #get past firewall

library(randomForest)
library(magrittr)
library(dplyr)

### Step 2: Load data ###

path = "C:/Users/spnelson/SF/Personal Folders/Airbnb/"
train <- read.csv(paste0(path,"train.csv"), header = T, stringsAsFactors = F)
test <- read.csv(paste0(path,"test.csv"), header = T, stringsAsFactors = F)
ids = test$id
test$log_price <- NA

all_data <- rbind(train,test)
train_set = 1:nrow(train)
test_set <- (nrow(train)+1):(nrow(train) + nrow(test))

# Select a subset of the data

keep_cols <- c('property_type','room_type','bed_type','cancellation_policy','city',
               'accommodates','bathrooms','latitude','longitude',
               'number_of_reviews','review_scores_rating','log_price')

all_data <- all_data[,keep_cols]

### Exploratory Questions ###

# Break up amenities into dummy vars?
#Figure out variables to remove: We could probably remove picture URL 
#how do we handle missing values?
#should we remove description because it adds little value?
#do amenities affect price? If so, binary values can be used by creating one column per amenity (see code below)

### Data transformation ###
#transform data
amenities<-strsplit(train$amenities, ",")
a<-unlist(amenities)
a<-gsub("[{]","", a)
a<-gsub("[}]","", a)
a<-gsub("[^A-Za-z0-9,;._-]"," ", a)
a<-trimws(a)
b<-sort(table(a), decreasing = T)
col_name<-names(b)
temp_df<-data.frame(matrix(ncol = 131, nrow = nrow(train)))
colnames(temp_df) <- col_name
train_df<-cbind(train, temp_df)

### cleanse data ###

# convert cleaning fee to boolean
train_df$cleaning_fee <- as.logical(train_df$cleaning_fee)

# convert first_review to date format
# convert host_has profile pic to boolean
train_df$host_has_profile_pic <- as.logical(train_df$host_has_profile_pic)

# convert host_identity_verified to boolean
train_df$host_identity_verified <- as.logical(train_df$host_identity_verified)

# convert host_since to date
# convert instant_bookable to boolean
train_df$instant_bookable <- as.logical(train_df$instant_bookable)

# convert last_review to date
# difference between first review and last review
# make a nchar(name) column
train_df$namelength <- as.numeric(nchar(train_df$name))

### EDA and histograms


###treating NAs
#large NA: bathrooms (convert to Not Specified), reviews_scores_rating, bedrooms, beds 



# Impute missing values with 0

fillna <- function(column) {
  column[is.na(column)] <- 0
  return(column)
}

col_type <- sapply(all_data,class)
numeric_type <- !(col_type %in% c("character","factor"))
all_data[,numeric_type] <- sapply(all_data[,numeric_type], fillna)

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
