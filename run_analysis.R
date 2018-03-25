# Read in train data
train <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
train_sub <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
train_act <- read.table("./data/UCI HAR Dataset/train/y_train.txt")

# Read in test data
test <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
test_sub <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")
test_act <- read.table("./data/UCI HAR Dataset/test/y_test.txt")

# Read in features & activity labels
features <- read.table("./data/UCI HAR Dataset/features.txt")
act_labels <- read.table("./data/UCI HAR Dataset/activity_labels.txt")         

# Bind data
test <- test_sub%>% bind_cols(test_act) %>% 
        bind_cols(test)

train <- train_sub%>% bind_cols(train_act) %>% 
        bind_cols(train)


# Coerce 'features' into legal & unique names (suitable as variable names)
features$V2 <- make.names(features$V2, unique = TRUE, allow_ = TRUE)

# make colnames by concatenating respective char vectors 
colnames(test)<- c("subject_id", "activity", paste0(features$V2))
colnames(train)<- c("subject_id", "activity", paste0(features$V2))


# Use correspondence between 'act_labels' and 'activity' to use descriptive labels for activity
m1 <- match(test$activity,act_labels$V1)
m2 <- match(train$activity,act_labels$V1)

test$activity <- act_labels$V2[m1]
train$activity <- act_labels$V2[m2]

# Combine dataframes
train_test <- bind_rows(train,test)

# Extract only the measurements on the mean and standard deviation for each measurement
# limit to subject_id, activity, time|frequency vars that measure mean|std   

df <- train_test %>% 
        select (subject_id, activity, starts_with("t"), starts_with("f")) %>% 
        select (subject_id, activity, contains("mean.."), contains("std.."))

# Appropriately label the data set  
names(df)<-gsub("std..", "SD", names(df))
names(df)<-gsub("mean..", "Mean", names(df))
names(df)<-gsub("^t", "Time", names(df))
names(df)<-gsub("^f", "Frequency", names(df))
names(df)<-gsub("Acc", "Accelerometer", names(df))
names(df)<-gsub("Gyro", "Gyroscope", names(df))
names(df)<-gsub("Mag", "Magnitude", names(df))
names(df)<-gsub("BodyBody", "Body", names(df))

# From the dataframe above, create a second, independent tidy data set with the 
# Average of each variable for each activity and each subject.
        avg_val <- df %>% 
        gather("measure", "val",3:68) %>% 
        group_by (measure,subject_id, activity) %>%   
        summarise(average=mean(val))

# save new tidy data file
write.table(finalTidyData, file = "tidydata.txt",row.name=FALSE)
