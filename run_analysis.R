library(dplyr)

filename <- "project.zip"

#check if the file exists
 
if (!file.exists(filename)){ 
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, filename, method="curl")
}  

# Check if the folder exists
if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}


# data setup

features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")


# Step 1: Merges the training and the test sets to create one data set.

x_dataset <- bind_rows(x_train, x_test)
y_dataset <- bind_rows(y_train, y_test)
subject_dataset <- bind_rows(subject_train, subject_test)

full_dataset <- bind_cols(x_dataset, y_dataset, subject_dataset)

# Step 2: Extracts only the measurements on the mean and standard deviation for each measurement.

tidy_dataset <- full_dataset %>%select(subject, code, contains ("mean"), contains("std"))

# Step 3: Uses descriptive activity names to name the activities in the data set.

tidy_dataset$code <- activities[tidy_dataset$code, 2] 

# Step 4: Appropriately labels the data set with descriptive variable names.

names(tidy_dataset)[2] = "activity"
names(tidy_dataset)<-gsub("Acc", "Accelerometer", names(tidy_dataset))
names(tidy_dataset)<-gsub("Gyro", "Gyroscope", names(tidy_dataset))
names(tidy_dataset)<-gsub("BodyBody", "Body", names(tidy_dataset))
names(tidy_dataset)<-gsub("Mag", "Magnitude", names(tidy_dataset))
names(tidy_dataset)<-gsub("^t", "Time", names(tidy_dataset))
names(tidy_dataset)<-gsub("^f", "Frequency", names(tidy_dataset))
names(tidy_dataset)<-gsub("tBody", "TimeBody", names(tidy_dataset))
names(tidy_dataset)<-gsub("-mean()", "Mean", names(tidy_dataset), ignore.case = TRUE)
names(tidy_dataset)<-gsub("-std()", "STD", names(tidy_dataset), ignore.case = TRUE)
names(tidy_dataset)<-gsub("-freq()", "Frequency", names(tidy_dataset), ignore.case = TRUE)
names(tidy_dataset)<-gsub("angle", "Angle", names(tidy_dataset))
names(tidy_dataset)<-gsub("gravity", "Gravity", names(tidy_dataset))


#Step 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

summarized_dataset <- tidy_dataset %>%
                      group_by(subject, activity) %>%
                      summarize_all(list(mean))
  
write.table(summarized_dataset, "summarized_data.txt", row.name=FALSE)
