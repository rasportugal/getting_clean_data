## Define home_directory and librarys needed to the tidy data
home_directory <- getwd()
library(data.table)
library(dplyr)
library(plyr)


## Join all data from the test sample in a single data table
## General data: subject, activity and features
setwd("./test")
subject_test <- data.table(read.table("subject_test.txt", stringsAsFactors=FALSE))
activity_test <- data.table(read.table("y_test.txt", stringsAsFactors=FALSE))
features_test <- data.table(read.table("X_test.txt", stringsAsFactors=FALSE))
setwd(home_directory)

## Inertial Signals data
setwd("./test/Inertial Signals")
files_test <- list.files(getwd())

test_inertial_signals <- data.table()

for (i in files_test) {
        if(nrow(test_inertial_signals)==0){
                test_inertial_signals <- data.table(read.table(i,stringsAsFactors=FALSE))
        }else{
                temp_table <- data.table(read.table(i,stringsAsFactors=FALSE))
                test_inertial_signals <- cbind(test_inertial_signals, temp_table)
        }
}


## Build test data table
test_DB <- cbind(subject_test, activity_test, features_test, test_inertial_signals)
setwd(home_directory)



## Join all data from the train sample in a single data table
## General data: subject, activity and features
setwd("./train")
subject_train <- data.table(read.table("subject_train.txt", stringsAsFactors=FALSE))
activity_train <- data.table(read.table("y_train.txt", stringsAsFactors=FALSE))
features_train <- data.table(read.table("X_train.txt", stringsAsFactors=FALSE))
setwd(home_directory)

## Inertial Signals data
setwd("./train/Inertial Signals")
files_train <- list.files(getwd())

train_inertial_signals <- data.table()

for (i in files_train) {
        if(nrow(train_inertial_signals)==0){
                train_inertial_signals <- data.table(read.table(i,stringsAsFactors=FALSE))
        }else{
                temp_table <- data.table(read.table(i,stringsAsFactors=FALSE))
                train_inertial_signals <- cbind(train_inertial_signals, temp_table)
        }
}


## Build train data table
train_DB <- cbind(subject_train, activity_train, features_train, train_inertial_signals)
setwd(home_directory)



## A unique data table with the entire sample (test + train)
main_DB <- rbind(test_DB, train_DB)
main_DB <- as.data.frame(main_DB)
setwd(home_directory)



## Variable names attribuition and first selection of the collumns of interest
main_DB2 <- main_DB[,1:563]
measurement_names <- data.table(read.table("features.txt",stringsAsFactors=FALSE))
names_col <- c("subject", "activity", measurement_names[,V2])
colnames(main_DB2)<-names_col



## Extracts only the measurements on the mean and standard deviation for each 
## measurement
col_mean <- grep("mean()",names_col, fixed=TRUE)
col_std <- grep("std()",names_col, fixed=TRUE)
col_selected <- sort(c(1,2,col_mean, col_std))
interest_DB <- main_DB2[,col_selected]


## descriptive activity names to name the activities in the data set
act_names <- c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")
interest_DB2 <- as.integer(interest_DB$activity)

for (i in 1:6){
        interest_DB2 <- gsub(i, act_names[i],  interest_DB2, fixed=TRUE)
}

interest_DB$activity_names <- interest_DB2
interest_DB_final <- interest_DB[,c(1,69,3:68)]

## data set with the average of each variable for each activity and each subject
by_subject_activity <- group_by(interest_DB_final,subject, activity_names)
tidy_data_set <- summarise_each(by_subject_activity,funs(mean))

write.table(tidy_data_set, "tidy_data.txt", row.name=FALSE)
