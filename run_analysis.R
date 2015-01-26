# Code to be added later:
# This R script called run_analysis.R that does the following. 
#
# 1.) Merges the training and the test sets to create one data set.
# 2.) Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3.) Uses descriptive activity names to name the activities in the data set
# 4.) Appropriately labels the data set with descriptive variable names. 
# 5.) From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#
# The dataset was obtained form the following url:
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
#
# The files used from the raw dataset are the following:
# activity_labels.txt
# features.txt
# subject_test.txt
# subject_train.txt
# X_test.txt
# Y_test.txt
# X_training.txt
# Y_training.txt

library("digest")
library("data.table")
library("dplyr")
# Next few lines is to see if the zip file containing the raw dataset is available in the 
# current working dir. Also checks to see if the checksum of the currently available one
# matches a know good checksum. This is to ensure data integrity.
rawDatasetZipMD5 <- "d29710c9530a31f303801b6bc34bd895"
rawDatasetZipName <- "getdata-projectfiles-UCI HAR Dataset.zip"
rawDatasetURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
if(!file.exists(rawDatasetZipName)){
    # Download the file if it doesn't exist
    download.file(rawDatasetURL, method="curl", destfile = rawDatasetZipName)
}else if(digest(file.path(rawDatasetZipName) , algo = "md5", file = T) != rawDatasetZipMD5){
    # If the currently available zip file doesn't match the checksum, redownload it
    download.file(rawDatasetURL, method="curl", destfile = rawDatasetZipName)
}
# unzip and overwrite any of the raw data files that may have already been extracted.
unzip(rawDatasetZipName)

# Raw datasets
xTestFile <- "UCI HAR Dataset/test/X_test.txt"
yTestFile <- "UCI HAR Dataset/test/Y_test.txt" # the activity set labels. Refer to activity_labels.txt to see the mapping
testSubjectsFile <- "UCI HAR Dataset/test/subject_test.txt"

xTrainFile <- "UCI HAR Dataset/train/X_train.txt"
yTrainFile <- "UCI HAR Dataset/train/Y_train.txt" # the activity set labels. Refer to activity_labels.txt to see the mapping
trainSubjectsFile <- "UCI HAR Dataset/train/subject_train.txt"

activityLabelsFile <- "UCI HAR Dataset/activity_labels.txt"

# The raw datasets need a little preprocessing to make it easier to read in to a data frame.
# The code below is supposed to remove any white space characters at the beginning of a line
# in each of the dataset files.
for(inFile in c(xTestFile, xTrainFile, yTestFile, yTrainFile)){
    tf1 <- readLines(inFile, encoding = "UTF-8")
    tf1 <- sub(pattern = "^\ +", replacement = "", x = tf1)
    cat(tf1, file = inFile, sep = "\n")
}

# Create column names from "features.txt"
cnames <- read.csv("UCI HAR Dataset//features.txt", sep = " ", header = F)
cnames <- cnames[2]
cnames <- cnames[,1]
cnames <- as.character(cnames)
cnames <- make.names(cnames, unique = T)

# Put together all the "test" data and "subject_test.txt"
testSet1 <- read.table(xTestFile, header = F)
testActivitySet1 <- read.table(yTestFile, header = F)
testSubjects <- read.table(testSubjectsFile, header = F)

testSet1 <- as.data.frame(testSet1)
testActivitySet1 <- as.data.frame(testActivitySet1)
testSubjects <- as.data.frame(testSubjects)

colnames(testSet1) <- cnames
colnames(testActivitySet1) <- c("activity")
colnames(testSubjects) <- c("subject_num")


# Put together all the "training" data
trainSet1 <- read.table(xTrainFile, header = F)
trainActivitySet1 <- read.table(yTrainFile, header = F)
trainSubjects <- read.table(trainSubjectsFile, header = F)

trainSet1 <- as.data.frame(trainSet1)
trainActivitySet1 <- as.data.frame(trainActivitySet1)
trainSubjects <- as.data.frame(trainSubjects)

colnames(trainSet1) <- cnames
colnames(trainActivitySet1) <- c("activity")
colnames(trainSubjects) <- c("subject_num")

# Combine "test" and "training" datasets
combinedSets2 <- rbind(testSet1, trainSet1) 
completeTestSet <- cbind(testSubjects,testActivitySet1, testSet1)
completeTrainSet <- cbind(trainSubjects, trainActivitySet1, trainSet1)
combinedSets <- rbind(completeTestSet, completeTrainSet)

# The filtered set that contains that only the Subjects, Activities, Means, and STDs
filteredSet <- select(combinedSets, subject_num, activity, contains("mean"), contains("std"))

# Per the assignment requirements: "Uses descriptive activity names to name the activities in the data set"
# the following will map the values in the activities column to their "desctiptive" names as defined in "activity_labels.txt"
activitiesTable <- read.csv(activityLabelsFile, sep = " ", header = F)
colnames(activitiesTable) <- c("activity_id", "activity_name")
filteredSet <- merge(filteredSet, activitiesTable, by.x="activity", by.y="activity_id")

uSubjects <- unique(filteredSet$subject_num)
uActivities <- unique(activitiesTable$activity_name)

mainSummaryTable <- filteredSet[-c(1:nrow(filteredSet)), ]
tTable1 <- filteredSet[-c(1:nrow(filteredSet)), ]
tTable2 <- tTable1

for(inSubject in uSubjects){
    tSubTable1 <- filter(filteredSet, subject_num == inSubject)
    
    tActTable1 <- filteredSet[-c(1:nrow(filteredSet)), ]
    for(inActivity in uActivities){
        tActTable1 <- filter(tSubTable1, activity_name == inActivity)
        subjectActivitySummary <- summarise_each(tActTable1, funs(mean))
        mainSummaryTable <- rbind(mainSummaryTable, tActTable1)
        #tActTable1 <- tActTable1[-c(1:nrow(tActTable1)), ]s
    }
}