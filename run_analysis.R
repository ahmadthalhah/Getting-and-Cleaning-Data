#setwd("C:/Users/administrator.000/Downloads/Coursera Data Scientist/Module 3 - Getting and Cleaning Data")
#No. 1 - Merges the training and the test sets to create one data set.

if (!require("data.table")) {
  install.packages("data.table")
}
if (!require("reshape2")) {
  install.packages("reshape2")
}

require("data.table")
require("reshape2")

# Load and process X_train & y_train data.
trainingData <- read.table("UCI_HAR_Dataset/train/X_train.txt")
dim(trainingData)
head(trainingData)
trainingData2 <- read.table("UCI_HAR_Dataset/train/y_train.txt")
table(trainingData2)
trainingSubject <- read.table("UCI_HAR_Dataset/train/Subject_train.txt")
testData <- read.table("UCI_HAR_Dataset/test/X_test.txt")
dim(testData)
testData2 <- read.table("UCI_HAR_Dataset/test/y_test.txt")
table(testData2)
testSubject <- read.table("UCI_HAR_Dataset/test/Subject_test.txt")
joinData <- rbind(trainingData, testData)
dim(joinData)
joinData2 <- rbind(trainingData2, testData2)
dim(joinData2)
joinSubject <- rbind(trainingSubject, testSubject)
dim(joinSubject)

#No.2 - Extracts only the measurements on the mean and standard deviation for each measurement.

features <- read.table("UCI_HAR_Dataset/features.txt") 
dim(features) 
meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2]) 
length(meanStdIndices)
joinData <- joinData[, meanStdIndices] 
dim(joinData)
names(joinData) <- gsub("\\(\\)", "", features[meanStdIndices, 2]) 
names(joinData) <- gsub("mean", "Mean", names(joinData)) 
names(joinData) <- gsub("-", "", names(joinData))

#No.3 - Uses descriptive activity names to name the activities in the data set

activity <- read.table("UCI_HAR_Dataset/activity_labels.txt") 
activity[, 2] <- tolower(gsub("_", "", activity[, 2])) 
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8)) 
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8)) 
activityLabel <- activity[joinData2[, 1], 2] 
joinData2[, 1] <- activityLabel 
names(joinData2) <- "activity" 

#No.4 - Appropriately labels the data set with descriptive variable names. 

names(joinSubject) <- "subject" 
cleanedData <- cbind(joinSubject, joinData2, joinData) 
dim(cleanedData)
write.table(cleanedData, "merged_data.txt")  

#No.5 - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
subjectLen <- length(table(joinSubject)) # 30 
activityLen <- dim(activity)[1] # 6 
columnLen <- dim(cleanedData)[2] 
result <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen)  
result <- as.data.frame(result) 
colnames(result) <- colnames(cleanedData) 
row <- 1 
for(i in 1:subjectLen) { 
  for(j in 1:activityLen) { 
    result[row, 1] <- sort(unique(joinSubject)[, 1])[i] 
    result[row, 2] <- activity[j, 2] 
    bool1 <- i == cleanedData$subject 
    bool2 <- activity[j, 2] == cleanedData$activity 
    result[row, 3:columnLen] <- colMeans(cleanedData[bool1&bool2, 3:columnLen]) 
    row <- row + 1 
  } 
} 
head(result) 
write.table(result, "tidy_data.txt")  

