##########################################################################################################

## setwd("C:/CourseEra/GetCleanData/getdata-projectfiles-UCI-HAR-Dataset/UCI-HAR-Dataset")


## Q1) Merges the training and the test sets to create one data set.

## Load training data set

## Read all the files provided and appropriatly name the data frame variable names and column names
FeatureNameMapping <- read.csv("features.txt",sep="",header=F,col.names=c("FeatureId","FeatureName"))

ActivityNameMapping <- read.csv("activity_labels.txt",sep="",header=F,col.names=c("ActivityId","ActivityName"))

ActivityId <- read.csv("./train/y_train.txt",sep="",header=F,col.names=c("ActivityId"))

SubjectId <- read.csv("./train/subject_train.txt",sep="",header=F,col.names=c("SubjectId"))
                               
FeatureData <- read.csv("./train/X_train.txt",sep="",header=F)

## As the feature data is not provided with header, assign the column names provided in feature.txt file and loaded in FeatureNameMapping
names(FeatureData) <- FeatureNameMapping$FeatureName

#creat a new column to indicate that the rows being added are from the "Training" dataset
TrainVec <- sample(c("Training"),size=nrow(FeatureData),replace=TRUE)

TrainFac <- factor(TrainVec,levels=c("Training","Testing"))

#add the indicator factor column at the beginning of the feature.data, also the subjectId and ActiviryId provided in different files
TrainActivityData <- cbind(TrainFac,SubjectId,ActivityId,FeatureData)

#Guve an descriptive name to the data indicator
names(TrainActivityData)[1] <- paste("IsDataFromTrainingOrTesting")
                               

## Load test data set

## Read all the files provided and appropriatly name the data frame variable names and column names
ActivityId <- read.csv("./test/y_test.txt",sep="",header=F,col.names=c("ActivityId"))

SubjectId <- read.csv("./test/subject_test.txt",sep="",header=F,col.names=c("SubjectId"))

FeatureData <- read.csv("./test/X_test.txt",sep="",header=F)

names(FeatureData) <- FeatureNameMapping$FeatureName

#create a new column to indicate that the rows being added are from the "Testing" dataset

TestingVec <- sample(c("Testing"),size=nrow(FeatureData),replace=TRUE)

TestingFac <- factor(TestingVec,levels=c("Training","Testing"))

#add the indicator factor column at the beginning of the feature.data, also the subjectId and ActiviryId provided in different files
TestActivityData <- cbind(TestingFac,SubjectId,ActivityId,FeatureData)

#Guve an descriptive name to the data indicator, same name as the training data, as these two needs to be merged 
names(TestActivityData)[1] <- paste("IsDataFromTrainingOrTesting")


## Merge training and test data set, row wise

MergedData <- rbind(TrainActivityData,TestActivityData)





## Q2) Extracts only the measurements on the mean and standard deviation for each measurement. 

## Remove the cols with feature names meanFreq
remove <- grep("meanFreq",names(MergedData))

## Retain col#1 "Training/Testing", col#2  SubjectId and Col#3 ActivityId, along with feature names that match mean and std 
neededCols <- c(c(1,2,3),grep("mean",names(MergedData)),grep("std",names(MergedData)))

MeanAndStdOnlyData <- MergedData[,setdiff(neededCols,remove)]






## Q3) Uses descriptive activity names to name the activities in the data set

## merge the feature data with the activity mapping provided
MeanAndStdOnlyDataWithActivityName <- merge(ActivityNameMapping,MeanAndStdOnlyData)







## Q4) Appropriately labels the data set with descriptive variable names.

## remove "()" which are not necessary and may sound cryptic to non-programmers
names(MeanAndStdOnlyDataWithActivityName) <- sub("\\(\\)","",names(MeanAndStdOnlyDataWithActivityName))

## replace std with a more descriptive name for standard deviation
names(MeanAndStdOnlyDataWithActivityName) <- sub("std","standard-deviation",names(MeanAndStdOnlyDataWithActivityName))






## Q5) Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## use aggregate function to object average of different measures across activity and subject dimensions
aggdata <-aggregate(MeanAndStdOnlyDataWithActivityName, by=list(MeanAndStdOnlyDataWithActivityName$SubjectId,MeanAndStdOnlyDataWithActivityName$ActivityName),FUN=mean, na.rm=TRUE)

## Give appropriate names to the added columns due to aggregation
names(aggdata)[1] = paste("Subject")
names(aggdata)[2] = paste("Activity")

## Remove duplicate columns and columns that are not necessary 
aggdata <- aggdata[,c(1,2,c(7:ncol(aggdata)))]

##prepend avg at the beginning of all col names starting col 3, as col#1 is SubjectNumber and Col#2 is Activity, all others are Avg of measures
names(aggdata)[3:ncol(aggdata)] <- sub("^","AvgOf-",names(aggdata)[3:ncol(aggdata)])

##Write a text file for the tidy data
write.table(aggdata, file="TidyData-Wearable.txt",sep="\t", row.names = FALSE, quote = FALSE)

