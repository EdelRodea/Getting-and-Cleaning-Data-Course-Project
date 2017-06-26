######################################################################################
####    Getting and Cleaning Data Course Project based on                         ####
####    Human Activity Recognition Using Smartphones Data Set                     ####
####    Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and             ####
####    Jorge L. Reyes-Ortiz.                                                     ####
####    A Public Domain Dataset for Human Activity Recognition Using Smartphones. ####
####    21th European Symposium on Artificial Neural Networks,                    ####
####    Computational Intelligence and Machine Learning, ESANN 2013.              ####
####    Bruges, Belgium 24-26 April 2013.                                         ####
######################################################################################

#Data downloaded date: "Sun Jun 25 12:27:38 2017"

#Reading training and test data sets from .txt files
traindata = read.table("X_train.txt")
testdata = read.table("X_test.txt")

#################################################################################
#####    1.	Merges the training and the test sets to create one data set.   #####
#################################################################################

#Merging training and test sets
data <- rbind(traindata, testdata) #merged data set
dim(data) #10299 rows and 561 columns

#Reading measurmenets labels form .txt file
features = read.table("features.txt")

#Reading tranining and test labels form .txt files
trainlabels = read.table("y_train.txt")
testlabels = read.table("y_test.txt")

#Merging the training and the test labels.
labels <- rbind(trainlabels, testlabels) #merged labels
names(labels)<- c("activity") #variable named activity
dim(labels) #10299 rows and 1 columns

#Adding variable activity to data set
data <- cbind(data, labels)
dim(data) #10299 rows and 562 columns

#########################################################
####    2.	Extracts only the measurements on the    ####
####mean and standard deviation for each measurement.####
#########################################################

#Identifying measurements on the mean and standard deviation 
measurements <- as.character(features$V2)   #converting to character
grepl("mean\\(\\)|std\\(\\)", measurements) #logical vector for measurements of interest
measurements[grepl("mean\\(\\)|std\\(\\)", measurements)] #selected only mean() or std() measurementes

#Subsetting data with only mean() and std() measurementes
selecteddata<-data[,grepl("mean\\(\\)|std\\(\\)", measurements)]
dim(selecteddata)# 10299 rows and 67 columns

#######################################################################################
####    3. Uses descriptive activity names to name the activities in the data set  ####
#######################################################################################

#Reading activity labels form .txt file
activitylabels = read.table("activity_labels.txt")

#Assingning descriptive activity names to name the activities in the data set
selecteddata$activity <- as.factor(selecteddata$activity) #convert variable as factor
levels(selecteddata$activity) <- activitylabels$V2

#####################################################################################
####    4.	Appropriately labels the data set with descriptive variable names.   ####
#####################################################################################

#Assigning descrptive variable names
varnames <- measurements[grepl("mean\\(\\)|std\\(\\)", measurements)]
names(selecteddata) <- c(varnames, "activity")
names(selecteddata)
        
###################################################################                 
####    5.	From the data set in step 4, creates a second,     ####
####    independent tidy data set with the average             ####
####    of each variable for each activity and each subject.   ####
###################################################################

#Second data set data set with only the average of each variable 
#for each activity and each subject.
!grepl("std\\(\\)", names(selecteddata)) #logical condition for variables in the second data 
finaldata <- selecteddata[,!grepl("std\\(\\)", names(selecteddata))]
dim(finaldata) #10299 rows and 34 columns
names(finaldata)

#Reading subject .txt files
trainsubject = read.table("subject_train.txt")
testsubject = read.table("subject_test.txt")
subject <- rbind(trainsubject, testsubject)
names(subject) <- c("subject")
dim(subject)

####Adding subject ID to data
finaldata <- cbind(subject, finaldata) 
dim(finaldata)

#### Calculating mean by subject and activity
library(reshape2)
melteddata <- melt(finaldata, id=c("subject", "activity"))
tidydata <- dcast(melteddata, subject+activity ~ variable, mean)

#Creating .txt file
write.table(tidydata, "tidydata.txt", row.name=FALSE)
