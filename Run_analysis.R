getwd()

setwd("D:\study\coursera\3.getting and cleaning data\UCI HAR Dataset\test")

s_test<-read.table("subject_test.txt")

x_test<-read.table("X_test.txt")

y_test<-read.table("y_test.txt")

setwd("D:\study\coursera\3.getting and cleaning data\UCI HAR Dataset\train")

s_train<-read.table("subject_train.txt")

x_train<-read.table("X_train.txt")

y_train<-read.table("y_train.txt")

setwd("D:\study\coursera\3.getting and cleaning data\UCI HAR Dataset")

activity_labels <- read.table("activity_labels.txt")

features <- read.table("features.txt")

##1. MERGING DATA

data_set<-rbind(x_train,x_test)

View(data_set)

2. Extracts only the measurements on the mean and standard deviation for each measurement.
##Create a vector of only mean and std, use the vector to subset.

MeanStdOnly <- grep("mean()|std()", features[, 2])

data_set <- data_set[,MeanStdOnly]

##4. Appropriately labels the data set with descriptive activity names.
##Create vector of "Clean" feature names by getting rid of "()" apply to the dataSet to rename labels.

CleanFeatureNames <- sapply(features[, 2], function(x) {gsub("[()]", "",x)})

names(data_set) <- CleanFeatureNames[MeanStdOnly]

##combine test and train of subject data and activity data, give descriptive lables. Using rbind() to do so.

subject <- rbind(s_train, s_test)

names(subject) <- 'subject'

activity <- rbind(y_train, y_test)

names(activity) <- 'activity'

##Combine subject, activity, and mean and std only data set to create final data set.

data_set <- cbind(subject,activity, data_set)

##3. Uses descriptive activity names to name the activities in the data set group the activity column of data_set, 
##re-name lable of levels with activity_levels, and apply it to dataSet.

act_group <- factor(data_set$activity)

levels(act_group) <- activity_labels[,2]

data_set$activity <- act_group

##5. Creates a second, New tidy data set with the average of each variable for each activity and each subject.

##Load Reshape2 package
library("reshape2")

##melt data to tall skinny data and cast means.Writting the tidy data to the working directory as "tidy_data.txt"

baseData <- melt(data_set,(id.vars=c("subject","activity")))

NEWDataSet <- dcast(baseData, subject + activity ~ variable, mean)

names(NEWDataSet)[-c(1:2)] <- paste("[mean of]" , names(NEWDataSet)[-c(1:2)] )

write.table(NEWDataSet, "tidy_data.txt", sep = ",")
