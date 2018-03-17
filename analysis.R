library(dplyr)

#local File Location
FilePath = "./UCI HAR Dataset"

#read files.

paste
x_train <- read.table(paste0(FilePath,"/train/X_train.txt"),header = FALSE)
y_train <- read.table(paste0(FilePath,"/train/y_train.txt"),header = FALSE)
sub_train <- read.table(paste0(FilePath,"/train/subject_train.txt"),header = FALSE)

x_test <- read.table(paste0(FilePath,"/test/X_test.txt"),header = FALSE)
y_test <- read.table(paste0(FilePath,"/test/y_test.txt"),header = FALSE)
sub_test <- read.table(paste0(FilePath,"/test/subject_test.txt"),header = FALSE)

features_Avl <- read.table(paste0(FilePath,"/features.txt"),header = FALSE)
activity_labels <- read.table(paste0(FilePath,"/activity_labels.txt"),header = FALSE)

##name the columns
#4. Appropriately labels the data set with descriptive variable names.
colnames(x_test)<- as.character(features_Avl[,2])
colnames(y_test)<- "ActivityNo"
colnames(sub_test) <- "RepNo"

colnames(x_train)<- as.character(features_Avl[,2])
colnames(y_train)<- "ActivityNo"
colnames(sub_train) <- "RepNo"

colnames(activity_labels) <- c("ActivityNo", "ActivityName")


#merge 3 different file

total_Train <- cbind(sub_train,y_train,x_train)
total_test  <- cbind(sub_test,y_test,x_test)

#Total Data
#1. Merges the training and the test sets to create one data set.

total_data<- rbind(total_test,total_Train)

#2. Extracts only the measurements on the mean and standard deviation for each measurement.
DataColNames <- colnames(total_data)
FIlterCol <-  (grepl("RepNo",DataColNames) | grepl("ActivityNo",DataColNames) | grepl("mean()",DataColNames) | grepl("std()",DataColNames)) & ! grepl("meanFreq()",DataColNames)
MeanStd_data <- total_data[,FIlterCol == TRUE]

#3. Uses descriptive activity names to name the activities in the data set
DataWithActivityName <- merge(activity_labels,MeanStd_data,by = "ActivityNo", ALL=TRUE)

#4. done above

#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable 
NewData <- aggregate(. ~ RepNo+ActivityName,DataWithActivityName,mean)
NewData <- arrange(NewData,RepNo,ActivityNo)


#   for each activity and each subject.
write.table(NewData, "TidySet.txt", row.name=FALSE)



