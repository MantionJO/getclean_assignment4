##Getting and cleaning data-Coursera
##Assignment-week4-run_analysis.R
##Human Activity Recognition Using Smartphones Data Set

# reading the train data
X_train <- read.table("C:/Users/MANTIONISM/Desktop/coursera/Data scientist/Getting and cleaning data/week4/eval/assignment4/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt")
Y_train <- read.table("C:/Users/MANTIONISM/Desktop/coursera/Data scientist/Getting and cleaning data/week4/eval/assignment4/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("C:/Users/MANTIONISM/Desktop/coursera/Data scientist/Getting and cleaning data/week4/eval/assignment4/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt")

#reading test data
subject_test <- read.table("C:/Users/MANTIONISM/Desktop/coursera/Data scientist/Getting and cleaning data/week4/eval/assignment4/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt")
X_test <- read.table("C:/Users/MANTIONISM/Desktop/coursera/Data scientist/Getting and cleaning data/week4/eval/assignment4/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt")
Y_test <- read.table("C:/Users/MANTIONISM/Desktop/coursera/Data scientist/Getting and cleaning data/week4/eval/assignment4/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt")

#reading variables features
var_label <- read.table("C:/Users/MANTIONISM/Desktop/coursera/Data scientist/Getting and cleaning data/week4/eval/assignment4/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/features.txt")

#Merges the training and the test sets to create one data set.
merged_train <- cbind(subject_train,X_train, Y_train)
merged_test <- cbind(subject_test,X_test, Y_test)
total_merged <- rbind(merged_train, merged_test)
namesdf <- c("subject", var_label[,2], "Activities")
names(total_merged) <- namesdf

#Extracting only the measurements on the mean and standard deviation for each measurement. 
extracted_names <- namesdf[grepl("mean()", namesdf) & !grepl("meanFreq",namesdf)| grepl("std", namesdf)]
extracted_mean_std <- total_merged[,c("subject",extracted_names, "Activities")]

#Uses descriptive activity names to name the activities in the data set
activity_labels <- read.table("C:/Users/MANTIONISM/Desktop/coursera/Data scientist/Getting and cleaning data/week4/eval/assignment4/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt")
extracted_mean_std$Activities <- factor(extracted_mean_std$Activities, levels = activity_labels[,1], labels = activity_labels[,2])

#Appropriately labels the data set with descriptive variable names
#split colnames and special characters
extracted_names <- gsub("[\\(\\)-]", "", extracted_names)
#writing full words of variables names
extracted_names <- gsub("^t", "time", extracted_names)
extracted_names <- gsub("^f", "frequency", extracted_names)
extracted_names <- gsub("Acc", "accelerometer", extracted_names)
extracted_names <- gsub("Gyro", "gyroscope", extracted_names)
extracted_names <- gsub("Mag", "magnitude", extracted_names)
extracted_names <- gsub("Mag", "magnitude", extracted_names)
extracted_names <- gsub("std", "StandardDeviation", extracted_names)
extracted_names <- gsub("BodyBody", "Body", extracted_names)
#labeling the data set with descriptive variable names
names(extracted_mean_std) <- c("subject",extracted_names, "Activities")

#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# using reshape2 package to melt and dcast the data set before creating independant tidy data set with the average of each variable for each activity and each subject.
library(reshape2)
activity_melt <- melt(extracted_mean_std, id=c("subject", "Activities"),
mesure.vars=extracted_mean_std[,-c("subject", "Activities")])
 tidydataMeans <- dcast(activity_melt, subject + Activities~variable,fun.aggregate = mean)

# writing independant tidy data set with the average of each variable for each activity and each subject.
write.table(tidydataMeans, "tidy_data_Means.txt", row.names = F, quote = F)
