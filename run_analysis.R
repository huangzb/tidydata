library(data.table)
library(reshape2)

## Download and unzip dataset.
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile ="./ProjectDataset.zip")
unzip("./ProjectDataset.zip")
dateDownloaded <- date()

## Merges the training and the test sets to create one data set.
WD <- getwd()
setwd(paste(WD, "/UCI\ HAR\ Dataset", sep = ""))

train <- read.table("./train/X_train.txt")
train_labels <- read.table("./train/y_train.txt")
colnames(train_labels)[1] <- "Activity"
subject_train <- read.table("./train/subject_train.txt")
colnames(subject_train)[1] <- "Subject"
train <- cbind(train, train_labels, subject_train) 

test <- read.table("./test/X_test.txt")
test_labels <- read.table("./test/y_test.txt")
colnames(test_labels)[1] <- "Activity"
subject_test <- read.table("./test/subject_test.txt")
colnames(subject_test)[1] <- "Subject"
test <- cbind( test, test_labels, subject_test)

total <- rbind(train, test)
        
features <- read.table("./features.txt")
activity_labels <- read.table("./activity_labels.txt")

## Extracts only the measurements on the mean and standard deviation for each measurement. 
mean_index <- grep("mean", features$V2)
std_index <- grep("std", features$V2)
merged_index <- sort(c(mean_index, std_index))
subtotal <- total[,merged_index]

subtotal <- cbind(subtotal, total$Activity, total$Subject)
colnames(subtotal)[which(names(subtotal) == "total$Activity")] <- "Activity"
colnames(subtotal)[which(names(subtotal) == "total$Subject")] <- "Subject"

## Uses descriptive activity names to name the activities in the data set
subtotal$Activity <- factor(subtotal$Activity,
                    levels = activity_labels$V1,
                    labels = activity_labels$V2)

## Appropriately labels the data set with descriptive variable names. 
colnames(subtotal)[1:length(merged_index)] <- as.character(features$V2[merged_index])

## Creates a second, independent tidy data set with the average of each variable 
## for each activity and each subject.
subtotal <- data.table(subtotal)
subtotalMelt <- melt(subtotal, id=c("Activity", "Subject"), 
                     measure.vars = colnames(subtotal)[1:length(merged_index)])

tidy <- dcast(subtotalMelt, Activity + Subject ~ variable, mean)
write.table(tidy, "./tidy.txt")