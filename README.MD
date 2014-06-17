Zubin Huang 
06/16/2014

run_analysis.R that does the following:

1. Merges the training and the test sets to create one data set.
I read in the test and train data as well as the activity and subject information. I used cbind to combine the activity and subject columns with the measurement columns. Then I used rbind to combine test and train data to total dataset.

2. Extracts only the measurements on the mean and standard deviation for each measurement. 
I used grep to get the index of the features containing "mean" and "std". Then I used the index to subset the total dataset to subtotal dataset. I used cbind to assemble activity and suject columns to the subtotal dataset.

3. Uses descriptive activity names to name the activities in the data set.
I used factor function to change levels 1 to 6 to corresponding activity names in the subtotal dataset.

4. Appropriately labels the data set with descriptive variable names. 
I used colnames function to assign the names in the features dataset to corresponding columns from the index of features containing "mean" and "std".

5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
I used melt and dcast to reshape the dataset to average of each variable for each activity and each subject and exported the final result tidy dataset.

Reference

Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012