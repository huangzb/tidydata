Data Dictionary - 
========================================================
The original data is from Reference
Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

I performed the following transformation to the original to get tidy data. 

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement. 
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names. 
5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

Most of the variable names are extracted from features.txt for simplicity even though the average of each variable for each activity and each subject is calculated.

The variable names are described as follows:

Activity
Six activities performed during the experiments.
1. WALKING            
2. WALKING_UPSTAIRS  
3. WALKING_DOWNSTAIRS 
4. SITTING           
5. STANDING 
6. LAYING

Subject
An identifier of the subject who carried out the experiment. Its range is from 1 to 30.

tBodyAcc-mean()-X
Mean of body acceleration signals in X direction.

tBodyAcc-mean()-Y
Mean of body acceleration signals in Y direction.

tBodyAcc-mean()-Z
Mean of body acceleration signals in Z direction.

tBodyAcc-std()-X
Standard deviation of body acceleration signals in x direction.

tBodyAcc-std()-Y
Standard deviation of body acceleration signals in y direction.

tBodyAcc-std()-Z
Standard deviation of body acceleration signals in z direction.

tGravityAcc-mean()-X
Mean of gravity acceleration signals in X direction.

tGravityAcc-mean()-Y
Mean of gravity acceleration signals in y direction.

tGravityAcc-mean()-Z
Mean of gravity acceleration signals in z direction.

tGravityAcc-std()-X
Standard deviation of gravity acceleration signals in x direction.

tGravityAcc-std()-Y
Standard deviation of gravity acceleration signals in y direction.

tGravityAcc-std()-Z
Standard deviation of gravity acceleration signals in z direction.

tBodyAccJerk-mean()-X
Mean of body linear acceleration derived jerk signals in x direction.

tBodyAccJerk-mean()-Y
Mean of body linear acceleration derived jerk signals in y direction.

tBodyAccJerk-mean()-Z
Mean of body linear acceleration derived jerk signals in z direction.

tBodyAccJerk-std()-X
Standard deviation of body linear acceleration derived jerk signals in x direction.

tBodyAccJerk-std()-Y
Standard deviation of body linear acceleration derived jerk signals in y direction.

tBodyAccJerk-std()-Z
Standard deviation of body linear acceleration derived jerk signals in y direction.

tBodyGyro-mean()-X
Mean of body angular velocity in x direction.

tBodyGyro-mean()-Y
Mean of body angular velocity in y direction.

tBodyGyro-mean()-Z
Mean of body angular velocity in z direction.

tBodyGyro-std()-X
Standard deviation of body angular velocity in x direction.

tBodyGyro-std()-Y
Standard deviation of body angular velocity in y direction.

tBodyGyro-std()-Z
Standard deviation of body angular velocity in z direction.

tBodyGyroJerk-mean()-X
Mean of body angular velocity derived jerk signal in x direction.

tBodyGyroJerk-mean()-Y
Mean of body angular velocity derived jerk signal in y direction.

tBodyGyroJerk-mean()-Z
Mean of body angular velocity derived jerk signal in z direction.

tBodyGyroJerk-std()-X
Standard deviation of body angular velocity derived jerk signal in x direction.

tBodyGyroJerk-std()-Y
Standard deviation of body angular velocity derived jerk signal in y direction.

tBodyGyroJerk-std()-Z
Standard deviation of body angular velocity derived jerk signal in z direction.

tBodyAccMag-mean()
Mean of magnitude of body linear acceleration.

tBodyAccMag-std()
Standard deviation of magintude of body linear acceleation.

tGravityAccMag-mean()
Mean of magnitude of gravity acceleration.

tGravityAccMag-std()
Standard deviation of gravity acceleration.

tBodyAccJerkMag-mean()
Mean of magnitude of body linear acceleration derived jerk signal.

tBodyAccJerkMag-std()
Standard deviation of body linear acceleration derived jerk signal.

tBodyGyroMag-mean()
Mean of magnitude of body angular velocity.

tBodyGyroMag-std()
Standard deviation of magnitude of body angular velocity.

tBodyGyroJerkMag-mean()
Mean of magnitude of body angular velocity derived jerk signal.

tBodyGyroJerkMag-std()
Standard deviation of magnitude of body angular velocity derived jerk signal.

fBodyAcc-mean()-X
Mean of body linear acceleration FFT signal in x direction.

fBodyAcc-mean()-Y
Mean of body linear acceleration FFT signal in y direction.

fBodyAcc-mean()-Z
Mean of body linear acceleration FFT signal in z direction.

fBodyAcc-std()-X
Standard deviation of body linear acceleration FFT signal in x direction.

fBodyAcc-std()-Y
Standard deviation of body linear acceleration FFT signal in y direction.

fBodyAcc-std()-Z
Standard deviation of body linear acceleration FFT signal in z direction.

fBodyAcc-meanFreq()-X
Mean frequency of body linear acceleration FFT signal in x direction.

fBodyAcc-meanFreq()-Y
Mean frequency of body linear acceleration FFT singal in y direction.

fBodyAcc-meanFreq()-Z
Mean frequency of body linear acceleration FFT singal in z direction.

fBodyAccJerk-mean()-X
Mean of body linear acceleration derived jerk FFT signal in x direction. 

fBodyAccJerk-mean()-Y
Mean of body linear acceleration derived jerk FFT signal in y direction. 

fBodyAccJerk-mean()-Z
Mean of body linear acceleration derived jerk FFT signal in z direction. 

fBodyAccJerk-std()-X
Standard deviation of body linear acceleration derived jerk FFT signal in x direction. 

fBodyAccJerk-std()-Y
Standard deviation of body linear acceleration derived jerk FFT signal in y direction. 

fBodyAccJerk-std()-Z
Standard deviation of body linear acceleration derived jerk FFT signal in z direction. 

fBodyAccJerk-meanFreq()-X
Mean frequency of body linear acceleration derived jerk FFT signal in x direction.

fBodyAccJerk-meanFreq()-Y
Mean frequency of body linear acceleration derived jerk FFT signal in y direction.

fBodyAccJerk-meanFreq()-Z
Mean frequency of body linear acceleration derived jerk FFT signal in z direction.

fBodyGyro-mean()-X
Mean of body angular velocity FFT signal in x direction.

fBodyGyro-mean()-Y
Mean of body angular velocity FFT signal in y direction.

fBodyGyro-mean()-Z
Mean of body angular velocity FFT signal in z direction.

fBodyGyro-std()-X
Standard deviation of body angular velocity FFT signal in x direction.

fBodyGyro-std()-Y
Standard deviation of body angular velocity FFT signal in y direction.

fBodyGyro-std()-Z
Standard deviation of body angular velocity FFT signal in z direction.

fBodyGyro-meanFreq()-X
Mean frequency of body angular velocity FFT signal in x direction.

fBodyGyro-meanFreq()-Y
Mean frequency of body angular velocity FFT signal in y direction.

fBodyGyro-meanFreq()-Z
Mean frequency of body angular velocity FFT signal in z direction.

fBodyAccMag-mean()
Mean of magnitude of body linear acceleration FFT signal.

fBodyAccMag-std()
Standard deviation of magnitude of  body linear acceleration FFT signal.

fBodyAccMag-meanFreq()
Mean frequency of magnitude of  body linear acceleration FFT signal.

fBodyBodyAccJerkMag-mean()
Mean of magnitude of  body linear acceleration FFT signal.

I do not know the meaning of the following signals as it is not clearly explained in the given dataset. 

fBodyBodyAccJerkMag-std()
fBodyBodyAccJerkMag-meanFreq()
fBodyBodyGyroMag-mean()
fBodyBodyGyroMag-std()
fBodyBodyGyroMag-meanFreq()
fBodyBodyGyroJerkMag-mean()
fBodyBodyGyroJerkMag-std()
fBodyBodyGyroJerkMag-meanFreq()


