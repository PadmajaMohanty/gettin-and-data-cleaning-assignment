##Codebook.md

Data Source The data is derived from the Human Activity Recognition Using Smartphones Dataset Version 1.0 (UCI HAR) using the steps described in the Readme.

"The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data.

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details."

For each record it is provided: Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration. Triaxial Angular velocity from the gyroscope. A 561-feature vector with time and frequency domain variables. Its activity label. An identifier of the subject who carried out the experiment. The Analysis The original dataset is divided in two parts.

Inertial sensor data Records of activity windows. This analysis uses only the second data set (Records of activity Windows) and only the activity labels, subject information and mean and standard deviation features are used.

The following files are used:

activity_labels.txt - used to map the activity codes to meaningful descriptions of the acitivities features.txt - contains the headings for the 561 columns of the Activity measurement data in X_train/X_test data sets subject_(test/train).txt - used to obtain the subject id (1-30) of the person who performed each activity X_(test/train).txt - various statistics related to the original gyro measurements y_(test/train).txt - the activity labels for the each observation in X_(test/train) dataset (WALKING, STANDING The analysis consists of the following steps:

The training and the test sets are merged to create one data set. The measurements on the mean and standard deviation for each measurement are extracted. Descriptive activity names are added to the activities in the data set The data is relabelled with descriptive variable names. From the data set in step 4, a second, independent tidy data set is created with the average of each variable for each activity and subject.

Tidy datasets provide a standardized way to link the structure of a dataset (its physical layout) with its semantics (its meaning).

Each variable forms a column. Each observation forms a row. Each type of observational unit forms a table.

Since each measurement has an X Y and Z component which vary independently the decision was made not to gather these into a dimenion and value Since the time and frequency features have different units of measurement these were not gathered and split into domain and Feature

Note: The "MeanFreq" and Angular measurements are not included as these are technically not what is requested.

The Dataset The data set consists of 1 table:

MeanByActivityAndSubject.txt (180 x 68) Each row is composed of:

An identifier of the subject who carried out the experiment.

An activity label.

A 66-feature vector representing the averages of the 66 mean and standard deviation measurements for a given Activity and Test Subject

The Data Structure The Columns are described below:

Dimensions SubjectID: ID of the test subject performing the various activities

Type: Integer Values: 1-30 ActivityDescription: Description of the Activity

Type: Factor Values: WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING Measurements The next 66 columns represent the Average of each feature for a given Activity and Subject.

The prefix of Time implies a time domain variable

The prefix of Frequency implies a frequency domain variable

Where the variable has a suffix of (-X/-Y/-Z) it is short hand for 3 distinct measurements for the X,Y and Z axis

The units used for the accelerations (total and body) are 'g's (gravity of earth -> 9.80665 m/seg2).

The gyroscope units are rad/seg.

The values are normalized and bounded within [-1,1].

Time Domain Variables

Time-BodyAccelerometer-Mean(-X/-Y/-Z)

Time-BodyAccelerometer-StdDev(-X/-Y/-Z)

Time-GravityAccelerometer-Mean(-X/-Y/-Z)

Time-GravityAccelerometer-StdDev(-X/-Y/-Z)

Time-BodyAccelerometerJerk-Mean(-X/-Y/-Z)

Time-BodyAccelerometerJerk-StdDev(-X/-Y/-Z)

Time-BodyGyroscope-Mean(-X/-Y/-Z)

Time-BodyGyroscope-StdDev(-X/-Y/-Z)

Time-BodyGyroscopeJerk-Mean(-X/-Y/-Z)

Time-BodyGyroscopeJerk-StdDev(-X/-Y/-Z)

Time-BodyAccelerometerMagnitude-Mean

Time-BodyAccelerometerMagnitude-StdDev

Time-GravityAccelerometerMagnitude-Mean

Time-GravityAccelerometerMagnitude-StdDev

Time-BodyAccelerometerJerkMagnitude-Mean

Time-BodyAccelerometerJerkMagnitude-StdDev

Time-BodyGyroscopeMagnitude-Mean

Time-BodyGyroscopeMagnitude-StdDev

Time-BodyGyroscopeJerkMagnitude-Mean

Time-BodyGyroscopeJerkMagnitude-StdDev

Type: Numeric Values: Normalised and bounded within [-1,1] Frequency Domain Variables

Frequency-BodyAccelerometer-Mean(-X/-Y/-Z)

Frequency-BodyAccelerometer-StdDev(-X/-Y/-Z)

Frequency-BodyAccelerometerJerk-Mean(-X/-Y/-Z)

Frequency-BodyAccelerometerJerk-StdDev(-X/-Y/-Z)

Frequency-BodyGyroscope-Mean(-X/-Y/-Z)

Frequency-BodyGyroscope-StdDev(-X/-Y/-Z)

Frequency-BodyAccelerometerMagnitude-Mean

Frequency-BodyAccelerometerMagnitude-StdDev

Frequency-BodyAccelerometerJerkMagnitude-Mean

Frequency-BodyAccelerometerJerkMagnitude-StdDev

Frequency-BodyGyroscopeMagnitude-Mean

Frequency-BodyGyroscopeMagnitude-StdDev

Frequency-BodyGyroscopeJerkMagnitude-Mean

Frequency-BodyGyroscopeJerkMagnitude-StdDev

Type: Numeric Values: Normalised and bounded within [-1,1] Some sample Data Produced by: str(meanByActivitySubject)

str(meanByActivitySubject)

Classes ‘grouped_df’, ‘tbl_df’, ‘tbl’ and 'data.frame':	180 obs. of 68 variables:

SubjectID : int 1 1 1 1 1 1 2 2 2 2 ... ActivityDescription : Factor w/ 6 levels "WALKING","WALKING_UPSTAIRS",..: 1 2 3 4 5 6 1 2 3 4 ... Time-BodyAccelerometer-Mean-X : num 0.277 0.255 0.289 0.261 0.279 ... Time-BodyAccelerometer-Mean-Y : num -0.01738 -0.02395 -0.00992 -0.00131 -0.01614 ... Time-BodyAccelerometer-Mean-Z : num -0.1111 -0.0973 -0.1076 -0.1045 -0.1106 ... Time-BodyAccelerometer-StdDev-X : num -0.284 -0.355 0.03 -0.977 -0.996 ... Time-BodyAccelerometer-StdDev-Y : num 0.11446 -0.00232 -0.03194 -0.92262 -0.97319 ... Time-BodyAccelerometer-StdDev-Z : num -0.26 -0.0195 -0.2304 -0.9396 -0.9798 ... Time-GravityAccelerometer-Mean-X : num 0.935 0.893 0.932 0.832 0.943 ... Time-GravityAccelerometer-Mean-Y : num -0.282 -0.362 -0.267 0.204 -0.273 ... Time-GravityAccelerometer-Mean-Z : num -0.0681 -0.0754 -0.0621 0.332 0.0135 ... Time-GravityAccelerometer-StdDev-X : num -0.977 -0.956 -0.951 -0.968 -0.994 ... Time-GravityAccelerometer-StdDev-Y : num -0.971 -0.953 -0.937 -0.936 -0.981 ... Time-GravityAccelerometer-StdDev-Z : num -0.948 -0.912 -0.896 -0.949 -0.976 ... Time-BodyAccelerometerJerk-Mean-X : num 0.074 0.1014 0.0542 0.0775 0.0754 ... Time-BodyAccelerometerJerk-Mean-Y : num 0.028272 0.019486 0.02965 -0.000619 0.007976 ... Time-BodyAccelerometerJerk-Mean-Z : num -0.00417 -0.04556 -0.01097 -0.00337 -0.00369 ... Time-BodyAccelerometerJerk-StdDev-X : num -0.1136 -0.4468 -0.0123 -0.9864 -0.9946 ... Time-BodyAccelerometerJerk-StdDev-Y : num 0.067 -0.378 -0.102 -0.981 -0.986 ... Time-BodyAccelerometerJerk-StdDev-Z : num -0.503 -0.707 -0.346 -0.988 -0.992 ... Time-BodyGyroscope-Mean-X : num -0.0418 0.0505 -0.0351 -0.0454 -0.024 ... Time-BodyGyroscope-Mean-Y : num -0.0695 -0.1662 -0.0909 -0.0919 -0.0594 ... Time-BodyGyroscope-Mean-Z : num 0.0849 0.0584 0.0901 0.0629 0.0748 ... Time-BodyGyroscope-StdDev-X : num -0.474 -0.545 -0.458 -0.977 -0.987 ... Time-BodyGyroscope-StdDev-Y : num -0.05461 0.00411 -0.12635 -0.96647 -0.98773 ... Time-BodyGyroscope-StdDev-Z : num -0.344 -0.507 -0.125 -0.941 -0.981 ... Time-BodyGyroscopeJerk-Mean-X : num -0.09 -0.1222 -0.074 -0.0937 -0.0996 ... Time-BodyGyroscopeJerk-Mean-Y : num -0.0398 -0.0421 -0.044 -0.0402 -0.0441 ... Time-BodyGyroscopeJerk-Mean-Z : num -0.0461 -0.0407 -0.027 -0.0467 -0.049 ... Time-BodyGyroscopeJerk-StdDev-X : num -0.207 -0.615 -0.487 -0.992 -0.993 ... Time-BodyGyroscopeJerk-StdDev-Y : num -0.304 -0.602 -0.239 -0.99 -0.995 ... Time-BodyGyroscopeJerk-StdDev-Z : num -0.404 -0.606 -0.269 -0.988 -0.992 ... Time-BodyAccelerometerMagnitude-Mean : num -0.137 -0.1299 0.0272 -0.9485 -0.9843 ... Time-BodyAccelerometerMagnitude-StdDev : num -0.2197 -0.325 0.0199 -0.9271 -0.9819 ... Time-GravityAccelerometerMagnitude-Mean : num -0.137 -0.1299 0.0272 -0.9485 -0.9843 ... Time-GravityAccelerometerMagnitude-StdDev : num -0.2197 -0.325 0.0199 -0.9271 -0.9819 ... Time-BodyAccelerometerJerkMagnitude-Mean : num -0.1414 -0.4665 -0.0894 -0.9874 -0.9924 ... Time-BodyAccelerometerJerkMagnitude-StdDev : num -0.0745 -0.479 -0.0258 -0.9841 -0.9931 ... Time-BodyGyroscopeMagnitude-Mean : num -0.161 -0.1267 -0.0757 -0.9309 -0.9765 ... Time-BodyGyroscopeMagnitude-StdDev : num -0.187 -0.149 -0.226 -0.935 -0.979 ... Time-BodyGyroscopeJerkMagnitude-Mean : num -0.299 -0.595 -0.295 -0.992 -0.995 ... Time-BodyGyroscopeJerkMagnitude-StdDev : num -0.325 -0.649 -0.307 -0.988 -0.995 ... Frequency-BodyAccelerometer-Mean-X : num -0.2028 -0.4043 0.0382 -0.9796 -0.9952 ... Frequency-BodyAccelerometer-Mean-Y : num 0.08971 -0.19098 0.00155 -0.94408 -0.97707 ... Frequency-BodyAccelerometer-Mean-Z : num -0.332 -0.433 -0.226 -0.959 -0.985 ... Frequency-BodyAccelerometer-StdDev-X : num -0.3191 -0.3374 0.0243 -0.9764 -0.996 ... Frequency-BodyAccelerometer-StdDev-Y : num 0.056 0.0218 -0.113 -0.9173 -0.9723 ... Frequency-BodyAccelerometer-StdDev-Z : num -0.28 0.086 -0.298 -0.934 -0.978 ... Frequency-BodyAccelerometerJerk-Mean-X : num -0.1705 -0.4799 -0.0277 -0.9866 -0.9946 ... Frequency-BodyAccelerometerJerk-Mean-Y : num -0.0352 -0.4134 -0.1287 -0.9816 -0.9854 ... Frequency-BodyAccelerometerJerk-Mean-Z : num -0.469 -0.685 -0.288 -0.986 -0.991 ... Frequency-BodyAccelerometerJerk-StdDev-X : num -0.1336 -0.4619 -0.0863 -0.9875 -0.9951 ... Frequency-BodyAccelerometerJerk-StdDev-Y : num 0.107 -0.382 -0.135 -0.983 -0.987 ... Frequency-BodyAccelerometerJerk-StdDev-Z : num -0.535 -0.726 -0.402 -0.988 -0.992 ... Frequency-BodyGyroscope-Mean-X : num -0.339 -0.493 -0.352 -0.976 -0.986 ... Frequency-BodyGyroscope-Mean-Y : num -0.1031 -0.3195 -0.0557 -0.9758 -0.989 ... Frequency-BodyGyroscope-Mean-Z : num -0.2559 -0.4536 -0.0319 -0.9513 -0.9808 ... Frequency-BodyGyroscope-StdDev-X : num -0.517 -0.566 -0.495 -0.978 -0.987 ... Frequency-BodyGyroscope-StdDev-Y : num -0.0335 0.1515 -0.1814 -0.9623 -0.9871 ... Frequency-BodyGyroscope-StdDev-Z : num -0.437 -0.572 -0.238 -0.944 -0.982 ... Frequency-BodyAccelerometerMagnitude-Mean : num -0.1286 -0.3524 0.0966 -0.9478 -0.9854 ... Frequency-BodyAccelerometerMagnitude-StdDev : num -0.398 -0.416 -0.187 -0.928 -0.982 ... Frequency-BodyAccelerometerJerkMagnitude-Mean : num -0.0571 -0.4427 0.0262 -0.9853 -0.9925 ... Frequency-BodyAccelerometerJerkMagnitude-StdDev: num -0.103 -0.533 -0.104 -0.982 -0.993 ... Frequency-BodyGyroscopeMagnitude-Mean : num -0.199 -0.326 -0.186 -0.958 -0.985 ... Frequency-BodyGyroscopeMagnitude-StdDev : num -0.321 -0.183 -0.398 -0.932 -0.978 ... Frequency-BodyGyroscopeJerkMagnitude-Mean : num -0.319 -0.635 -0.282 -0.99 -0.995 ... Frequency-BodyGyroscopeJerkMagnitude-StdDev : num -0.382 -0.694 -0.392 -0.987 -0.995 ... attr(, "vars")= chr "SubjectID" attr(, "drop")= logi TRUEREADING DATA

