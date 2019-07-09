library(dplyr)

######################################
# Downloading and unzipping dataset  #
######################################
  
  #setting dataset URL
  
  URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  
  #downloadin dataset
  
  download.file(URL, "./dataset.zip", method="curl")
  
  #unzipping dataset
  
  unzip(zipfile="./Dataset.zip")
  
  
  
  ###########################################
  # Reading the training and the test sets  #
  ###########################################  
  
  #reading training datasets into tables
  
  x_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
  y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
  subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
  
  #reading testing datasets into tables:
  
  x_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
  y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
  subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
  
  ###########################################
  # Setting column names                    #
  ########################################### 
  
  #reading feature vector:
  
  features <- read.table('./UCI HAR Dataset/features.txt')

  #reading activity labels:
  activityLabels = read.table('./UCI HAR Dataset/activity_labels.txt')

  #Assigning column names for training datasets:
  colnames(x_train) <- features[,2]  #reading columun names from features vector 
  colnames(y_train) <-"activityId"
  colnames(subject_train) <- "subjectId"
  
  #Assigning column names for test datasets:
  colnames(x_test) <- features[,2] #reading columun names from features vector 
  colnames(y_test) <- "activityId"
  colnames(subject_test) <- "subjectId"
  
  #setting column names of activityLabels data frame
  colnames(activityLabels) <- c('activityId','activityType')

  
  ########################################################################################
  #                                                                                      #
  #  1. Merging the training and the test sets                                           #
  #                                                                                      # 
  ########################################################################################

  # merging train data frames into single data frame with column binding
  merge_train <- cbind(y_train, subject_train, x_train)  
  
  # merging train data frames into single data frame with column binding
  merge_test <- cbind(y_test, subject_test, x_test)      
  
  # merging all training and test data frames into single data frame with row binding
  merged_data <- rbind(merge_train, merge_test)          
  
  ########################################################################################
  #                                                                                      #
  # 2.Extracting the measurements on the mean and standard deviation for each measurement#
  #                                                                                      #
  # Keeping only mean and std for each measurement                                       #
  ########################################################################################

  mean_and_std <- (grepl("activityId|subjectId|mean|std", colnames(merged_data)))
  
  DatasetForMeanAndStd <- merged_data[ , mean_and_std == TRUE]
  
  ########################################################################################
  #                                                                                      #
  # 3. Uses descriptive activity names to name the activities in the data set            # 
  #                                                                                      #    
  # replacing activity values with descriptive names 
  #                                                                                      #
  ########################################################################################
  
  DatasetForMeanAndStd$activityId <- factor(DatasetForMeanAndStd$activityId, 
                                            levels = activityLabels[, 1], 
                                            labels = activityLabels[, 2])
  

  ########################################################################################
  #                                                                                      #
  #  4. Appropriately labels the data set with descriptive variable names                #
  #                                                                                      # 
  ########################################################################################  
  
  names(DatasetForMeanAndStd)[1] = "activity"
  names(DatasetForMeanAndStd)[2] = "subject"
  names(DatasetForMeanAndStd)<-gsub("Acc", "Accelerometer", names(DatasetForMeanAndStd))
  names(DatasetForMeanAndStd)<-gsub("Gyro", "Gyroscope", names(DatasetForMeanAndStd))
  names(DatasetForMeanAndStd)<-gsub("BodyBody", "Body", names(DatasetForMeanAndStd))
  names(DatasetForMeanAndStd)<-gsub("Mag", "Magnitude", names(DatasetForMeanAndStd))
  names(DatasetForMeanAndStd)<-gsub("^t", "Time", names(DatasetForMeanAndStd))
  names(DatasetForMeanAndStd)<-gsub("^f", "Frequency", names(DatasetForMeanAndStd))
  names(DatasetForMeanAndStd)<-gsub("tBody", "TimeBody", names(DatasetForMeanAndStd))
  names(DatasetForMeanAndStd)<-gsub("-mean()", "Mean", names(DatasetForMeanAndStd), ignore.case = TRUE)
  names(DatasetForMeanAndStd)<-gsub("-std()", "STD", names(DatasetForMeanAndStd), ignore.case = TRUE)
  names(DatasetForMeanAndStd)<-gsub("-freq()", "Frequency", names(DatasetForMeanAndStd), ignore.case = TRUE)
  names(DatasetForMeanAndStd)<-gsub("angle", "Angle", names(DatasetForMeanAndStd))
  names(DatasetForMeanAndStd)<-gsub("gravity", "Gravity", names(DatasetForMeanAndStd))
  
  
  
  ########################################################################################
  #                                                                                      #
  # 5. From the data set in step 4, creates a second, independent tidy data set          # 
  #    with the average of each variable for each activity and each subject.             #    
  #                                                                                      #
  ########################################################################################
  
  # making second tidy data set
  DatasetForMeanAndStdTidy <- DatasetForMeanAndStd %>% 
    group_by(subject, activity)  %>%
    summarise_all(funs(mean))

  # writing data set in a txt file
  write.table(DatasetForMeanAndStdTidy, "TidyDataSet.txt", row.name=FALSE)
  
  
  