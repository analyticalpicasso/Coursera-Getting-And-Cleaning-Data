# Getting And cleaning Data Coursera Project

run_analysis <- function(){
  
  if(require("dplyr")){
    print("dplyr package is available and loaded...")
  } else {
    install.packages("dplyr")
    print("Downloading dplyr...")
    if(require(dplyr)){
      print("dplyr package is installed correctly.")
    } else {
      stop("Installation failed!")
    }
  }  
  
  if(require("downloader")){
    print("downloader package is available and loaded...")
  } else {
    install.packages("downloader")
    print("Downloading downloader...")
    if(require(downloader)){
      print("downloader package is installed correctly.")
    } else {
      stop("Installation failed!")
    }
  }
  
  data.download <- function(){
    download.url <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
    download.file <- 'dataset.zip'
    download(download.url,download.file)
    unzip(download.file)
  }
  
  #Step 1: Merge the training and test sets to create one data set
  setwd("~/GitHub/Coursera-Getting-And-Cleaning-Data//UCI HAR Dataset")
  x_train <- read.table("train/X_train.txt")
  y_train <- read.table("train/y_train.txt")
  subject_train <- read.table("train/subject_train.txt")
  
  x_test <- read.table("test/X_test.txt")
  y_test <- read.table("test/y_test.txt")
  subject_test <- read.table("test/subject_test.txt")
  
  # create merged dataset for X
  x.merged <- rbind(x_train, x_test)
  
  # create merged dataset for y
  y.merged <- rbind(y_train, y_test)
  
  # create merged dataset for subject
  subject.merged <- rbind(subject_train, subject_test)
  
  #Step 2:Extracts only the measurements on the mean and standard deviation for each measurement.
  features <- read.table("features.txt")

  mean.std.features <- grep("-(mean|std)\\(\\)", features[, 2])
  
  # subset the dataset based on  mean and standard deviation columns
  x.merged <- x.merged[, mean.std.features]

  # redefine the column names
  names(x.merged) <- features[mean.std.features, 2]
  
  #Step 3:Uses descriptive activity names to name the activities in the data set
  activity.labels <- read.table("activity_labels.txt")
  
  # update values with correct activity names
  y.merged[, 1] <- activity.labels[y.merged[, 1], 2]
  
  # Appropriate column names
  names(y.merged) <- "activity"
  
  #Step 4: Appropriately labels the data set with descriptive variable names. 
  names(subject.merged) <- "subject"
  
  # Merge all dataset into single dataset
  merged.total.dataset <- cbind(x.merged, y.merged, subject.merged)

  #From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
  tidy.data <- ddply(merged.total.dataset, .(subject, activity), function(x) colMeans(x[, 1:66]))
  
  write.csv(tidy.data, file="UCI_HAR_Clean_data.csv", row.names=FALSE)
  
}
