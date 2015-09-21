run_analysis <- function(){

    
    # Merges the training and the test sets to create one data set
    subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
    activity_train <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
    features_train <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)    
    
    subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
    activity_test <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
    features_test <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)    
    
    subject_all <- rbind(subject_train, subject_test)
    activity_all <- rbind(activity_train,activity_test)
    feature_all <- rbind(features_train, features_test)

    #Appropriately labels the data set with descriptive variable names
    
    features <- read.table("UCI HAR Dataset/features.txt", header = FALSE)

    features$V2 = gsub('-mean', 'Mean', features$V2)
    features$V2 = gsub('-std', 'Std', features$V2)
    features$V2 = gsub('[-()]', '', features$V2)    
    features$V2 <- gsub("^t", "Time", features$V2)
    features$V2 <- gsub("^f", "Frequency",features$V2)    

    colnames(activity_all) <- "Activity"
    colnames(subject_all) <- "Subject"        
    colnames(feature_all) <- c(features$V2)
    
    features_no_of_col <- ncol(feature_all)
    data_all <- cbind(feature_all,activity_all,subject_all)    
    
    #Extracts only the measurements on the mean and standard deviation for each measurement
    mean_std_columns  <- grep(".*Mean.*|.*Std.*", features$V2)

    #Mean, Std columns plus Subject plus Activity    
    data_required <-  data_all[, c(mean_std_columns, features_no_of_col + 1,features_no_of_col + 2)]
    
    
    #Uses descriptive activity names to name the activities in the data set
    activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)
    data_required$Activity <- factor(data_required$Activity,levels=activity_labels$V1,labels=activity_labels$V2)
    
    data_required$Subject <- as.factor(data_required$Subject)
    

    #Part 5 - From the data set in step 4, creates a second, independent tidy data set with the
    #average of each variable for each activity and each subject

    tidy <- aggregate(. ~Subject + Activity, data_required, mean)
    write.table(tidy, "tidy.txt", sep="\t")   
    tidy
    
}
