    #This script performs the following tasks, for completion of Coursera - Getting and Cleaning Data Course Project:
    
    # Merges the training and the test sets to create one data set.
    
    # Extracts only the measurements on the mean and standard deviation for each
    # measurement.
    
    # Uses descriptive activity names to name the activities in the data set 
    
    # Appropriately labels the data set with descriptive variable names.
    
    # From the data set in step 4, creates a second, independent tidy data set
    # with the average of each variable for each activity and each
    # subject.
    
    # The script should be run from WD in a folder location in the directory
    # above the extracted data files.
    
    #I use the dplyr package for some of the data manipulations and reshape is also used to "melt" the data
    library(dplyr)
    library(reshape2)
    
    #read in all of the underlying data files
    x_test <- read.table(".\\UCI HAR Dataset\\test\\X_test.txt",header = FALSE, sep ="")
    y_test <- read.table(".\\UCI HAR Dataset\\test\\y_test.txt",header = FALSE, sep ="")
    subj_test <- read.table(".\\UCI HAR Dataset\\test\\subject_test.txt",header = FALSE, sep ="")

    x_train <- read.table(".\\UCI HAR Dataset\\train\\X_train.txt",header = FALSE, sep ="")
    y_train <- read.table(".\\UCI HAR Dataset\\train\\y_train.txt",header = FALSE, sep ="")
    subj_train <- read.table(".\\UCI HAR Dataset\\train\\subject_train.txt",header = FALSE, sep ="")
    
    # list of the different activities performed and measured. Descriptor for y variables.
    activity <- read.table(".\\UCI HAR Dataset\\activity_labels.txt",header = FALSE, sep ="")
    # list of the different measured features for each measurement. Corresponds to column labels of x variables
    features <- read.table(".\\UCI HAR Dataset\\features.txt",header = FALSE, sep ="")  

    
    #combine the test and train data sets and apply the appropriate column names
    x_all <- rbind(x_test, x_train)
    names(x_all) <- features[,2]
    
    y_all <- rbind(y_test,y_train)
    names(y_all) <- "activity_code"
    
    subj_all <- rbind(subj_test,subj_train)
    names(subj_all) <- "subject"
    
    #add in the subject data and the activity data
    all_data <- cbind(subj_all,y_all,x_all)
    
    
    #Add in descriptive activity names by merging on the activity code with the activity dataset
    names(activity) <- c("activity_code", "activity")
    m_all<-merge(all_data,activity, by.x="activity_code", by.y="activity_code")
    
    # Find all the column variables that involve the calculation of a mean or standard deviation. 
    # From the features_info text file I have used whether the variable name has the following text as my criteria:
    # mean(): Mean value
    # std(): Standard deviation
    extract_index<-grep("mean\\(\\)|std\\(\\)",names(m_all))
    
    relevant_variables<- cbind(m_all["subject"],m_all["activity"],m_all[,extract_index])
    relevant_variables$subject<-as.factor(relevant_variables$subject)
    
    melted<- melt(relevant_variables, id.vars=c("subject", "activity"), variable.name = "typeofmeasure")
    
    out_data <- summarise(group_by(melted,subject,activity,typeofmeasure), average = mean(value))
    
    write.table(out_data, file = ".\\answer.txt", row.names=FALSE)

