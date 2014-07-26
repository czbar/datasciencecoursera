#####################################################
## This script cleans the accelerometer data set
## combining the training and test sets, adding 
## column headers and inserting a column with the
## descriptive activity name.
## Produces two tidy data sets: one as described above
## and the other containing averages of each column
## grouped by subject and activity.
#####################################################

mergeTrainAndTest <- function()
{
  # read training and test data
  train<-read.table("./train/X_train.txt")
  test<-read.table("./test/X_test.txt")
  
  # combine training and test data table
  mx <- rbind(train,test)
  
  # read activity labels
  trainActivities<-read.table("./train/y_train.txt")
  testActivities<-read.table("./test/y_test.txt")
  
  # combine activity labels
  # so they match merged training and test data rows
  my <- rbind(trainActivities, testActivities)
  
  # insert activity labels as the first column
  myx <- cbind(my,mx)
  
  # read feature names
  featureNames<-read.table("./features.txt")
  
  # use feature names as column names
  v<-as.vector(featureNames[,2])
  v<-c("activity",v)
  
  colnames(myx)<-v
  
  # read activity names
  # First column contains the activity label and the
  # second the activity name
  activityLabels<-read.table("./activity_labels.txt")
  colnames(activityLabels)<-c("activity", "activity_name")

  # select only columns that have "mean" or "std"
  # in their names
  a<-grepl("mean",colnames(myx))
  b<-grepl("std",colnames(myx))
  #don't lose the activity column
  a[1]<-TRUE

  # combine the 2 logical vectors
  comb<-a | b
  
  # subset the mean and std columns 
  mean_std<-myx[,comb]
  
  # merge (join) so we get activity label names 
  tidy_data_1<-merge(activityLabels, mean_std,by.x="activity", by.y="activity", all=TRUE)  

  # write out the first tidy data set
  write.table(tidy_data_1, "./tidy_data_1.txt", row.names=FALSE)
  
  # now create the second data set.
  # to do that let's go back to the pre-filtered 
  # data table myx and insert a column with subject id
  
  # read training and test subjects
  trainSubjects<-read.table("./train/subject_train.txt")
  testSubjects<-read.table("./test/subject_test.txt")
  subjects <- rbind(trainSubjects, testSubjects)

  v<-colnames(myx)
  full_data<-cbind(subjects, myx)
  colnames(full_data)<-c("subject", v)

  # melt the data frame
  molten<-melt(full_data, id=c("subject","activity"))
  
  # decast with mean values of all columns, grouped by subject
  # and activity
  tidy_data_2<-dcast(molten, subject + activity ~ variable, mean)
  
  # write out the second tidy data set
  write.table(tidy_data_2, "./tidy_data_2.txt", row.names=FALSE)
}

