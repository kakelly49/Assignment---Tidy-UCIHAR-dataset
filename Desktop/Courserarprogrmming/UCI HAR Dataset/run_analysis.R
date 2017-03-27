## Run_analysis.R - creating a tidy version of a subset of the UCIHAR datasets
      library(dplyr)
## read the following files as tables from the test folder:
##    - X_test, Y_test, subject_test
## and use cbind to append them together

      rawtest <-read.table("test/X_test.txt")
      ytest <-read.table("test/Y_test.txt")
      subjecttest <-read.table("test/subject_test.txt",skip=1)
      testing<-cbind(subjecttest,ytest,rawtest)

## read the following files as tables from the train folder:
##    - X_train, Y_train, subject_train
## and use cbind to append them together

      rawtrain <-read.table("train/X_train.txt")
      ytrain <-read.table("train/Y_train.txt")
      subjecttrain <-read.table("train/subject_train.txt")
      training<-cbind(subjecttrain,ytrain,rawtrain)

## read activity labels and select the second column(V2)which 
## has character values
      temp <-read.table("activity_labels.txt")
      activity_labels = tolower(as.character(temp$V2))


## Use rbind to merge test and train files
      mergedata <-rbind(testing, training)


##  Create column names using features.txt file: 
##  Read file, select a subset and transpose the 
##  subset so that row values are now column values

      measurements<-read.table("features.txt")
      measures <-select(measurements,V2)
      makecolnames<-t(measures)


##clean makecolnames data
      makecolnames<-gsub("\\(","",makecolnames) %>%
         {gsub("()","",.)} %>%
         {gsub("-","",.)}  %>%
         {gsub("\\)","",.)}
      makecolnames<-gsub("BodyBody","Body",makecolnames)


##create name for the new column with user ID's
      newcolumns<-c("subject","activity")
      makecolnames<-append(newcolumns,makecolnames)

##Assign column names to the mergedata 
      colnames(mergedata) <-(makecolnames)

##convert activity numbers to activity names
      mergedata$activity = as.factor(mergedata$activity)
      levels(mergedata$activity) = activity_labels

## Create a vector of desired column names containing - "mean",
## "std", "subject", "activity" anywhere in the column name
## unselect columns with "angle" in the column name 
      makecolnames<-grep("angle",makecolnames,invert=TRUE,value=TRUE)
      column_names<-grep("mean|subject|std|activity",makecolnames,value=TRUE)

##select the columns I want - contain "mean","subject" or "std" or "activity" in column name
      l<-length(column_names)
      tidyUCIHAR<-subset(mergedata, select=column_names[1:l])
      write.table(tidyUCIHAR, file="tidyUCIHAR.txt",row.name=FALSE)




