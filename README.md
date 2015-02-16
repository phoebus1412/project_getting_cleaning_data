R script for Getting and Cleaning Data Course Project
========================================

## Read training set and test set

    setwd("Z:\\R\\CourseraExercises\\getdata-projectfiles-UCI HAR Dataset\\UCI HAR Dataset\\train")
    
    trainSet<-read.table("X_train.txt")
    
    trainLabel<-read.table("y_train.txt")
    
    trainSubject<-read.table("subject_train.txt")
    
    setwd("Z:\\R\\CourseraExercises\\getdata-projectfiles-UCI HAR Dataset\\UCI HAR Dataset\\test")
    
    testSet<-read.table("X_test.txt")
    
    testLabel<-read.table("y_test.txt")
    
    testSubject<-read.table("subject_test.txt")
    
    setwd("Z:\\R\\CourseraExercises\\getdata-projectfiles-UCI HAR Dataset\\UCI HAR Dataset")
    
    activityLabel<-read.table("activity_labels.txt")
    
    features<-read.table("features.txt")

##In order to make merged data set available for future uses, Here,before I merge data, I did point 3 first, that is, Adding a new column called Activities into training and test sets, which labels each observation with its moving pattern, besides, in order to finish point 5, I also add a column called Subject which denotes who did this observation.Then, use merge() to combine two data sets.

    trainWordLabel<-c()
    for(i in 1:nrow(trainLabel)){
        trainWordLabel<-c(trainWordLabel,as.character(activityLabel[trainLabel[i,1],2]))
    }
    trainSet<-data.frame(trainSet,Activities=trainWordLabel,Subject=as.vector(trainSubject))
    testWordLabel<-c()
    for(i in 1:nrow(testLabel)){
        testWordLabel<-c(testWordLabel,as.character(activityLabel[testLabel[i,1],2]))
    }
    testSet<-data.frame(testSet,Activities=testWordLabel,Subject=as.vector(testSubject))
    mergeSet<-merge(trainSet,testSet,all=TRUE)

## Extracts measurements on the mean and standard deviation for each measurement.In order to select measurements related to mean and std,I use grepl() which is a pattern matching function, it will return a logical value to decide whether the parameter pattern is in the parameter x or not.By using this function,I can select all the variables related to mean and standrad deviation.Then extract them.

    meanIndex<-c()
    for(j in 1:nrow(features)){
        if(grepl(pattern="mean",x=as.character(features[j,2]))){
            meanIndex<-c(meanIndex,as.character(features[j,2]))
        }
    }
    stdIndex<-c()
    for(k in 1:nrow(features)){
        if(grepl(pattern="std",x=as.character(features[k,2]))){
            stdIndex<-c(stdIndex,as.character(features[k,2]))
        }
    }
    measureMeanStd<-mergeSet[,c(meanIndex,stdIndex)]

## labels the data set with descriptive variable names,here, I use colnames() to denote variable names.

    colnames(mergeSet)<-c(as.character(features[,2]),"Activities","Subject")

## creates a second, independent tidy data set with the average of each variable for each activity and each subject.

    secondSet<-data.frame(matrix(0,ncol=561,nrow=36))
    for(m in 1:nrow(activityLabel)){
        for(n in 1:nrow(features)){
            secondSet[m,n]<-mean(mergeSet[as.character(mergeSet$Activities)==as.character(activityLabel[m,2]),as.character(features[n,2])])
        }
    }
    for(p in 1:30){
        for(q in 1:nrow(features)){
            secondSet[p+6,q]<-mean(mergeSet[as.character(mergeSet$Subject)==p,as.character(features[q,2])])
        }
    }

## Finally, write the second independent tidy data set into working directory

    rownames(secondSet)<-c(as.character(activityLabel[,2]),1:30)
    colnames(secondSet)<-as.character(features[,2])
    write.table(secondSet,file="Second Data Set.txt",row.names=FALSE)


