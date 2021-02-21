

#Obtain dataset
##First we create the folder structure
if(!file.exists("./data")) {
        dir.create(file.path("./", "data"), showWarnings = FALSE)
        }
if(!file.exists("./data/rawdata")) {
        dir.create(file.path("./data", "rawdata"),showWarnings = FALSE)
        }

##Now we download and unzip the file
datasetUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipPath<-"./data/zippedRawDataset.zip"
download.file(datasetUrl,zipPath)
unzip(zipPath,exdir="./data/rawdata")


#Merges the training and the test sets to create one data set.
##The following method will obtain a list of all the files, the types should be
## train or test
obtainFilesToLoad <-function(type){
        filesToLoadTest<-c(
                       paste("X_",type,".txt",sep=""),
                       paste("y_",type,".txt",sep=""),
                       paste("subject_",type,".txt",sep="")
        )

}

##This function will do a bind of the new columns when we read the files specified
mergeFiles <- function(folder,files){
        mergedFiles=NA
        for(txtFile in files){
                path<-paste(folder,txtFile,sep="/")
                data<-as.numeric(scan(path, character(), quote = ))
                mergedFiles<-
                        if(anyNA(mergedFiles)){ data }
                        else{ cbind(mergedFiles,data)}
        }
        return (mergedFiles)
}

trainData<-mergeFiles("./data/rawdata/UCI HAR Dataset/train",
           obtainFilesToLoad("train"))

testData<-mergeFiles("./data/rawdata/UCI HAR Dataset/test",
                      obtainFilesToLoad("test"))
dataset<-rbind(trainData,testData)



#Extracts only the measurements on the mean and standard deviation for each measurement. 
obtainFilesToLoadMeasuraments <-function(type){
        filesToLoadTest<-c(
                paste("Inertial Signals/body_acc_x_",type,".txt",sep=""),
                paste("Inertial Signals/body_acc_y_",type,".txt",sep=""),
                paste("Inertial Signals/body_acc_z_",type,".txt",sep=""),
                paste("Inertial Signals/body_gyro_x_",type,".txt",sep=""),
                paste("Inertial Signals/body_gyro_y_",type,".txt",sep=""),
                paste("Inertial Signals/body_gyro_z_",type,".txt",sep=""),
                paste("Inertial Signals/body_acc_x_",type,".txt",sep=""),
                paste("Inertial Signals/body_acc_y_",type,".txt",sep=""),
                paste("Inertial Signals/body_acc_z_",type,".txt",sep="")
        )
}

mergeFilesFilteringFields <- function(folder,files,fieldspositions,fieldsnames){
        mergedFiles=NA
        for(txtFile in files){
                path<-paste(folder,txtFile,sep="/")
                data<-as.numeric(read.table(path, sep=" "))
                data<-data[,fieldpositions]
                names(data)<-fieldsnames
                mergedFiles<-
                        if(anyNA(mergedFiles)){ data }
                else{ cbind(mergedFiles,data)}
        }
        return (mergedFiles)
}

#get column numbers and names where description has mean or std in it
features = read.table("./data/rawdata/UCI HAR Dataset/features.txt")
namesPositions =grep("mean|std",features[,2])
namesDescriptions = grep("mean|std",features[,2],value = TRUE)

trainData<-mergeFilesFilteringFields("./data/rawdata/UCI HAR Dataset/train",
                                     obtainFilesToLoadMeasuraments("train"),
                                     namesPositions,
                                     namesDescriptions)

testData<-mergeFilesFilteringFields("./data/rawdata/UCI HAR Dataset/test",
                                     obtainFilesToLoadMeasuraments("test"),
                                     namesPositions,
                                     namesDescriptions)

#Uses descriptive activity names to name the activities in the data set
##with these values we should replace the ones in files y_{type}.txt
path<-"./data/rawdata/UCI HAR Dataset/activity_labels.txt"
labels<-read.table(file(path),sep=' ')
table(dataset[,2])
for(i in 1:6 ){
        dataset[,2]<-gsub(labels[i,1],labels[i,2],dataset[,2])
}
table(dataset[,2])

#Appropriately labels the data set with descriptive variable names. 


names(dataset)<-c(,"activity","subject",namesDescriptions)

#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.        
averages = dataset %>% group_by(activity,subject) %>% summarize_all(funs(mean))




