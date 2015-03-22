GetData <- function(url)
{

### save working directory and download source data files for project.
### uses 'downloader' package to unzip files. 
     
     
homeDir<<-getwd()
download(url,dest="datasets.zip", mode="wb")
unzip("datasets.zip", exdir=".")
download_date<<-date()

}


LoadDataToR <- function()
{

####################################################################################
### reads in all data files and combines them into one master
### data frame named df_test.
###
### this function creats two 'feature' datasets for the test and train pops
### and then calls other functions to read and append the
### specific text and train datasets to the parent feature sets.
####################################################################################     
     
### set file paths and file names based on Readme file.       

     featurepath<-paste(homeDir, "/UCI HAR Dataset", sep="")     
     testpath<-paste(homeDir, "/UCI HAR Dataset/test", sep="")
     trainpath<-paste(homeDir, "/UCI HAR Dataset/train", sep="")
     
     
     testfile<-"/x_test.txt"
     testlabels<-"/y_test.txt"
     testids<-"/subject_test.txt"
     
     trainfile<-"/x_train.txt"
     trainlabels<-"/y_train.txt"
     trainids<-"/subject_train.txt"
     
     
     featurefile<-"/features.txt"
     activitiesfile<-"/activity_labels.txt"


### read datatsets.
     test_data<-read.table(paste(testpath, testfile, sep=""), header=FALSE, sep="")
     test_labels<-read.table(paste(testpath, testlabels, sep=""), header=FALSE, sep="")
     test_ids<-read.table(paste(testpath, testids, sep=""), header=FALSE, sep="")
     test_flag<-ifelse(test_ids[,1]>0, 1, 0)

     train_data<-read.table(paste(trainpath, trainfile, sep=""), header=FALSE, sep="")
     train_labels<-read.table(paste(trainpath, trainlabels, sep=""), header=FALSE, sep="")
     train_ids<-read.table(paste(trainpath, trainids, sep=""), header=FALSE, sep="")
     train_flag<-ifelse(train_ids[,1]<0, 1, 0)

     features<-read.table(paste(featurepath, featurefile, sep=""), header=FALSE, sep="")
     
     tfeat<-data.frame(1:3, c("ID", "FLAG", "ACTIVITY"))
     colnames(tfeat)<-c("V1","V2")
     tfeatures<-rbind(tfeat, features)
     
     df_test<-cbind(test_ids, test_flag, test_labels, test_data)     
     df_train<-cbind(train_ids, train_flag, train_labels, train_data)     
     colnames(df_test)<-tfeatures[,2]
     colnames(df_train)<-tfeatures[,2]

     df_test<-cbind(df_test, CreateTest())
     df_train<-cbind(df_train, CreateTrain())

     df_total<<-rbind(df_test, df_train)

}


CreateTest <- function()
{
   
################################################################
### aggregates data from the files in the test directory. 
### returns that data to be aggregated with the corresponding 
### features data.
################################################################


     setwd(paste(homeDir, "/UCI HAR Dataset/test/Inertial Signals", sep=""))
     
     file_list <- list.files()
     
     for (file in file_list)
          
          {
          
          # if the merged dataset doesn't exist, create it
          if (!exists("dataset"))
               {
               dataset <- read.table(file, header=FALSE, sep="")
               datpos<-1:length(dataset)
               datnames<-paste(substr(file, 1, nchar(file)-9), "_", datpos, sep="")
               colnames(dataset)<-datnames
               rm(datpos)
               rm(datnames)
     
               
               } else if (exists("dataset")) {  # if the merged dataset does exist, append to it
               
               temp_dataset <-read.table(file, header=FALSE, sep="")
               datpos<-1:length(temp_dataset)
               datnames<-paste(substr(file, 1, nchar(file)-9), "_", datpos, sep="")
               colnames(temp_dataset)<-datnames
               
               dataset<-cbind(dataset, temp_dataset)
               rm(temp_dataset)
               rm(datpos)
               rm(datnames)
               }

          #print(file)
          
          }
     
     ##testdat<-dataset
     return(dataset)
     rm(dataset)
     
}


CreateTrain <- function()
{

################################################################
### aggregates data from the files in the train directory. 
### returns that data to be aggregated with the corresponding 
### features data.
################################################################
     
     setwd(paste(homeDir, "/UCI HAR Dataset/train/Inertial Signals", sep=""))
     
     file_list <- list.files()
     
     for (file in file_list)
          
     {
          
          # if the merged dataset doesn't exist, create it
          if (!exists("dataset"))
          {
               dataset <- read.table(file, header=FALSE, sep="")
               datpos<-1:length(dataset)
               datnames<-paste(substr(file, 1, nchar(file)-10), "_", datpos, sep="")
               colnames(dataset)<-datnames
               rm(datpos)
               rm(datnames)
               
               
          } else if (exists("dataset")) {  # if the merged dataset does exist, append to it
               
               temp_dataset <-read.table(file, header=FALSE, sep="")
               datpos<-1:length(temp_dataset)
               datnames<-paste(substr(file, 1, nchar(file)-10), "_", datpos, sep="")
               colnames(temp_dataset)<-datnames
               
               dataset<-cbind(dataset, temp_dataset)
               rm(temp_dataset)
               rm(datpos)
               rm(datnames)
          }
          
          #print(file)
          
     }
     
     ##traindat<-dataset
     return(dataset)
     rm(dataset)
     
}


GetToTidy <- function()
{

################################################################
### Condesnes parent data file to only the key demos, the 
### 'mean' and 'std dev' fields for all metrics. Also, converts 
### key demos to clean factor labels for categorical data.
###
### requires dplyr package.
################################################################
     

df_clean<<-df_total[,c(1:9, 44:49, 84:89, 124:129, 164:169, 204:209, 217:218, 230:231, 243:244, 256:257, 269:274, 297:299, 348:353, 376:378, 427:432, 455:457, 506:507, 519:520, 529, 532:533, 542, 545:546, 555, 558:564)]  

df_clean$FLAG<<-factor(df_clean$FLAG, levels=c(1, 0), labels=c('Test', 'Train'))
df_clean$ACTIVITY<<-factor(df_clean$ACTIVITY, levels=c(1,2,3,4,5,6), labels=c('Walking', 'Walking_Upstairs', 'Walking_Downstairs', 'Sitting', 'Standing', 'Laying'))


df_tidy<<-df_clean %>% group_by(ID, ACTIVITY) %>% summarise_each(funs(mean))


}

OutputDataFiles <- function()
{
### writes tables to home directory
     
setwd(homeDir)

write.table(df_tidy, file=paste(homeDir, "/tidy_dataset.txt", sep=""), row.names=FALSE)
write.table(df_clean, file=paste(homeDir, "/condensed_clean_dataset.txt", sep=""), row.names=FALSE)
}    
     