
rm(list=ls())

###########################################################
#1. CREATING A COMBINED DATA SET WITH TRAINING AND TEST####
###########################################################

#Setting the working directory
setwd("F:/RTraining/GCD")

features<-read.table("F:/Edu_Universidad/4_Online/1_Coursera/Econometrics/JHopkins_Datascience/3_Getting_Cleaning_Data/GCD_Week4/GCD_Quiz4/UCI/features.txt", header = FALSE)

activityType<-read.table("F:/Edu_Universidad/4_Online/1_Coursera/Econometrics/JHopkins_Datascience/3_Getting_Cleaning_Data/GCD_Week4/GCD_Quiz4/UCI/activity_labels.txt",header=FALSE)
colnames(activityType)<-c("activityId","activityType") #Assigning variale names


#IMPORTING TRAINING DATA##########################
subjectTrain<-read.table("F:/Edu_Universidad/4_Online/1_Coursera/Econometrics/JHopkins_Datascience/3_Getting_Cleaning_Data/GCD_Week4/GCD_Quiz4/UCI/train/subject_train.txt",header=FALSE)
colnames(subjectTrain)<-"subjectId"

xTrain<-read.table("F:/Edu_Universidad/4_Online/1_Coursera/Econometrics/JHopkins_Datascience/3_Getting_Cleaning_Data/GCD_Week4/GCD_Quiz4/UCI/train/X_train.txt",header=FALSE)
colnames(xTrain)<-features[,2]

yTrain<-read.table("F:/Edu_Universidad/4_Online/1_Coursera/Econometrics/JHopkins_Datascience/3_Getting_Cleaning_Data/GCD_Week4/GCD_Quiz4/UCI/train/Y_train.txt",header=FALSE)
colnames(yTrain)<-"activityId"

# Combined training set
trainingData = cbind(yTrain,subjectTrain,xTrain)


##IMPORTING TEST DATA##########################################
subjectTest<-read.table("F:/Edu_Universidad/4_Online/1_Coursera/Econometrics/JHopkins_Datascience/3_Getting_Cleaning_Data/GCD_Week4/GCD_Quiz4/UCI/test/subject_test.txt",header=FALSE)
colnames(subjectTest)<-"subjectId"

xTest<-read.table("F:/Edu_Universidad/4_Online/1_Coursera/Econometrics/JHopkins_Datascience/3_Getting_Cleaning_Data/GCD_Week4/GCD_Quiz4/UCI/test/X_test.txt",header=FALSE)
colnames(xTest)<-features[,2]

yTest<-read.table("F:/Edu_Universidad/4_Online/1_Coursera/Econometrics/JHopkins_Datascience/3_Getting_Cleaning_Data/GCD_Week4/GCD_Quiz4/UCI/test/Y_test.txt",header=FALSE)
colnames(yTest)<-"activityId"

# Combined test set 
testData = cbind(yTest,subjectTest,xTest)


##COMBINED DATA##############################################

combinedData<-rbind(trainingData,testData)

# Create a vector for the column names from the finalData, which will be used
colNames  = colnames(combinedData)

#Checking data
names(testData)
names(trainingData)
names(combinedData)


###########################################################
#2. EXTRACTING THE MEAN AND STANDARD DESVIATION############
###########################################################

# Create a logicalVector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
Vector<-(grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames))
Vector

# Subset finalData table based on the logicalVector to keep only desired columns
combinedData2 = combinedData[Vector==TRUE]
head(combinedData2)
summary(combinedData2)

###########################################################
#3. USE ACTIVITY NAMES FOR FINAL DATA#######################
###########################################################

# Merge the finalData set with the acitivityType table to include descriptive activity names
combinedData2 = merge(combinedData2,activityType,by='activityId',all.x=TRUE);
head(combinedData2,100)
names(combinedData2)

# Updating the colNames vector to include the new column names after merge
colNames2  = colnames(combinedData2) 



###########################################################
#4. DESCRIPTIVE VARIABLE NAMES#############################
###########################################################

# Cleaning up the variable names
colNames<-colnames(combinedData)
colNames

for (i in 1:length(colNames)) 
{
        colNames[i] = gsub("-std","StdDev",colNames[i])
        colNames[i] = gsub("-mean","Mean",colNames[i])
        colNames[i] = gsub("^(t)","Time",colNames[i])
        colNames[i] = gsub("^(f)","Freq",colNames[i])
        colNames[i] = gsub("Acc","Accelerometer",colNames[i])
        colNames[i] = gsub("Gyro","Gyroscope",colNames[i])
        colNames[i] = gsub("Mag","Magnitude",colNames[i])
}

# Reassigning column names to the combinedData set
combinedData_names<-combinedData  #Creating a new database
colnames(combinedData_names) = colNames #Assigning new colnames

#Comparing the new variables names
names(combinedData)
names(combinedData_names)



###########################################################
#5. SECOND TIDY WITH AVERAGE OF EACH VARIABLE##############
###########################################################

# Create a new table, finalDataNoActivityType without the activityType column
finalDataNoActivityType<-combinedData2[,names(combinedData2) != 'activityType'];

# Summarizing the finalDataNoActivityType table to include just the mean of each variable for each activity and each subject
tidyData<-aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean);
resume<-sapply(tidyData, mean) #Obtaining the mean of each variable


# Merging the tidyData with activityType to include descriptive acitvity names
tidyData<-merge(tidyData,activityType,by='activityId',all.x=TRUE)


# Export the tidyData set 
write.table(tidyData, "F:/Edu_Universidad/4_Online/1_Coursera/Econometrics/JHopkins_Datascience/3_Getting_Cleaning_Data/GCD_Week4/GCD_Quiz4/UCI/tidyData.txt",row.names=TRUE,sep="\t")



