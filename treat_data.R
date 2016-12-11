

#Root working dir
rootDir <- file.path(getwd())

#Dataset file
destFileName <- file.path(rootDir, "UCI_HAR_Dataset.zip")

#Define a directory where files will be extracted
destDir <- file.path(rootDir, "UCI HAR Dataset")




#Download dataset
if (!file.exists(destFileName)) {
    dataURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(dataURL, destfile = destFileName , method = "curl")
}

#Check if the file has already been unzipped 
if (!dir.exists(destDir)) {
    unzip(destFileName, exdir = rootDir)
    list.files()
}
rm("destFileName", "dataURL")



# Activity labels dataset
actFile <- file.path(destDir, "activity_labels.txt")
actDF<- read.table(actFile, header = FALSE)
names(actDF)<- c("ActivityID","ActivityDescription")
actDF

# Features 
#Columns
featuresFile <- file.path(destDir, "features.txt")
featuresDF<- read.table(featuresFile, header = FALSE)
names(featuresDF)<- c("featureID","featureVar")
View(featuresDF)


stRep<-matrix(
    c(
        '^t',' time ',
        '^f',' frequency ',
        'Body',' body ',
        'Gravity',' gravity ',
        'Acc',' acceleration ',
        'Gyro',' angular velocity ',
        '-X',' x-dimension ',
        '-Y',' y-dimension ',
        '-Z',' z-dimension ',
        'Mag',' magnitude ',
        'Jerk',' Jerk signal',
        'mean\\(\\)',' mean ',
        'std\\(\\)',' std. dev. ',
        'mad\\(\\)',' max. abs. dev. ',
        'max\\(\\)',' max ',
        'min\\(\\)',' min ',
        'sma\\(\\)',' signal magnitude area ',
        'energy\\(\\)',' energy measure ',
        'iqr\\(\\)',' interquartile range ',
        'entropy\\(\\)',' signal entropy ',
        'arCoeff\\(\\)',' Autorregresion coefficients ',
        'correlation\\(\\)',' correlation between two signals ',
        'maxInds\\(\\)',' index of the frequency component with largest magnitude ',
        'meanFreq\\(\\)',' weighted average of the frequency components ',
        'skewness\\(\\)',' skewness ',
        'kurtosis\\(\\)',' kurtosis ',
        'bandsEnergy\\(\\)',' Energy of a frequency interval ',
        'angle\\(',' angle between ',
        "\\)","",
        "\\b t \\b"," time ",
        "\\b f\\b"," frequency",
        '\\bX\\b',' x-dimension ',
        '\\bY\\b',' y-dimension ',
        '\\bZ\\b',' z-dimension ',        
        ","," and ",
        "\\s{2,}"," ",
        "^\\s*",""
    ),ncol = 2, byrow=TRUE)


xx<-featuresDF$featureVar
nstRep<- nrow(stRep)
for(i in 1:nstRep){
    
    myPattern <- stRep[i,1]
    replaceText<-stRep[i,2]
    xx<-gsub(pattern = myPattern,replacement = replaceText, xx)
}

featuresDF$label <- xx
featuresDF$isMean<- grepl(pattern = "mean",ignore.case = TRUE,x=featuresDF$featureVar)
featuresDF$isStDev<- grepl(pattern = "std",ignore.case = TRUE,x=featuresDF$featureVar)
featuresDF$keepCol <- featuresDF$isMean | featuresDF$isStDev





##########################train dir
trainDir <- file.path(destDir, "train")

#Training labels.
labelsFile<- file.path(trainDir, "y_train.txt")
trainingDF<- read.table(labelsFile, header = FALSE)
names(trainingDF)<- c("ActivityID")
activityDF <-merge(actDF, trainingDF)
View(activityDF)
rm("labelsFile")

# Train
trainFile<-file.path(trainDir,"X_train.txt" )
trainDF<- read.table(trainFile,header = FALSE)
names(trainDF)<- featuresDF$featureVar
View(trainDF)
rm("trainFile")

#subject train
subjectFile <- file.path(trainDir, "subject_train.txt")
subjectDF<- read.table(subjectFile,header = FALSE)
names(subjectDF)<- "SubjectID"
View(subjectDF)
rm("subjectFile")

trainDF2 <- cbind(subjectDF, activityDF, trainDF)
View(trainDF2)


##########################test dir
testDir <- file.path(destDir, "test")

#Test labels.
labelsFile<- file.path(testDir, "y_test.txt")
testingDF<- read.table(labelsFile, header = FALSE)
names(testingDF)<- c("ActivityID")
activityDF2 <-merge(actDF, testingDF)
View(activityDF2)
rm("labelsFile")

# Train
testFile<-file.path(testDir,"X_test.txt" )
testDF<- read.table(testFile,header = FALSE)
names(testDF)<- featuresDF$featureVar
View(testDF)
rm("trainFile")

#subject train
subjectFile <- file.path(testDir, "subject_test.txt")
subjectDF2<- read.table(subjectFile,header = FALSE)
names(subjectDF2)<- "SubjectID"
View(subjectDF2)
rm("subjectFile")

testDF2 <- cbind(subjectDF2, activityDF2, testDF)
View(testDF2)


df <- rbind(trainDF2,testDF2)
View(df)
names(df) <- c("SubjectID","activityID", "activityDS",featuresDF$label)


rm(
    "actDF",        "actFile",      "activityDF",   "activityDF2",  "actLabels",   
    "dataURL"  ,    "destFileName"   ,         "features"   , "featuresFile",     "subjectDF" ,   "subjectDF2"  ,
    "testDF"    ,   "testDF2"  ,    "testDir"     , "testFile"  ,   "testingDF",   
    "trainDF" ,     "trainDF2"   ,  "trainDir"  ,   "trainingDF"  
)

xxxx<-featuresDF[c(featuresDF$keepCol),"label"]
xxxx<-featuresDF[(featuresDF$keepCol),"label"]
xxxx<-c("SubjectID","activityDS",xxxx)
xxxx
df2<-df[, xxxx]
df3<- tbl_df(df2)
df3

df4<-group_by(df3,SubjectID,activityDS)


dfTidy <- df3 %>%
    group_by(SubjectID, activityDS) %>%
    summarise_each(funs(mean))

str(dfTidy)
