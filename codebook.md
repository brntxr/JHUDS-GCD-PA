# Outline
1. Load required libraries;
2. Download data if it hasn't been downloaded yet;
3. Variable description and data cleaning steps
4. Read activities labels and id's;
5. Read and parse column names (i.e. features);
6. Read Train Data
7. Read Test Data
8. Merge, filter and group 
9. Export data

# 1. Load required libraries;
I won't go into any details about which libraries to load
 I don't believe I've used anything beyond R-core and dplyr
which I specifically load. All of its dependencies should be met.
 I'm assuming that you already have dplyr installed and running.

```R
library(dplyr)
```

These dirs are necessary to run all the analysis.
Root dir here is where I pulled a copy from a repo
specifically createad to this assginment

Then I defined other variables that I will need.
Root dir will the working directory, where everything will happen.
```R
rootDir <- file.path(getwd())
```
# 2. Download data if it hasn't been downloaded yet;
I'm adding a condition to decide wheter or not to download the dataset.
```R
getData = FALSE  #Set it to True if you want to download the data again

if (getData){

    #Dataset file
    #This is the file that will be downloaded containing the data.
    destFileName <- file.path(rootDir, "UCI_HAR_Dataset.zip")

    #Define a directory where files will be extracted
    destDir <- file.path(rootDir, "UCI HAR Dataset")

    #Download dataset
    if (!file.exists(destFileName)) {
        dataURL <-
            "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        download.file(dataURL, destfile = destFileName , method = "curl")
    }

    #Check if the file has already been unzipped
    if (!dir.exists(destDir)) {
        unzip(destFileName, exdir = rootDir)
        list.files()
    }
    rm("destFileName", "dataURL")
}else {
    #If you already have the files, this is the default value for destDir, 
    #destDir is the directory where the datafiles were extracted
    #Use the default directory that contains the data 
    destDir <- file.path(rootDir, "UCI HAR Dataset")
}
```

# 3. Variable description and data cleaning steps
After getting and unzipping the data, here's what you will end up with:  

* test dir - contains three files and one directory:  

    * subject_test.txt contains an id for each row of the data set indicating to which individual that observation corresponds.  
  
    * X_test.txt main test data set with all the observations  

    * y_test.txt: simple text with an activity ID for  

    * Inertial Signs dir: i won't be using these files  

* train dir: same as above  

* activity_labels.txt: identifies each activity id with its correspondent description  

* features.txt: identifies the columns in the data sets  

* features_info.txt: gives some useful information about the meaning of the variables  

* README.txt: describes the dataset.  

Basically, here's what needs to be done (according to the assignment):

1. Merges the training and the test sets to create one data set.
In order to merge the datasets, it's necessary to join all files that contain  information about each data set.
For both datasets,  it is necessary to add column names (i.e. features) from the features file, and also add subject and activity ids to each row.

2. Extracts only the measurements on the mean and standard deviation for each measurement.
After merging both datasets, it's necessary to identify the columns that contain means or standard deviation.
It's possible to identify means and std. devs. using descriptions found in features_info.txt. 

3. Uses descriptive activity names to name the activities in the data set
The file "activity_labels.txt" contains descriptions to activities ID's, I'll just merge datasets in order to complete step 3.

4. Appropriately labels the data set with descriptive variable names.
I'll use features_info.txt to build a dictionary with the meaning of every part of the feature name and then using simple substition generate a new vector with descriptive names. Just in case it could be necessary every column name was parsed.

5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject. After filtering the columns, I just have to duplicate and group by subject and activity. Then export it as required.

# 4. Activity labels and ID's dataset
This code reads the file that contains the activities ids and its descriptios. It will be used to give meaningful descriptions to each activity.
```R
actFile <- file.path(destDir, "activity_labels.txt")
actDF <- read.table(actFile, header = FALSE)
rm("actFile")
names(actDF) <- c("ActivityID", "ActivityDS")
```
# 5. Read and parse column names (i.e. features);
## Features - Column names
This code will read the features file put it into a data frame and assign names to the columns.
```R
featuresFile <- file.path(destDir, "features.txt")
featuresDF <- read.table(featuresFile, header = FALSE)
names(featuresDF) <- c("featureID", "featureVar")
rm("featuresFile")
```
## Parsing column names 
As required in the assignment, columns will have their names parsed to more meaningul labels. 
In order to do so, I will create a 'dictionary' with regex expressions and corresponding readable labels as found in features-details.
At the bottom there are a few replacement codes included only in order to treat the strings (e.g.: Remove leading and multiple whitespaces, substitute commas for "and", etc...)
```R
stRep <- matrix(
    c(
        '^t',        
        ' time ',
        '^f',        
        ' frequency ',
        'Body',        
        ' body ',
        'Gravity',        
        ' gravity ',
        'Acc',        
        ' acceleration ',
        'Gyro',        
        ' angular velocity ',
        '-X',' x-dimension ',
        '-Y',' y-dimension ',
        '-Z',
        ' z-dimension ',
        'Mag',
        ' magnitude ',
        'Jerk',
        ' Jerk signal',
        'mean\\(\\)',
        ' mean ',
        'std\\(\\)',
        ' std. dev. ',
        'mad\\(\\)',
        ' max. abs. dev. ',
        'max\\(\\)',
        ' max ',
        'min\\(\\)',
        ' min ',
        'sma\\(\\)',
        ' signal magnitude area ',
        'energy\\(\\)',
        ' energy measure ',
        'iqr\\(\\)',
        ' interquartile range ',
        'entropy\\(\\)',
        ' signal entropy ',
        'arCoeff\\(\\)',
        ' Autorregresion coefficients ',
        'correlation\\(\\)',
        ' correlation between two signals ',
        'maxInds\\(\\)',
        ' index of the frequency component with largest magnitude ',
        'meanFreq\\(\\)',
        ' weighted average of the frequency components ',
        'skewness\\(\\)',
        ' skewness ',
        'kurtosis\\(\\)',
        ' kurtosis ',
        'bandsEnergy\\(\\)',
        ' Energy of a frequency interval ',
        'angle\\(',
        ' angle between ',
        "\\)",
        "",
        "\\b t \\b",
        " time ",
        "\\b f\\b",
        " frequency",
        '\\bX\\b',
        ' x-dimension ',
        '\\bY\\b',
        ' y-dimension ',
        '\\bZ\\b',
        ' z-dimension ',
        ",",
        " and ",
        "\\s{2,}",
        " ",
        "^\\s*",
        ""
    ),
    ncol = 2,
    byrow = TRUE
)
```
After defining how each expression needs to be replaced, I'll set a temp variable to store a copy the vector containing column names. This vector comes from the features file.
After defining a temporary variable there is a for loop to do each replacement in
```R
xx <- featuresDF$featureVar

loop through all replacements that need to be done
For every pair pattern-replace, R will search the temporary variable for the pattern, replace it with the given label and reassign to another temp variable.
```R
for (i in 1:nrow(stRep)) {
    myPattern <- stRep[i, 1]
    replaceText <- stRep[i, 2]
    xx <- gsub(pattern = myPattern, replacement = replaceText, xx)
}
```
After substituing everything, human readable column names will be added to the features dataset.

```R
featuresDF$label <- xx
```
Removing temporary variables.
```R
rm("myPattern", "replaceText", "xx")
```
Beyond parsing column names, three more columns to features dataset:
- isMean (boolean): indicates wheter or not an specific column is a mean
- isStDeb (boolean): indicates whether or not a column is an standar deviation
- keepCol (boolean): the column is a mean or a std deviation. This is the column that will be used to filter the dataset. 

```R
featuresDF$isMean <-
    grepl(pattern = "mean",
          ignore.case = TRUE,
          x = featuresDF$featureVar)
featuresDF$isStDev <-
    grepl(pattern = "std",
          ignore.case = TRUE,
          x = featuresDF$featureVar)
featuresDF$keepCol <- featuresDF$isMean | featuresDF$isStDev
```

## Filter vector
There are two more columns that need to stay in the final dataset: subject and activiy ID's. This is what is done in the code below.

```R
colsKeep <- featuresDF[c(featuresDF$keepCol), "label"]
colsKeep <- featuresDF[(featuresDF$keepCol), "label"]
colsKeep <- c("SubjectID", "activityDS", colsKeep)
```

# 6. Read Train Data
### This part of the code is loads and prepares train data. 

Define a variable indicating where the train dataset is located.
```R
trainDir <- file.path(destDir, "train")
```
###  Activity ID
Load activities id's for train data.
```R
labelsFile <- file.path(trainDir, "y_train.txt")
trainingDF <- read.table(labelsFile, header = FALSE)
names(trainingDF) <- c("ActivityID")
activityDF <- merge(actDF, trainingDF)
```
### Subject ID
Load subjects id's for train data.
```R
subjectFile <- file.path(trainDir, "subject_train.txt")
subjectDF <- read.table(subjectFile, header = FALSE)
names(subjectDF) <- "SubjectID"
```
### Train data
Load train data and add column names
```R
trainFile <- file.path(trainDir, "X_train.txt")
trainDF <- read.table(trainFile, header = FALSE)
names(trainDF) <- featuresDF$featureVar
```

###  Merge train data, subject ID and activity ID
```R
trainDF2 <- cbind(subjectDF, activityDF, trainDF)
```
Train dataset is loaded and ready to be merged. 

### Cleaning up
```R
rm("subjectFile")
rm("labelsFile")
rm("trainFile")
rm("trainDir")
rm("subjectDF", "activityDF")
rm("trainDF")
```
#7. Read Test Data

This part of the code is loads and prepares test data  
Define a variable indicating where the test dataset is located.
```R
testDir <- file.path(destDir, "test")
```
### Activity ID
Load activities id's for test data.
```R
labelsFile <- file.path(testDir, "y_test.txt")
testingDF <- read.table(labelsFile, header = FALSE)
names(testingDF) <- c("ActivityID")
activityDF2 <- merge(actDF, testingDF)
```

### Test data
Load test data and add column names
```R
testFile <- file.path(testDir, "X_test.txt")
testDF <- read.table(testFile, header = FALSE)
names(testDF) <- featuresDF$featureVar
```

### Subject ID
Load subjects id's for test data.
```R
subjectFile <- file.path(testDir, "subject_test.txt")
subjectDF2 <- read.table(subjectFile, header = FALSE)
names(subjectDF2) <- "SubjectID"
```

Train dataset is loaded and ready to be merged. 
```R
testDF2 <- cbind(subjectDF2, activityDF2, testDF)
```
Cleaning up
```R
rm("trainFile")
rm("subjectFile")
rm("labelsFile")
rm("trainFile")
rm("trainDir")
rm("subjectDF", "activityDF")
rm("trainDF")
```


# 8. Merge, filter and group  

### Merge train and test datasets
```R
df <- rbind(trainDF2, testDF2)
```
### Fix column names
```R
names(df) <-
    c("SubjectID", "activityID", "activityDS", featuresDF$label)
```

### Filter dataset 
Leave only ids means and standard deviations.
```R
df2 <- df[, colsKeep]
```

 Convert to table data
```R
df3 <- tbl_df(df2)
df4 <- group_by(df3, SubjectID, activityDS)
```

### Group by subject and activity

```R
dfFinal <- df3 %>%
    group_by(SubjectID, activityDS) %>%
    summarise_each(funs(mean))
```
# 9. Export data
```R
exportFile<- file.path(rootDir,"dfFinal.txt")
write.table(dfFinal, exportFile, row.name=FALSE)
rm('exportFile')
```

### Remove temporary variables
```R
rm(
    "actDF",
    "actFile",
    "activityDF",
    "activityDF2",
    "actLabels",
    "dataURL"  ,
    "destFileName"   ,
    "features"   ,
    "featuresFile",
    "subjectDF" ,
    "subjectDF2"  ,
    "testDF"    ,
    "testDF2"  ,
    "testDir"     ,
    "testFile"  ,
    "testingDF",
    "trainDF" ,
    "trainDF2"   ,
    "trainDir"  ,
    "trainingDF",
    "df2",
    "df3",
    "df4"
)
```

