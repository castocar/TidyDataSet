# Get and extract data

if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/dataset.zip")
data <- unzip("./data/dataset.zip", exdir = ".")

# Merge the training and the test sets to create one data set.

XTrain <- XTest <- NULL
filePath <- function(...) { paste(..., sep = "/") }
dataDir <- "UCI HAR Dataset"
readData <- function(path) {read.table(filePath(dataDir, path))}
if(is.null(XTrain)) { XTrain <<- readData("train/X_train.txt") }
if(is.null(XTest))  { XTest  <<- readData("test/X_test.txt") }
merged <- rbind(XTrain, XTest)

# Extracts only the measurements on the mean and standard deviation for each measurement.

featureNames <- readData("features.txt")[, 2]
names(merged) <- featureNames
matches <- grep("(mean|std)\\(\\)", names(merged))
limited <- merged[, matches]

# Uses descriptive activity names to name the activities in the data set

yTrain <- readData("train/y_train.txt")
yTest  <- readData("test/y_test.txt")
yMerged <- rbind(yTrain, yTest)[, 1]
activityNames <- c("Walking", "Walking Upstairs", "Walking Downstairs", "Sitting", "Standing", "Laying")
activities <- activityNames[yMerged]

# Appropriately labels the data set with descriptive variable names

names(limited) <- gsub("^t", "Time", names(limited))
names(limited) <- gsub("^f", "Frequency", names(limited))
names(limited) <- gsub("-mean\\(\\)", "Mean", names(limited))
names(limited) <- gsub("-std\\(\\)", "StdDev", names(limited))
names(limited) <- gsub("-", "", names(limited))
names(limited) <- gsub("BodyBody", "Body", names(limited))

subjectTrain <- readData("train/subject_train.txt")
subjectTest  <- readData("test/subject_test.txt")
subjects <- rbind(subjectTrain, subjectTest)[, 1]
tidy <- cbind(Subject = subjects, Activity = activities, limited)

# Creates independent tidy data set with the average of each variable for each activity and each subject

library(plyr)
limitedColMeans <- function(dato) { colMeans(dato[,-c(1,2)]) }
tidyMeans <- ddply(tidy, .(Subject, Activity), limitedColMeans)
names(tidyMeans)[-c(1,2)] <- paste0("Mean", names(tidyMeans)[-c(1,2)])
write.table(tidyMeans, "tidyMeans.txt", row.names = FALSE)
tidyMeans
