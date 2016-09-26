
#load training and testing set
train <- read.table("/Users/xunwang/Dropbox/courses/data/cleaningData/UCI HAR Dataset/train/X_train.txt")
test <- read.table("/Users/xunwang/Dropbox/courses/data/cleaningData/UCI HAR Dataset/test/X_test.txt")
# Load column names
labels <- read.table("/Users/xunwang/Dropbox/courses/data/cleaningData/UCI HAR Dataset/features.txt", stringsAsFactors = FALSE)
#load subject ID and activity column
SubTrain <- read.table("/Users/xunwang/Dropbox/courses/data/cleaningData/UCI HAR Dataset/train/subject_train.txt", stringsAsFactors = FALSE)
ActTrain <- read.table("/Users/xunwang/Dropbox/courses/data/cleaningData/UCI HAR Dataset/train/y_train.txt", stringsAsFactors = FALSE)
SubTest <- read.table("/Users/xunwang/Dropbox/courses/data/cleaningData/UCI HAR Dataset/test/subject_test.txt", stringsAsFactors = FALSE)
ActTest <- read.table("/Users/xunwang/Dropbox/courses/data/cleaningData/UCI HAR Dataset/test/y_test.txt", stringsAsFactors = FALSE)

ActTrainCol <- ActTrain$V1
SubjectIdTrain <- SubTrain$V1
train[,1:563] <- cbind(SubjectIdTrain, ActTrain, train[,1:561]) 
colnames(train) <- c("SubjectId", "Activity", labels$V2)
head(train)

ActTestCol <- ActTest$V1
SubjectIdTest <- SubTest$V1
test[,1:563] <- cbind(SubjectIdTest, ActTest, test[,1:561]) 
colnames(test) <- c("SubjectId", "Activity", labels$V2)
head(test)


mergeData <- rbind(train, test)
# search for column names containing "mean"
mean <- grepl("mean\\(\\)",labels$V2)
meanCol <- which(mean==TRUE)
meanCol2 <- meanCol+2
# select these columns from dataset
mergeDataSub1 <- mergeData[,meanCol2]
# search for column names containing "std"
std <- grepl("std",labels$V2)
stdCol <- which(std==TRUE)
stdCol2 <- stdCol+2
# select these columns from dataset
mergeDataSub2 <- mergeData[,stdCol2]
# combine two sets of columns
mergeDataSub <- cbind(mergeData[,1:2], mergeDataSub1, mergeDataSub2)
mergeDataSub$Activity <- factor(mergeDataSub$Activity, labels = c("WALKING","WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING"))
# calculate mean values per subject per activity
tidy_data<-aggregate( .~SubjectId+Activity,data=mergeDataSub, FUN = "mean")
# Write dataset as .txt
write.table(tidy_data, "tidy_data.txt", row.name=FALSE, quote=FALSE)
