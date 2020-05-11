# 0. Downloading and unzipping dataset
if(!file.exists("./data")){dir.create("./data")}
fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileurl,destfile = "./data/smartphone.zip")
unzip(zipfile = "./data/smartphone.zip", exdir = "./data")

# 1. Merges the training and the test sets to create one data set.

# 1.1. Reading files

# 1.1.1. Reading train tables
x_train <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./data/UCI HAR Dataset/train/Y_train.txt")
subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")

# 1.1.2. Reading test tables
x_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./data/UCI HAR Dataset/test/Y_test.txt")
subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")

# 1.1.3. Reading feature vector
features <- read.table("./data/UCI HAR Dataset/features.txt")

# 1.1.4. Reading activity labels
activity_labels <- read.table("./data/UCI HAR Dataset/activity_labels.txt")

# 1.2. Assigning column names
colnames(x_train) <- features[,2]
colnames(y_train) <- "activity_id"
colnames(subject_train) <- "subject_id"

colnames(x_test) <- features[,2]
colnames(y_test) <- "activity_id"
colnames(subject_test) <- "subject_id"

colnames(activity_labels) <- c("activity_id", "activity_type")

# 1.3. Merging all data in one set
merge_train <- cbind(y_train, subject_train, x_train)
merge_test <- cbind(y_test, subject_test, x_test)
set_all_in_one <- rbind(merge_train, merge_test)
dim(set_all_in_one)

# 2. Extracts only the measurements on the mean and standard deviation for each
# measurement.

# 2.1. Reading column names
colnames <- names(set_all_in_one)

# 2.2. Create vector for defining ID, mean and standard deviation
mean_and_std <- grepl("activity_id", colnames) |
  grepl("subject_id", colnames) |
  grepl("mean..", colnames) |
  grepl("std..", colnames)

# 2.3. Making subset from set_all_in_one
set_for_mean_std <- set_all_in_one[,mean_and_std == TRUE]

# 3. Uses descriptive activity names to name the activities in the data set
set_activity_names <- merge(set_for_mean_std, activity_labels,
                            by = "activity_id",
                            all.x = TRUE)

# 4. Appropriately labels the data set with descriptive variable names.
# Done in previous steps

# 5. From the data set in step 4, creates a second, independent tidy data set
# with the average of each variable for each activity and each subject.

# 5.1. Making tidy dataset
tidy_set <- aggregate(. ~subject_id + activity_id, set_activity_names, mean)
tidy_set <- tidy_set[order(tidy_set$subject_id, tidy_set$activity_id),]

# 5.2. Write tidy dataset in .txt file
write.table(tidy_set, file = "./data/tidy_set.txt", row.names = FALSE)
