# The current script does the following;

# 1. Merges the training and the test sets to create one data set.

	#X training and X test Data
	xtrain_data <- read.table("train/X_train.txt")
	xtest_data <- read.table("test/X_test.txt")
	x_data <- rbind(xtrain_data, xtest_data)

	#sub training and sub test data
	subtrain_data <- read.table("train/subject_train.txt")
	subtest_data <- read.table("test/subject_test.txt")
	subj_data <- rbind(subtrain_data, subtest_data)

	#y traning and y test data
	ytrain_data <- read.table("train/y_train.txt")
	ytest_data <- read.table("test/y_test.txt")
	y_data <- rbind(ytrain_data, ytest_data)



# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

	features <- read.table("features.txt")
	cool_features <- grep("-mean\\(\\)|-std\\(\\)",features[,2])
	x_data <- x_data[,cool_features]
	names(x_data) <- tolower(features[cool_features, 2])
	names(x_data) <- gsub("\\(|\\)","",names(x_data))

# 3. Uses descriptive activity names to name the activities in the data set
	
	names(y_data) <- "activity"
	activity <- read.table("activity_labels.txt")
	activity[, 2] = gsub("_", "-", tolower(as.character(activity[, 2])))
	y_data[,1] = activity[y_data[,1],2]

# 4. Appropriately labels the data set with descriptive activity names.
	names(subj_data) <- "subject"
	tidy_data_set <- cbind(subj_data, y_data, x_data)

	#write the tidy data set
	write.table(tidy_data_set, "tidy_data_set.txt", row.names=FALSE)


# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
	
	uniqueSubjects <- unique(subj_data)[,1]
	numSubjects <- length(uniqueSubjects)

	numActivities <- nrow(activity)
	numCols <- dim(tidy_data_set)[2]
	
	
	
	data_summary <- tidy_data_set[1:(numSubjects*numActivities), ]
	
	row = 1
	for (s in 1:numSubjects) {
		
		for (a in 1:numActivities) {
			
			data_summary[row, 1] = uniqueSubjects[s]
			data_summary[row, 2] = activity[a, 2]
			idxs <- tidy_data_set[tidy_data_set$subject==s & tidy_data_set$activity==activity[a, 2], ]
			data_summary[row, 3:numCols] <- colMeans(idxs[, 3:numCols])
			row = row+1
		
		}

	}

	#write the tidy data set (avg)
	write.table(data_summary, "tidy_data_set_avg.txt", row.names=FALSE)