# upload and view dataset
library(readxl)
data <- read_excel("dataset.xls")
View(data)

columns_we_want <- data[, c("x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15", "x16", "x17", "x18", "19")]

######################
# splitting the data randomly into two sets: 1) 60 rows, 2) 40 rows (holdout/validation sample)
#create a list of random number ranging from 1 to number of rows from actual data 
#and 60% of the data into training data  

split_data = sort(sample(nrow(columns_we_want), nrow(columns_we_want)*.6))

#creating training data set by selecting the output row values
train<-data[split_data,]

#creating test data set by not selecting the output row values
test<-data[-split_data,]


### Table 5-8 ###
# MANOVA test statistics
# creating the MANOVA model with the 3 independent variables and x4 as the dependent variable
estimation_manova <- manova(cbind(x13, x17, x11) ~ x4, data=train)
# getting the MANOVA summary using the Wilks' test
summary(estimation_manova, test="Wilks")


### not really but somewhat of Table 5-9 ###
# only missing canonical correlation, which i cant seem to find out how to do that
# may have to manually calculate it, but will get back to it after finding out how to compute discriminate z scores


### Table 5-10 ###
# confusion matrix
# matrix for train set
table(predict(hbat.da, newdata=test)$class, train$x4)
# matrix for test set
table(predict(hbat.da, newdata=test)$class, test$x4)


### Table 5-11 ###
# analysis sample
desc_z.train <- predict(hbat.da)
train$predicted_group <- desc_z.train$class
train$descr_z_score <- desc_z.train$x
table511.analysis_sample <- train[, c("id", "x4", "descr_z_score", "predicted_group")]
# holdout sample
desc_z.test <- predict(hbat.da, newdata=test)
test$predicted_group <- desc_z.test$class
test$descr_z_score <- desc_z.test$x
table511.holdout_sample <- test[, c("id", "x4", "descr_z_score", "predicted_group")]

