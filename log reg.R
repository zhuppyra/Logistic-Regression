# calling necessary libraries
library(caret)

# summoning data
data_HR <- read.csv("/Volumes/HDD SAVIRA/KULIAH/SEMESTER 2/ANALISIS MULTIVARIAT/UAS AnMul/Analisis Employee Retention dengan Logistic Regression dan Cluster Analysis/HR_comma_sep.csv",
                    stringsAsFactors=TRUE)
summary(data_HR)

# checking type of dataset
str(data_HR)

# transforming integer data to categorical data
data_HR$Work_accident <- as.factor(data_HR$Work_accident)
data_HR$left <- as.factor(data_HR$left)
data_HR$promotion_last_5years <- as.factor(data_HR$promotion_last_5years)

# checking if there's any missing value
any(is.na(data_HR))

# checking each category representation within the data
xtabs(~ left + Work_accident, data=data_HR)
xtabs(~ left + promotion_last_5years, data=data_HR)
xtabs(~ left + Department, data=data_HR)
xtabs(~ left + salary, data=data_HR)

#Creating training and test sets for the logistic regression
smp_size <- floor(0.8 * nrow(data_HR))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(data_HR)), size = smp_size)

# parting the data into training and testing dataset
train <- data_HR[train_ind, ]
test <- data_HR[-train_ind, ]

# checking dimension (row, column) of testing and training data
dim(test)
dim(train)

# remove the original dataset
rm(data_HR)

# checking type of dataset within train and test data
str(train)
str(test)

# building logistic regression model
logistic<-glm(left~.,data=train,family="binomial")
summary(logistic)

# running prediction on testing data using the logistic regression model
# for a default binomial model the default predictions are of log-odds
# type = "response" gives the predicted probabilities.
pred <- predict(logistic, newdata = test, type = "response")

# storing prediction result as:
# employee leaving if predicted probabilities greater than equal to 0.5
# employee staying if predicted probabilities less than 0.5
y_pred_num <- ifelse(pred >= 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- test$left

# building a confusion matrix to obtain the accuracy of the model
confusionMatrix(factor(y_pred),factor(y_act))

# Plotting the relationship of variable satisfaction_level
# with predicted probability of employee leaving

# creating a dataframe consisted of predicted probability of leaving and
# satisfaction level
predicted.data1 <- data.frame(
  probability.of.leaving=pred,
  satisfaction_level=test$satisfaction_level)

# plotting the dataframe
ggplot(data=predicted.data1, aes(x=satisfaction_level, y=probability.of.leaving)) +
  geom_point(aes(color=satisfaction_level), size=5) +
  xlab("satisfaction_level") +
  ylab("Predicted probability of employee leaving")

# Plotting the predicted probabilities and giving color to the samples
# to show whether the employee actually left or not

# Creating a dataframe of predicted probabilites
# and whether the employee left or not
predicted.data <- data.frame(
  probability.of.leaving=pred,
  left=test$left)
predicted.data <- predicted.data[
  order(predicted.data$probability.of.leaving, decreasing=FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)

# Making the plot
ggplot(data=predicted.data, aes(x=rank, y=probability.of.leaving)) +
  geom_point(aes(color=left), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of employee leaving the company")