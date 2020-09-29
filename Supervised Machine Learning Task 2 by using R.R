#Simple Linear Regression
#In this regression task we will predict the percentage of marks that a student is expected to score based upon the number of hours they studied. This is a simple linear regression task as it involves just two variables.
# View data from remote link
Task2=read.csv(url("http://bit.ly/w-data"))
View(Task2)
x <- Task2$Hours
y <- Task2$Scores

#Plot scatter plot to check relation between the data
plot(x, y, main = "Relation between study hours and Percentage scores",
     xlab = "Study Hours", ylab = " Percentage Score",
     pch = 19, frame = FALSE)
# Add regression line
plot(x, y, main = "Relation between study hours and Percentage scores",
     xlab = "Study Hours", ylab = "Percentage Score",
     pch = 19, frame = FALSE)
abline(lm(y ~ x, data = Task2), col = "blue")

#comment:From the graph above, we can clearly see that there is a positive linear relation between the number of hours studied and percentage of score.

#We have split our data into training and testing sets
n = nrow(Task2)
n
set.seed(123) # To get the same random sample each time
train.index = sample(n,floor(0.80*n))

#Train_data
train.data = Task2[train.index,]
dim(train.data)

#Test_data
test.data = Task2[-train.index,]
dim(test.data)
test.data
model1=lm(Scores~Hours,data=train.data)
summary(model1)
#Make Prediction
#A) Let's test our model by predicting on our testing set:
distPred=predict(model1, test.data)
distPred

#B) Let's create a dataset of actual and predicted results to check model performance:
actuals_preds <- data.frame(cbind(actuals=test.data$Scores, predicteds=distPred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  
correlation_accuracy
head(actuals_preds)

#C) Let's check the performance of our model:
sse <- sum((actuals_preds$predicteds - actuals_preds$actuals)^2)
sst <- sum((mean(Task2$Scores) - actuals_preds$actuals)^2)
R2 <- 1-sse/sst
R2

#Current performance of our model is R2 = 0.9366075

RMSE <- sqrt(mean((actuals_preds$actuals - actuals_preds$predicteds)^2))
RMSE
library(MLmetrics)
RMSE(actuals_preds$predicteds,actuals_preds$actuals)
MAPE(actuals_preds$predicteds,actuals_preds$actuals)

#Conclusion - Performance of our improved model is good with R2 = 0.9366075. It means our model can explain about 93% variance in our test data.

# new x=9.25 hours study

# Model building
model2=lm(y~x,data=Task2)
summary(model2)

# Predictions of marks for 9.25 hours study
test2= data.frame('x'=9.25)
distPred=predict(model2, test2)
distPred

# So predicted socre is 92.9 or 93