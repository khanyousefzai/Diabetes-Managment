# Simpe linear regression

# Importing the dataset
dataset = read.csv('Salary_Data.csv')
# dataset = dataset[, 2:3]



# Splitting the dataset into training set and the test set
# install.packages('catools')

library(caTools)
set.seed(123)
split = sample.split(dataset$Salary, SplitRatio = 2/3)
training_set = subset(dataset, split ==  TRUE)
test_set = subset(dataset, split == FALSE)

#Feature Scaling

#training_set[, 2:3] = scale(training_set[, 2:3])
#test_set[, 2:3] = scale(test_set[, 2:3])

#fitting training set to linear model
regressor = lm(formula = Salary ~ YearsExperience,
               data= training_set)

#No statitical stars means no significance while 3 means high significance

# predicting the test set

y_pred = predict(regressor, newdata = test_set)

# Visulaizing the training test results
library(ggplot2)
ggplot() +
  geom_point(aes(x= training_set$YearsExperience, y = training_set$Salary), 
             color = 'red') + 
  geom_line(aes(x= training_set$YearsExperience, y = predict(regressor, newdata = training_set)), 
            color = 'blue') + 
  ggtitle("Salary vs experience in training set") + 
  xlab("years of experience") + 
  ylab("Salary")

# Visulaizing the test set results
library(ggplot2)
ggplot() +
  geom_point(aes(x= test_set$YearsExperience, y = test_set$Salary), 
             color = 'black') + 
  geom_line(aes(x= training_set$YearsExperience, y = predict(regressor, newdata = training_set)), 
            color = 'blue') +
  ggtitle("Salary vs experience in test set") + 
  xlab("years of experience") + 
  ylab("Salary")

# New Algortihm
library(MASS)
library(neuralnet)
#install.packages('neuralnet')
set.seed(123)
dataset = read.csv('Nn_dataset.csv')

DataFrame <- dataset


str(DataFrame)

hist(DataFrame$NGL)

head(DataFrame,2)

apply(DataFrame,2,range)
#scale
maxValue <- apply(DataFrame, 2, max)
minValue <- apply(DataFrame, 2, min)

DataFrame <- as.data.frame(scale(DataFrame,center = minValue,scale = maxValue-minValue))

##train

#ind <- sample(1:nrow(DataFrame),300)
trainDF <- DataFrame[1:300,]
trainDF$Exercise <- NULL
#trainDF$Meal.Type <- NULL
#trainDF$MTI <- NULL


testDF <- DataFrame[301:368,]
testDF$Exercise <- NULL
#testDF$Meal.Type <- NULL
#testDF$MTI <- NULL

#

allVars <- colnames(trainDF)
predictorVars <- allVars[!allVars%in%"NGL"]
predictorVars <- paste(predictorVars,collapse = "+")
form=as.formula(paste("NGL~",predictorVars,collapse = "+"))
form
neuralModel1<-neuralnet(formula = form,hidden = c(4,2),learningrate = 0.1,linear.output = T,
                        data = trainDF)
#plot
plot(neuralModel1)


predictions <- compute(neuralModel1,testDF[,1:5])
str(predictions)

predictions <- predictions$net.result*(max(testDF$NGL)-min(testDF$NGL))+min(testDF$NGL)
actualValues <- (testDF$NGL)*(max(testDF$NGL)-min(testDF$NGL))+min(testDF$NGL)


MSE <- sum((predictions - actualValues)^2)/nrow(testDF)
MSE

#plot(testDF$NGL, predictions, col='blue', main='Real vs Predicted', 
#lines(pch=1,cex=0.9,type="p",xlab ="Actual",ylab = "Predicted")
MSE <- sum((predictions - actualValues)^2)/nrow(testDF)
MSE

#plot(testDF$NGL, predictions, col='blue', main='Real vs Predicted', pch=1,cex=0.9,type="p",xlab ="Actual",ylab = "Predicted")
#abline(0,1,col="black")
prediction <- predictions[1:68]
t <- testDF$NGL
plot(t, type ="o", col ="red", xlab="no.1", ylab="actual(red)   predicted(blue)")
lines(prediction, type = "o", col="blue")


#install.packages('neuralnet')
set.seed(123)
dataset = read.csv('Nn_dataset.csv')
library("caTools")
DataFrame <- dataset


str(DataFrame)

hist(DataFrame$NGL)

head(DataFrame,2)

apply(DataFrame,2,range)
#scale
maxValue <- apply(DataFrame, 2, max)
minValue <- apply(DataFrame, 2, min)

DataFrame <- as.data.frame(scale(DataFrame,center = minValue,scale = maxValue-minValue))
# Making dummy variables
#dataset$level2 = dataset$level^2
#dataset$level2 = dataset$level^3
#dataset$level2 = dataset$level^4
#dataset$level2 = dataset$level^5
#dataset$level2 = dataset$level^6
#dataset$level2 = dataset$level^7
#dataset$level2 = dataset$level^8
#train

#ind <- sample(1:nrow(DataFrame),300)
trainDF <- DataFrame[1:300,]
#trainDF$Exercise <- NULL
#trainDF$Meal.Type <- NULL
#trainDF$MTI <- NULL


testDF <- DataFrame[301:368,]
#testDF$Exercise <- NULL
#testDF$Meal.Type <- NULL
#testDF$MTI <- NULL

regressor = lm(formula = NGL ~ Time.Diff
               data = trainDF)
y_pred = predict(regressor, newdata = testDF)
# Visulaizing the test set results
library(ggplot2)
ggplot() +
  geom_point(aes(x= testDF$NGL, y = testDF$Time.Diff), 
             color = 'black') + 
  geom_line(aes(x= trainDF$NGL, y = predict(regressor, newdata = trainDF)), 
            color = 'blue') +
  ggtitle("NGL vs timme diff") + 
  xlab("NGL") + 
  ylab("time diffrence")