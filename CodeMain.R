
#Reading in the dataset and converting all missing values in the form of "0" to NA's to remove them later
Data <- read.csv2("Nn_dataset.csv", header = TRUE, sep = ",", na.strings = "0")
head(Data)
View(Data)
summary(Data)

#To know the no of NA's in each column
sapply(Data, function(x) sum(is.na(x)))

#Removing all the NA's
Dataset <- Data[complete.cases(Data),] 
head(Dataset)
View(Dataset)
summary(Dataset)

#Some visualizations to get the feel of the dataset
hist(Dataset$CGL)
hist(Dataset$NGL)
hist(Dataset$STI)
hist(Dataset$MTI)


#Matrix of scatterplots
pairs(Dataset, panel = panel.smooth)

#Logistic regression model
#Preparing the DataSet
library("stats")
set.seed(123)
n <- nrow(Dataset)
train <- sample(n, trunc(0.70*n))
Dataset_Training <- Dataset[train, ]
Dataset_Testing <- Dataset[-train, ]


# Training The Model
glm_1 <- glm(NGL ~., data = Dataset_Training)
summary(glm_1)

#Removing non significant ones from the analysis above
glm_2 <- update(glm_1, ~. - MTI - Exercise - Meal.Type - Time.Diff)
summary(glm_fm2)

par(mfrow = c(2,2))
plot(glm_2)

