############# 1.INPUT ############# 




setwd("C:/Users/makarand.ghule/Documents/AttritionAnalysis")
AttritionData <- read.csv("20161111ARFollowUp3.csv")

dataset <- AttritionData

nobs <- nrow(dataset); nobs
str(dataset)
summary(dataset)

############# 2.Data Preparation #############



library(mice)

simple <- dataset[c("MaritalStatus", "Course","TransportMode", "MaritalState")]
summary(simple)
set.seed(144)
imputed = complete(mice(simple))
summary(imputed)
dataset$MaritalStatus = imputed$MaritalStatus
dataset$Course = imputed$Course
dataset$TransportMode = imputed$TransportMode
dataset$MaritalState = imputed$MaritalState



dataset$GenderStatus <- ifelse(dataset$Gender == 'Male', 1, 0)
