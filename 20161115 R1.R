############# 1.INPUT ############# 

setwd("C:/Users/makarand.ghule/Documents/AttritionAnalysis")
AttritionData <- read.csv("20161116ARFollowUp.csv")

dataset <- AttritionData

nobs <- nrow(dataset); nobs
str(dataset)
summary(dataset)


# 
# #sample1
# sampledata <- dataset[sample(nrow(dataset),500),]
# nrow(sampledata)
# 
# str(sampledata)
#sample2
library(dplyr)
sampledata2 <- sample_n(dataset, 500)
str(sampledata2)


############# 2.Data Preparation #############
library(mice)

simple <- dataset[c("MaritalStatus", "MaritalState", "Course", "TransportMode")]
summary(simple)
set.seed(144)
imputed = complete(mice(simple))
summary(imputed)
dataset$MaritalStatus = imputed$MaritalStatus
dataset$Course = imputed$Course
dataset$TransportMode = imputed$TransportMode
dataset$MaritalState = imputed$MaritalState


library(descr)

CrossTable(dataset$MaritalStatus, dataset$Availability_Filter, expected = TRUE)
chisq.test(table(dataset$MaritalStatus, dataset$Availability_Filter))

CrossTable(dataset$Gender, dataset$Availability_Filter, expected = TRUE)
chisq.test(table(dataset$Gender, dataset$Availability_Filter))

CrossTable(dataset$Course, dataset$Availability_Filter, expected = TRUE, simulate.p.value=TRUE)
chisq.test(table(dataset$Course, dataset$Availability_Filter), simulate.p.value = TRUE)

names(dataset)
