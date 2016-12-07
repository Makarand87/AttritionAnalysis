############# 1.INPUT ############# 

setwd("C:/Users/makarand.ghule/Documents/AttritionAnalysis")
AttritionData <- read.csv("20161116ARFollowUp.csv", na.strings = c("NA", ""))

dataset <- AttritionData

nobs <- nrow(dataset); nobs
# str(dataset)
# summary(dataset)

tapply(dataset$Attrition, dataset$WorkLocation, sum)
tapply(dataset$Available, dataset$WorkLocation, sum)
# 
# #sample1
# sampledata <- dataset[sample(nrow(dataset),500),]
# nrow(sampledata)
# 
# str(sampledata)
#sample2
# library(dplyr)
# set.seed(100)
# sampledata <- sample_n(dataset, 2*nrow(dataset)/3)
# nrow(sampledata)
# table(sampledata$Availability_Filter)

# summary(sampledata)
# Install and load caTools package
# install.packages("caTools")




############# 2.Data Preparation #############

# LastReviewRating has 1019 NA so excluding > LastReviewRating, LastReviewType, Rating.Bracket LastReviewDate, Rating.Bracket
# Distance, RptSpanofControl, 
#LastButOneReviewRating > 968 NA so excluding > LastButOneReviewType, LastButOneReviewDate, LastButOneReviewRating
# removing redundent variables > ProdAvgDuringNoticeRange	QualAvgDuringNoticeRange	ProdAvgBeforeNoticeRange	QualAvgBeforeNoticeRange
# Last30DaysLeaveCount changed for 3 employee  if gt 30  -> 30
# TotalExtraHoursWorked changed for 2 employee # if gt 180 -> 180 (Ø 180 ie 4W*5D*9Hrs)
# TravelTime changed for 99 employee -> 90 (min)

# NAs
# TravelTimeRange 1333
# RptSpanofControl 1037
# Distance 1333
# PrevEmployer 1160
# PrevShift :768  
# PrevworkFacility  PrevWorkLocation 611
# StaffingEmployeeStatus 836


#Collinear with work location
# CurrentAddressCity CurrentAddressCity

# PermenantAddressCity  PermenantAddressPincode
# RptEffectiveFrom 
# PrevWorkFacility  PrevShift PrevWorkFacility PrevWorkLocation 
# Client  SubClient ProjectEffectiveFrom PrevVertical PrevProcess PrevClient PrevSubClient PrevEmployer
# Shift
# Workfacility



dataset2 <- dataset[c("EmployeeCode", "ExperienceInAGS", "EmployeeAge", "Gender", "MaritalStatus",  "WorkLocation", 
                      "JobRole", "ExperienceType", "ProdAvgDuringNotice", "QualAvgDuringNotice", "Course", "Last30DaysLeaveCount",
                      "TotalExtraHoursWorked", "Function", "Shift", "TransportMode","EngagementIndex", "Availability_Filter", "Attrition", "Available")]
nums <- sapply(dataset2, is.numeric)
numdataset2 <- dataset2[,nums]

cor(numdataset2)
library(mice) # Multivariate Imputation by Chained Equations
simple <- dataset2[c("MaritalStatus", "Course", "TransportMode", "EngagementIndex", "ProdAvgDuringNotice", "QualAvgDuringNotice")]
summary(simple)
set.seed(144)
imputed = complete(mice(simple))
summary(imputed)
dataset2$MaritalStatus = imputed$MaritalStatus
dataset2$Course = imputed$Course
dataset2$TransportMode = imputed$TransportMode
dataset2$EngagementIndex = imputed$EngagementIndex
dataset2$ProdAvgDuringNotice =imputed$ProdAvgDuringNotice
dataset2$QualAvgDuringNotice =imputed$ProdAvgDuringNotice


# Randomly split data
library(caTools)
set.seed(88)
split = sample.split(dataset2$Availability_Filter, SplitRatio = 0.75)
training=subset(dataset2, split==TRUE); nrow(training)
testing = subset(dataset2, split==FALSE); nrow(testing)


table(training$Availability_Filter, dataset$WorkLocation)

dataset3 <- rbind(training, testing)

summary(training)
tapply(training$ExperienceInAGS, training$Availability_Filter, mean)
tapply(training$EmployeeAge, training$Availability_Filter, mean)
table(training$Availability_Filter, training$Gender)
################# Model #######################


logit1 <- glm(Availability_Filter ~ ExperienceInAGS + EmployeeAge + Gender + MaritalStatus + WorkLocation + JobRole + 
                ExperienceType + ProdAvgDuringNotice + Course + Last30DaysLeaveCount + TotalExtraHoursWorked + 
                Function + Shift + TransportMode + EngagementIndex, 
              data=training, family=binomial(link="logit"))




summary(logit1)
cor(logit1$y, logit1$fitted.values)

cat(sprintf("Chi-square p-value: %.8f\n", dchisq(logit1$null.deviance-logit1$deviance, logit1$df.null-logit1$df.residual)))
library(car)
vif(logit1)


## Prdict on train data ####
predictTrain <- predict(logit1, type="response")
summary(predictTrain)
tapply(predictTrain, training$Availability_Filter, mean)

# Confusion matrix for threshold of 0.5
table(training$Availability_Filter, predictTrain > 0.5)

acc <- (1084+877)/(1084+113+161+877); acc
sen <- 877/(877+161); sen
spe <- 1084/(1084+113); spe


library(ROCR)

# Prediction function
ROCRpred = prediction(predictTrain,  training$Availability_Filter)

# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7), main="ROC Plot")


as.numeric(performance(ROCRpred, "auc")@y.values)

##############################

## Prdict on test data ####
predictTest <- predict(logit1, newdata=testing, type="response")
tapply(predictTest, testing$Availability_Filter, mean, na.rm=TRUE)

# Confusion matrix for threshold of 0.5
table(testing$Availability_Filter, predictTest > 0.5)

accTest <- (304+186)/(304+25+28+186); accTest
senTest <- 186/(186+28); senTest
speTest <- 304/(304+25); speTest


library(ROCR)

# Prediction function
ROCRpredTest = prediction(predictTest, testing$Availability_Filter)

# Performance function
ROCRperfTest = performance(ROCRpredTest, "tpr", "fpr")
plot(ROCRperfTest, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7), main="ROC Plot")


as.numeric(performance(ROCRpred, "auc")@y.values)



############ prediciton on all data ##########
pred2 <- predict(logit1, newdata = dataset3, type="response")

sdata <- subset(dataset3)
nrow(sdata)
t <- cbind(pred2, sdata )
head(t)

write.csv(t, file="C:/Users/makarand.ghule/Documents/AttritionAnalysis/20161116ARFollowUp_all_score2.csv", row.names=FALSE)

library(ggplot2)
pred = prediction(pred2,  dataset3$Availability_Filter)

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Linear 20161116ARFollowUp.csv Availability_Filter")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                  label=paste("AUC =", round(au, 2)))
print(p)

########################################### RANDOM FOREST ###################################


library(randomForest)
set.seed(200)
library(caret)
# install.packages("e1071")
library(e1071)

# Define cross-validation experiment
fitControl = trainControl( method = "cv", number = 10 )
cartGrid = expand.grid( .cp = (1:50)*0.01) 


rforest1 <- randomForest(Availability_Filter ~ ExperienceInAGS + EmployeeAge + Gender + MaritalStatus + WorkLocation + JobRole + 
                           ExperienceType + ProdAvgDuringNotice + Course + Last30DaysLeaveCount + TotalExtraHoursWorked + 
                           Function + Shift + TransportMode + EngagementIndex, 
                         data=training, ntree=200, nodesize=25, method = "rpart", trControl = fitControl, tuneGrid = cartGrid  )

################## prediction on training dataset
set.seed(50)
predictForest <- predict(rforest1)
table(training$Availability_Filter, predictForest)
accTrain <- (1064+948)/(1064+133+90+948); accTrain
senTrain <- 948/(948+90); senTrain
speTrain <- 1064/(1064+133); speTrain


# predictionon Test dataset

set.seed(50)
predictForest2 <- predict(rforest1, newdata=testing)
table(testing$Availability_Filter, predictForest2)
accRFTest <- (302+190)/(302+27+24+190); accRFTest
sensRFTest <- 190/(190+24); sensRFTest
specRFTest <- 302/(302+27); specRFTest



############ prediciton on all data ##########
dataset3 <- rbind(training, testing)
predRF2 <- predict(rforest1, newdata = dataset3, type = "class")

tRF <- cbind(predRF2, dataset3 )
head(tRF)

write.csv(tRF, file="C:/Users/makarand.ghule/Documents/AttritionAnalysis/20161116ARFollowUp_all_scoreRF2.csv", row.names=FALSE)

library(ggplot2)
predRF = prediction(as.numeric(predRF2),  as.numeric(dataset3$Availability_Filter))

pe <- performance(predRF, "tpr", "fpr")
au <- performance(predRF, "auc")@y.values[[1]]


plot(au, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7), main="ROC Plot")
as.numeric(performance(predAllROC2, "auc")@y.values)



randomForest::varImpPlot(rforest1, main="Variable Importance Random Forest")

# Plot the error rate against the number of trees.

plot(rforest1, main="Error Rates Random Forest")
legend("topright", c("OOB", "Current Employee", "Employee Left"), text.col=1:6, lty=1:3, col=1:3)




############################ Decision Tree ######################################


library(rpart)
library(rpart.plot)

rTree <- rpart(Availability_Filter ~ ExperienceInAGS + EmployeeAge + Gender + MaritalStatus + WorkLocation + JobRole + 
                 ExperienceType + ProdAvgDuringNotice + Course + Last30DaysLeaveCount + TotalExtraHoursWorked + 
                 Function + Shift + TransportMode + EngagementIndex, 
               data=training, method="class",  control=rpart.control(minbucket=25))
prp(rTree)
print(rTree)
printcp(rTree)
predictCARTTrain <- predict(rTree, type="class")
table(predictCARTTrain, training$Availability_Filter)
accCARTTrain <- (1032+940)/(1032+98+165+940); accCARTTrain
sensCARTTrain <- 940/(940+165); sensCARTTrain
specCARTTrain <- 1032/(1032+98); specCARTTrain

## testing Data ####
PredictROC = predict(rTree, newdata = testing, type="class")
table(PredictROC, testing$Availability_Filter)
accCARTTest <- (338+313)/(338+33+61+313); accCARTTest
sensCARTTest <- 313/(313+61); sensCARTTest
specCARTTest <- 338/(33+338); specCARTTest
library(ROCR)


## All Data ####
PredictROC = predict(rTree, newdata = dataset3, type="class")
table(PredictROC, dataset3$Availability_Filter)
accCARTAll <- (1370+1253)/(1370+131+226+1253); accCARTAll
sensCARTAll <- 1253/(1253+226); sensCARTAll
specCARTAll <- 1370/(1370+131); specCARTAll
library(ROCR)

dataset3 <- rbind(training, testing)

nrow(dataset3)
t <- cbind(PredictROC, dataset3 )
head(t)

write.csv(t, file="C:/Users/makarand.ghule/Documents/AttritionAnalysis/20161116ARFollowUp_ROCR.csv", row.names=FALSE)







predAllROC <- predict(rTree, newdata=dataset3, type="class")
predAllROC2 = prediction(as.numeric(predAllROC), as.numeric(dataset3$Availability_Filter))
perfTree <- performance(predAllROC2, "tpr", "fpr")

plot(perfTree, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7), main="ROC Plot")
as.numeric(performance(predAllROC2, "auc")@y.values)





## model comparisons
plot(perfTree, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7), main="ROC Plot")


