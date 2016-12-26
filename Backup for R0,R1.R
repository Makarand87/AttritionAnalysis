






summary(sampledata2[c("MaritalStatus", "Course", "TransportMode", "EngagementIndex")])
library(descr)

# MaritalStatus
CrossTable(sampledata2$MaritalStatus, sampledata2$Availability_Filter, expected = TRUE)
chisq.test(table(sampledata2$MaritalStatus, sampledata2$Availability_Filter))

# Gender
CrossTable(sampledata2$Gender, sampledata2$Availability_Filter, expected = TRUE)
chisq.test(table(sampledata2$Gender, sampledata2$Availability_Filter))


# RptEmployeeCode
CrossTable(sampledata2$RptEmployeeCode, sampledata2$Availability_Filter, expected = TRUE, simulate.p.value=TRUE)
chisq.test(table(sampledata2$RptEmployeeCode, sampledata2$Availability_Filter), simulate.p.value=TRUE)

# Rpt2EmployeeCode
CrossTable(sampledata2$Rpt2EmployeeCode, sampledata2$Availability_Filter, expected = TRUE, simulate.p.value=TRUE)
chisq.test(table(sampledata2$Rpt2EmployeeCode, sampledata2$Availability_Filter), simulate.p.value=TRUE)

# ExperienceInAGS
CrossTable(sampledata2$ExperienceInAGS, sampledata2$Availability_Filter, expected = TRUE, simulate.p.value=TRUE)
chisq.test(table(sampledata2$ExperienceInAGS, sampledata2$Availability_Filter), simulate.p.value=TRUE)

# Experience.Range
CrossTable(sampledata2$Experience.Range, sampledata2$Availability_Filter, expected = TRUE, simulate.p.value=TRUE)
chisq.test(table(sampledata2$Experience.Range, sampledata2$Availability_Filter))

# EmployeeAge
chisq.test(table(sampledata2$EmployeeAge, sampledata2$Availability_Filter), simulate.p.value=TRUE)

#Shift
# 08:00PM-05:00AM  have only 2 employee assigned to it
CrossTable(sampledata2$Shift, sampledata2$Availability_Filter, expected = TRUE)
chisq.test(table(sampledata2$Shift, sampledata2$Availability_Filter), simulate.p.value=TRUE)

# TransportMode
chisq.test(table(sampledata2$TransportMode, sampledata2$Availability_Filter), simulate.p.value=TRUE)

# WorkFacility
chisq.test(table(sampledata2$WorkFacility, sampledata2$Availability_Filter), simulate.p.value=TRUE)

#PrevShift
chisq.test(table(sampledata2$PrevShift, sampledata2$Availability_Filter), simulate.p.value=TRUE)

#  Prev WorkFacility
chisq.test(table(sampledata2$PrevWorkFacility, sampledata2$Availability_Filter), simulate.p.value=TRUE)

#  PrevWorkLocation
chisq.test(table(sampledata2$PrevWorkLocation, sampledata2$Availability_Filter), simulate.p.value=TRUE)

# Job Role
chisq.test(table(sampledata2$JobRole, sampledata2$Availability_Filter))

# Designation
chisq.test(table(sampledata2$Designation, sampledata2$Availability_Filter), simulate.p.value=TRUE)

# Work Location
chisq.test(table(sampledata2$WorkLocation, sampledata2$Availability_Filter))

# RptEmployeeCode
chisq.test(table(sampledata2$RptEmployeeCode, sampledata2$Availability_Filter), simulate.p.value=TRUE)


# ReasonofLeaving
chisq.test(table(sampledata2$ReasonofLeaving, sampledata2$Availability_Filter))


# ExperienceType
chisq.test(table(sampledata2$ExperienceType, sampledata2$Availability_Filter))

# ExitType
chisq.test(table(sampledata2$ExitType, sampledata2$Availability_Filter))

# CurrentAddressCity
chisq.test(table(sampledata2$CurrentAddressCity, sampledata2$Availability_Filter), simulate.p.value=TRUE)

# CurrentAddressPincode
chisq.test(table(sampledata2$CurrentAddressPincode, sampledata2$Availability_Filter), simulate.p.value=TRUE)


#  PermenantAddressCity
chisq.test(table(sampledata2$PermenantAddressCity, sampledata2$Availability_Filter), simulate.p.value=TRUE)


#  PermenantAddressPincode
chisq.test(table(sampledata2$PermenantAddressPincode, sampledata2$Availability_Filter), simulate.p.value=TRUE)



#  RptEffectiveFrom
chisq.test(table(sampledata2$RptEffectiveFrom, sampledata2$Availability_Filter), simulate.p.value=TRUE)


#  RptSpanofControl
chisq.test(table(sampledata2$RptSpanofControl, sampledata2$Availability_Filter), simulate.p.value=TRUE)


# LastReviewType
chisq.test(table(sampledata2$LastReviewType, sampledata2$Availability_Filter))


chisq.test(table(sampledata2$LastReviewDate, sampledata2$Availability_Filter))

# LastReviewRating
chisq.test(table(sampledata2$LastReviewRating, sampledata2$Availability_Filter), simulate.p.value = TRUE)




#  LastButOneReviewType
chisq.test(table(sampledata2$LastButOneReviewType, sampledata2$Availability_Filter))


#  LastButOneReviewRating
chisq.test(table(sampledata2$LastButOneReviewRating, sampledata2$Availability_Filter), simulate.p.value=TRUE)


#  LastButOneReviewDate
chisq.test(table(sampledata2$LastButOneReviewDate, sampledata2$Availability_Filter), simulate.p.value=TRUE)


# ProdAvgDuringNotice
CrossTable(sampledata2$ProdAvgDuringNotice, sampledata2$Availability_Filter, expected=TRUE)
chisq.test(table(sampledata2$ProdAvgDuringNotice, sampledata2$Availability_Filter))
chisq.test(table(sampledata2$ProdAvgDuringNotice, sampledata2$Availability_Filter), simulate.p.value=TRUE)


# QualAvgDuringNotice
chisq.test(table(sampledata2$QualAvgDuringNotice, sampledata2$Availability_Filter), simulate.p.value=TRUE)



#  ProdAvgBeforeNotice
chisq.test(table(sampledata2$ProdAvgBeforeNotice, sampledata2$Availability_Filter), simulate.p.value=TRUE)



# QualAvgBeforeNotice
chisq.test(table(sampledata2$QualAvgBeforeNotice, sampledata2$Availability_Filter))




#  ProdAvgDuringNoticeRange
chisq.test(table(sampledata2$ProdAvgDuringNoticeRange, sampledata2$Availability_Filter))

# QualAvgDuringNoticeRange
chisq.test(table(sampledata2$QualAvgDuringNoticeRange, sampledata2$Availability_Filter), simulate.p.value = TRUE)


#  ProdAvgBeforeNoticeRange
chisq.test(table(sampledata2$ProdAvgBeforeNoticeRange, sampledata2$Availability_Filter))

#  QualAvgBeforeNoticeRange
chisq.test(table(sampledata2$QualAvgBeforeNoticeRange, sampledata2$Availability_Filter))



#  Process
chisq.test(table(sampledata2$Process, sampledata2$Availability_Filter), simulate.p.value = TRUE)




#  Client
chisq.test(table(sampledata2$Client, sampledata2$Availability_Filter))
chisq.test(table(sampledata2$Client, sampledata2$Availability_Filter), simulate.p.value = TRUE)


#  SubClient
chisq.test(table(sampledata2$SubClient, sampledata2$Availability_Filter), simulate.p.value=TRUE)


# PrevVertical 
## AR support has only 1 employee left and 2 current employee
CrossTable(sampledata2$PrevVertical, sampledata2$Availability_Filter, expected = TRUE)
chisq.test(table(sampledata2$PrevVertical, sampledata2$Availability_Filter), simulate.p.value=TRUE)


# PrevProcess
chisq.test(table(sampledata2$PrevProcess, sampledata2$Availability_Filter), simulate.p.value=TRUE)


# PrevClient
chisq.test(table(sampledata2$PrevClient, sampledata2$Availability_Filter), simulate.p.value=TRUE)


# PrevSubClient
chisq.test(table(sampledata2$PrevSubClient, sampledata2$Availability_Filter), simulate.p.value=TRUE)

# PrevEmployer
# many employeer have 1 or 2 employee
CrossTable(sampledata2$PrevEmployer, sampledata2$Availability_Filter, expected=TRUE)
chisq.test(table(sampledata2$PrevEmployer, sampledata2$Availability_Filter), simulate.p.value=TRUE)


# Course
CrossTable(sampledata2$Course, sampledata2$Availability_Filter, expected = TRUE, simulate.p.value=TRUE)
chisq.test(table(sampledata2$Course, sampledata2$Availability_Filter), simulate.p.value = TRUE)


chisq.test(table(sampledata2$Last30DaysLeaveCount, sampledata2$Availability_Filter), simulate.p.value=TRUE)




chisq.test(table(sampledata2$TravelTimeRange, sampledata2$Availability_Filter), simulate.p.value=TRUE)




chisq.test(table(sampledata2$TravelTime, sampledata2$Availability_Filter), simulate.p.value=TRUE)




#  EngagementIndex
CrossTable(sampledata2$EngagementIndex, sampledata2$Availability_Filter)
chisq.test(table(sampledata2$EngagementIndex, sampledata2$Availability_Filter), simulate.p.value=TRUE)

# TotalExtraHoursWorked
chisq.test(table(sampledata2$TotalExtraHoursWorked, sampledata2$Availability_Filter))

# StaffingEmployeeStatus
chisq.test(table(sampledata2$StaffingEmployeeStatus, sampledata2$Availability_Filter), simulate.p.value=TRUE)

# Shift_Name
chisq.test(table(sampledata2$Shift_Name, sampledata2$Availability_Filter), simulate.p.value=TRUE)

# Function
chisq.test(table(sampledata2$Function, sampledata2$Availability_Filter))

# Vertical
chisq.test(table(sampledata2$Vertical, sampledata2$Availability_Filter))



# Distance
chisq.test(table(sampledata2$Distance, sampledata2$Availability_Filter), simulate.p.value=TRUE)


# Course
chisq.test(table(sampledata2$Course, sampledata2$Availability_Filter), simulate.p.value=TRUE)




#  Designation
chisq.test(table(sampledata2$Designation, sampledata2$Availability_Filter), simulate.p.value=TRUE)


###################### Gender Vs Marital Status ###############
CrossTable(sampledata2$Gender, sampledata2$MaritalStatus, expected = TRUE)
chisq.test(sampledata2$Gender, sampledata2$MaritalStatus)


##################### VIF
significant1 <- sampledata2[c("QualAvgBeforeNotice", "ProdAvgDuringNotice", "ProdAvgBeforeNotice",  "LastReviewType",  "LastButOneReviewType",
                              "ExperienceInAGS", "WorkFacility", "Last30DaysLeaveCount", "TravelTime", "EngagementIndex", "LastButOneReviewRating", 
                              "JobRole", "QualAvgDuringNotice", "StaffingEmployeeStatus", "Function", "WorkLocation", "LastReviewRating", "Shift_Name", 
                              "PermenantAddressPincode", "CurrentAddressPincode")] 
str(significant1)
summary(significant1)
source("vif_func.R")
vif_func(sampledata2)





###################### ###########################


logit1 <- glm(Availability_Filter ~ QualAvgBeforeNotice + ProdAvgDuringNotice + ProdAvgBeforeNotice + 
                ExperienceInAGS + WorkFacility + Last30DaysLeaveCount + TravelTime + EngagementIndex + JobRole +
                QualAvgDuringNotice + Function + WorkLocation + Shift_Name + PermenantAddressPincode + CurrentAddressPincode, data=sampledata2, family = "binomial" )
summary(logit1)
cor(logit1$y, logit1$fitted.values)
# library(car)
# car::vif(logit1) 
# ld_var <- attributes(alias(logit1)$Complete)$dimnames[[1]]
# summary(ld_var)


sampledata3 <- subset(sampledata2, select=-c(EmployeeCode, Designation, MaritalStatus, Course, TransportMode, EngagementIndex))
summary(sampledata2)
str(sampledata3)



library(car)
VIF





########################### 

pred1 <- predict(logit1, newdata = testing1, type="response")
summary(pred1)

head(pred1)

pred2 <- predict(logit1, newdata = rbind(training, testing), type="response")

sdata <- subset(rbind(training, testing), select=c("EmployeeCode", "Availability_Filter"))

nrow(sdata)
t <- cbind(sdata, pred2)
head(t)

write.csv(t, file="C:/Users/makarand.ghule/Documents/AttritionAnalysis/20161116ARFollowUp_all_score.csv", row.names=FALSE)




#### All Data ####

PredictROCALL = predict(rTree, newdata = dataset3, type="class")
table(PredictROCALL, testing$Availability_Filter)
accCARTTest <- (338+313)/(338+33+61+313); accCARTTest
sensCARTTest <- 313/(313+61); sensCARTTest
specCARTTest <- 338/(33+338); specCARTTest


pred = prediction(PredictROCALL[,2], Test$Reverse)
perf = performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7), main="ROC Plot")
as.numeric(performance(pred, "auc")@y.values)






######################### random forest and decision Tree ###############


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








### Leverage 
lev=hat(model.matrix(logit1))
plot(lev)
infl <- training[lev>0.2,]
infl


cook  = cooks.distance(logit1)
plot(cook,ylab="Cooks distances")
points(infl, cook[infl], col="red")