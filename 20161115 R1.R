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

simple <- sampledata2[c("MaritalStatus", "MaritalState", "Course", "TransportMode")]
summary(simple)
set.seed(144)
imputed = complete(mice(simple))
summary(imputed)
sampledata2$MaritalStatus = imputed$MaritalStatus
sampledata2$Course = imputed$Course
sampledata2$TransportMode = imputed$TransportMode
sampledata2$MaritalState = imputed$MaritalState


library(descr)

CrossTable(sampledata2$MaritalStatus, sampledata2$Availability_Filter, expected = TRUE)
chisq.test(table(sampledata2$MaritalStatus, sampledata2$Availability_Filter))

CrossTable(sampledata2$Gender, sampledata2$Availability_Filter, expected = TRUE)
chisq.test(table(sampledata2$Gender, sampledata2$Availability_Filter))

CrossTable(sampledata2$Course, sampledata2$Availability_Filter, expected = TRUE, simulate.p.value=TRUE)
chisq.test(table(sampledata2$Course, sampledata2$Availability_Filter), simulate.p.value = TRUE)

CrossTable(sampledata2$RptEmployeeCode, sampledata2$Availability_Filter, expected = TRUE, simulate.p.value=TRUE)
chisq.test(table(sampledata2$RptEmployeeCode, sampledata2$Availability_Filter), simulate.p.value=TRUE)


CrossTable(sampledata2$Rpt2EmployeeCode, sampledata2$Availability_Filter, expected = TRUE, simulate.p.value=TRUE)
chisq.test(table(sampledata2$Rpt2EmployeeCode, sampledata2$Availability_Filter), simulate.p.value=TRUE)


CrossTable(sampledata2$ExperienceInAGS, sampledata2$Availability_Filter, expected = TRUE, simulate.p.value=TRUE)
chisq.test(table(sampledata2$ExperienceInAGS, sampledata2$Availability_Filter), simulate.p.value=TRUE)


CrossTable(sampledata2$Experience.Range, sampledata2$Availability_Filter, expected = TRUE, simulate.p.value=TRUE)
chisq.test(table(sampledata2$Experience.Range, sampledata2$Availability_Filter), simulate.p.value=TRUE)

chisq.test(table(sampledata2$EmployeeAge, sampledata2$Availability_Filter), simulate.p.value=TRUE)


CrossTable(sampledata2$Shift, sampledata2$Availability_Filter, expected = TRUE)
chisq.test(table(sampledata2$Shift, sampledata2$Availability_Filter), simulate.p.value=TRUE)

# removed from data
# CrossTable(sampledata2$PrevShift, sampledata2$Availability_Filter, expected = TRUE, simulate.p.value=TRUE)
# chisq.test(table(sampledata2$PrevShift, sampledata2$Availability_Filter))


chisq.test(table(sampledata2$TransportMode, sampledata2$Availability_Filter), simulate.p.value=TRUE)

chisq.test(table(sampledata2$WorkFacility, sampledata2$Availability_Filter), simulate.p.value=TRUE)

chisq.test(table(sampledata2$WorkLocation, sampledata2$Availability_Filter), simulate.p.value=TRUE)

chisq.test(table(sampledata2$RptEmployeeCode, sampledata2$Availability_Filter), simulate.p.value=TRUE)

chisq.test(table(sampledata2$JobRole, sampledata2$Availability_Filter), simulate.p.value=TRUE)

chisq.test(table(sampledata2$Designation, sampledata2$Availability_Filter), simulate.p.value=TRUE)

chisq.test(table(sampledata2$ReasonofLeaving, sampledata2$Availability_Filter))

chisq.test(table(sampledata2$ExperienceType, sampledata2$Availability_Filter))

chisq.test(table(sampledata2$ExitType, sampledata2$Availability_Filter))

chisq.test(table(sampledata2$LastReviewType, sampledata2$Availability_Filter))


chisq.test(table(sampledata2$LastReviewDate, sampledata2$Availability_Filter))

chisq.test(table(sampledata2$LastReviewRating, sampledata2$Availability_Filter))


CrossTable(sampledata2$ProdAvgDuringNotice, sampledata2$Availability_Filter, expected=TRUE)
chisq.test(table(sampledata2$ProdAvgDuringNotice, sampledata2$Availability_Filter))
chisq.test(table(sampledata2$ProdAvgDuringNotice, sampledata2$Availability_Filter), simulate.p.value=TRUE)



chisq.test(table(sampledata2$QualAvgDuringNotice, sampledata2$Availability_Filter), simulate.p.value=TRUE)


chisq.test(table(sampledata2$Last30DaysLeaveCount, sampledata2$Availability_Filter), simulate.p.value=TRUE)




chisq.test(table(sampledata2$TravelTimeRange, sampledata2$Availability_Filter), simulate.p.value=TRUE)




chisq.test(table(sampledata2$TravelTime, sampledata2$Availability_Filter), simulate.p.value=TRUE)




chisq.test(table(sampledata2$TotalExtraHoursWorked, sampledata2$Availability_Filter), simulate.p.value=TRUE)




chisq.test(table(sampledata2$Shift_Name, sampledata2$Availability_Filter), simulate.p.value=TRUE)




chisq.test(table(sampledata2$Distance, sampledata2$Availability_Filter), simulate.p.value=TRUE)



#
chisq.test(table(sampledata2$QualAvgDuringNotice, sampledata2$Availability_Filter), simulate.p.value=TRUE)



#
chisq.test(table(sampledata2$QualAvgDuringNotice, sampledata2$Availability_Filter), simulate.p.value=TRUE)



#
chisq.test(table(sampledata2$QualAvgDuringNotice, sampledata2$Availability_Filter), simulate.p.value=TRUE)



#
chisq.test(table(sampledata2$QualAvgDuringNotice, sampledata2$Availability_Filter), simulate.p.value=TRUE)

