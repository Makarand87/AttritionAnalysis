############# 1.INPUT ############# 

setwd("C:/Users/makarand.ghule/Documents/AttritionAnalysis")
AttritionData <- read.csv("20161116ARFollowUp.csv", na.strings = c("NA", ""))

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
seed(100)
sampledata2 <- sample_n(dataset, 500)
summary(sampledata2)


############# 2.Data Preparation #############
library(mice)
# LastReviewRating has 257 NA so excluding> LastReviewRating, LastReviewType, Rating.Bracket, 
# Distance, RptSpanofControl, LastButOneReviewRating, 


simple <- sampledata2[c("MaritalStatus", "Course", "TransportMode", "EngagementIndex")]
summary(simple)
set.seed(144)
imputed = complete(mice(simple))
summary(imputed)
sampledata2$MaritalStatus = imputed$MaritalStatus
sampledata2$Course = imputed$Course
sampledata2$TransportMode = imputed$TransportMode
sampledata2$EngagementIndex = imputed$EngagementIndex


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
source()