######## 1. Imputed Dataset #########
dataset2_2 <- read.csv("Imputed Data.csv")
str(dataset2_2)
############# Randomly split data ##############
library(caTools)
set.seed(88)
split = sample.split(dataset2_2$Availability_Filter, SplitRatio = 0.75)
training=subset(dataset2_2, split==TRUE); nrow(training)
testing = subset(dataset2_2, split==FALSE); nrow(testing)
summary(training)
dataset3 <- rbind(training, testing)

################# 4. Model 1 (all IMP) #######################


logit1 <- glm(Availability_Filter ~ Last30DaysLeaveCount + EngagementIndex  
              + ExperienceInAGS + TotalExtraHoursWorked + ProdAvgDuringNotice + Function 
              + JobRole + ExperienceType  + TransportMode + WorkLocation + EmployeeAge 
              + MaritalStatus + Gender + Course + Shift 
              , data=training, family=binomial(link="logit"))


cor(logit1$y, logit1$fitted.values)

summary(logit1)


#################### 4.1 Removing Last30DaysLeaveCount #########

logit2 <- glm(Availability_Filter ~ EngagementIndex  
              + ExperienceInAGS + TotalExtraHoursWorked + ProdAvgDuringNotice + Function 
              + JobRole + ExperienceType  + TransportMode + WorkLocation + EmployeeAge 
              + MaritalStatus + Gender + Course + Shift 
              , data=training, family=binomial(link="logit"))


cor(logit2$y, logit2$fitted.values)

summary(logit2)

####### Removing ProdAvgDuringNotice ##########
logit3 <- glm(Availability_Filter ~ EngagementIndex  
              + ExperienceInAGS + TotalExtraHoursWorked + Function 
              + JobRole + ExperienceType  + TransportMode + WorkLocation + EmployeeAge 
              + MaritalStatus + Gender + Course + Shift 
              , data=training, family=binomial(link="logit"))

cor(logit3$y, logit3$fitted.values)
summary(logit3)


####### Removing Course ######
logit4 <- glm(Availability_Filter ~ EngagementIndex  
              + ExperienceInAGS + TotalExtraHoursWorked + Function 
              + JobRole + ExperienceType  + TransportMode + WorkLocation + EmployeeAge 
              + MaritalStatus + Gender + Shift 
              , data=training, family=binomial(link="logit"))

cor(logit4$y, logit4$fitted.values)
summary(logit4)

anova(logit1, logit4, test = "Chisq")
anova(logit2, logit4, test="Chisq")
anova(logit3, logit4, test="Chisq")

############## Removing Shift 
logit5 <- glm(Availability_Filter ~ EngagementIndex  
              + ExperienceInAGS + TotalExtraHoursWorked + Function 
              + JobRole + ExperienceType  + TransportMode + WorkLocation + EmployeeAge 
              + MaritalStatus + Gender 
              , data=training, family=binomial(link="logit"))
cor(logit5$y, logit5$fitted.values)

anova(logit4, logit5, test="Chisq")
anova(logit4, test="Chisq")
anova(logit5, test="Chisq")

library(MASS)
logit1.step <- stepAIC(logit1)
summary(logit1.step)
cor(logit1.step$y, logit1.step$fitted.values)


logit2.step <- stepAIC(logit2)
cor(logit2.step$y, logit2.step$fitted.values)
cor(logit1$y, logit1$fitted.values)
summary(logit2.step)

install.packages("glmnet")
library(glmnet)
