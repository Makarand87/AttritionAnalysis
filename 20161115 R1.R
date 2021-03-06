############# 1.INPUT ############# 

setwd("C:/Users/makarand.ghule/Documents/AttritionAnalysis/AttritionAnalysis")
AttritionData <- read.csv("20161116ARFollowUp.csv", na.strings = c("NA", ""))

dataset <- AttritionData

nobs <- nrow(dataset); nobs
str(dataset)
summary(dataset)
sd(dataset)



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



################ Imputation ############

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

write.csv(dataset2, file="C:/Users/makarand.ghule/Documents/AttritionAnalysis/Imputed Data.csv", row.names=FALSE)


######## Imputed Dataset #########
dataset2_2 <- read.csv("Imputed Data.csv")

continuous_vars <- c("ExperienceInAGS", "EmployeeAge", "ProdAvgDuringNotice", 
                            "QualAvgDuringNotice", "Last30DaysLeaveCount", 
                            "TotalExtraHoursWorked")


factor_vars <- c("Gender", "MaritalStatus", "WorkLocation", "JobRole", 
                           "ExperienceType", "Course", "Function", "Shift", 
                           "TransportMode", "EngagementIndex")


#######################  Engagement Index ###########
# dataset2_2$EngagementIndex <- factor(dataset2_2$EngagementIndex, levels = c("Green", "Amber", "Red"))
summary(dataset2_2)
str(dataset2_2)
sapply(dataset2_2, sd)


#######################  

# Randomly split data
library(caTools)
set.seed(88)
split = sample.split(dataset2_2$Availability_Filter, SplitRatio = 0.75)
training=subset(dataset2_2, split==TRUE); nrow(training)
testing = subset(dataset2_2, split==FALSE); nrow(testing)
summary(training)
dataset3 <- rbind(training, testing)


################### 3. Inintal Analyis ##########


## Information Value
# install.packages("smbinning")
library(smbinning)

iv_df <- data.frame(VARS=c(factor_vars, continuous_vars), IV=numeric(16))


for (factor_var in factor_vars) {
  smb <- smbinning.factor(dataset2_2, y="Attrition", x=factor_var)
  if (class(smb) != "character") {
    iv_df[iv_df$VARS == factor_var, "IV"] <- smb$iv
  }
}

for(continuous_var in continuous_vars){
  smb <- smbinning(dataset2_2, y="Attrition", x=continuous_var) 
  if(class(smb) != "character"){ 
    iv_df[iv_df$VARS == continuous_var, "IV"] <- smb$iv
  }
}
iv_df <- iv_df[order(-iv_df$IV), ]
iv_df




############
tapply(dataset2_2$Attrition, dataset2_2$WorkLocation, sum)
tapply(dataset2_2$Available, dataset2_2$WorkLocation, sum)
tapply(dataset2_2$Last30DaysLeaveCount, dataset2_2$Availability_Filter, mean)
table(dataset2_2$Shift, dataset2_2$Availability_Filter)
table(dataset2_2$MaritalStatus, dataset2_2$Availability_Filter)
MarriedLeft <- 168/(168+280); MarriedLeft
UnmarriedLeft <- 1116/(1316+1116); round(UnmarriedLeft,2)
table(dataset2_2$Course, dataset2_2$Availability_Filter)
table(training$WorkLocation, training$Availability_Filter)
tapply(training$ExperienceInAGS, training$Availability_Filter, mean)
tapply(training$EmployeeAge, training$Availability_Filter, mean)
table(training$Availability_Filter, training$Gender)




########### co-relation plot ###########
nums <- sapply(dataset2_2, is.numeric)
numdataset2 <- dataset2_2[,nums]

str(numdataset2)
cor(numdataset2)

str(factor_var)

cor <- cor(numdataset2, use="pairwise", method="pearson")
summary(cor)

# Order the correlations by their strength.
ord <- order(cor[1,])
ccor <- cor[ord, ord]
print(ccor)

# Graphically display the correlations.
# install.packages("corrplot")
library(corrplot)

corrplot(cor, mar=c(0,0,1,0))
title(main="Correlation Imputed Data.csv using Pearson",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))




################# 4. Model 1 (all IMP) #######################

#testing model without Last30DaysLeaveCount accuracy on all data
logit1 <- glm(Availability_Filter ~  EngagementIndex  
              + ExperienceInAGS + TotalExtraHoursWorked + ProdAvgDuringNotice + Function 
              + JobRole + ExperienceType  + TransportMode + WorkLocation + EmployeeAge 
              + MaritalStatus + Gender + Course + Shift 
              , data=training, family=binomial(link="logit"))


cor(logit1$y, logit1$fitted.values)

summary(logit1)




cat(sprintf("Chi-square p-value: %.8f\n", dchisq(logit1$null.deviance-logit1$deviance, logit1$df.null-logit1$df.residual)))
library(car)
vif(logit1)




############## 5.1 Prdict on train data ############
predTrain <- predict(logit1, type="response")
tapply(predTrain, training$Availability_Filter, mean)

library(ROCR)
library(InformationValue)
PredTrainROC1 = prediction(predTrain,  training$Availability_Filter)
PerfTrainROC1 = performance(PredTrainROC1, "tpr", "fpr")
plot(PerfTrainROC1, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7), main="ROC Plot")
as.numeric(performance(PredTrainROC1, "auc")@y.values)


# Confusion matrix for threshold of 0.5
table(training$Availability_Filter, predTrain > 0.5)
accTrain1 <- (1084+877)/(1084+113+161+877); accTrain1
senTrain1 <- 877/(877+161); senTrain1
speTrain1 <- 1084/(1084+113); speTrain1
1 - misClassError(training$Attrition, predTrain)


# install.packages("InformationValue")
library(InformationValue)
optimalCutoff(training$Availability_Filter, predTrain)
########## 5.2 Predict on test data ##############
predTest1 <- predict(logit1, newdata=testing, type="response")
tapply(predTest1, testing$Availability_Filter, mean, na.rm=TRUE)
optimalCutoff(testing$Availability_Filter, predTest1)

# Confusion matrix for threshold of 0.5
table(testing$Availability_Filter, predTest1 > 0.5)
accTest1 <- (304+186)/(304+25+28+186); accTest1
senTest1 <- 186/(186+28); senTest1
speTest1 <- 304/(304+25); speTest1
misClassError(testing$Attrition, predTest1 , 0.5)


PredTestROC1 = prediction(predTest1, testing$Availability_Filter)
PerfTestROC1 = performance(PredTestROC1, "tpr", "fpr")
plot(PerfTestROC1, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7), main="ROC Plot")
as.numeric(performance(PredTestROC1, "auc")@y.values)



##################### 5.3 prediciton on all data ####################
predAll1 <- predict(logit1, newdata = dataset3, type="response")
# predAll2 <- plogis(predict(logit1, dataset3, type="response"))  # predicted scores
summary(predAll1)
tapply(predAll1, dataset3$Availability_Filter, mean, na.rm=TRUE)
optimalCutoff(dataset3, predAll1)

# Confusion matrix for threshold of 0.5
table(dataset3$Availability_Filter, predAll1> 0.5)
accAll1 <- (1448+1170)/(1448+148+214+1170); accAll1
senAll1 <- 1170/(1170+214); senAll1
speAll1 <- 1448/(1448+148); speAll1
1- misClassError(dataset3$Attrition, predAll1)
sensitivity(dataset3$Attrition, predAll1)
specificity(dataset3$Attrition, predAll1)
# Concordance/Discordance

Concordance(dataset3$Attrition, predAll1)
Concordance(training$Attrition, predTrain)
Concordance(testing$Attrition, predTest1)


plotROC(dataset3$Attrition, predAll1,Show.labels = TRUE, returnSensitivityMat = TRUE)



PredAllROC1 <- prediction(predAll1, dataset3$Availability_Filter)
PerfAllROC1 <- performance(PredAllROC1, "tpr", "fpr")
plot(PerfAllROC1, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7), main="ROC Plot")
as.numeric(performance(PredAllROC1, "auc")@y.values)

################# 6 Export Model Score #################

t <- cbind(predAll1, dataset3 )
head(t)

write.csv(t, file="C:/Users/makarand.ghule/Documents/AttritionAnalysis/ARFollowUp_Logistic_Score2.csv", row.names=FALSE)


TNA <- is.na(predAll1)
NAV <- predAll1[-TNA]
nrow(NAV)




########################## 7 Diagnostic #############################


# Hosmer-Lemeshow Goodness of Fit

# How well our model fits depends on the difference between the model and the observed data.  

install.packages("ResourceSelection")
library(ResourceSelection)

hoslem.test(training$Available, fitted(logit1))




################ ULCA ##############

install.packages("packagename")
install.packages("aod")
library(aod)
library(ggplot2)
library(Rcpp)


## CIs using profiled log-likelihood
cbind(exp(logit1$coefficients), exp(confint(logit1)))
#  Wald confidence limits
cbind(exp(logit1$coefficients), exp(confint.default(logit1)))

# overall effect
wald.test(b = coef(logit1), Sigma = vcov(logit1), Terms = 6:7) #Work Location

wald.test(b = coef(logit1), Sigma = vcov(logit1), Terms = 8:9) # Job Role

wald.test(b = coef(logit1), Sigma = vcov(logit1), Terms = 10:11) # Experience Type

wald.test(b = coef(logit1), Sigma = vcov(logit1), Terms = 13:24) # Courses

wald.test(b = coef(logit1), Sigma = vcov(logit1), Terms = 28:37) # Shift

wald.test(b = coef(logit1), Sigma = vcov(logit1), Terms = 38:42) # Transport Mode

wald.test(b = coef(logit1), Sigma = vcov(logit1), Terms = 43:44) # Engagement Index


# hypotheses about the differences in the coefficients for the different levels of rank
l <- cbind(0, 0, 0, 0, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0  )
wald.test(b = coef(logit1), Sigma = vcov(logit1), L = l)




#### probabilities plot ####
dev.off()


newdata3 <- cbind(dataset3, predict(logit1, newdata=dataset3, type="link", se=TRUE))
newdata3 <- within(newdata3, {
              PredictedProb  <- plogis(fit)
              LL <- plogis(fit - (1.96*se.fit))
              UL <- plogis(fit + (1.96*se.fit))
})
head(newdata3)
library(ggplot2)


##  ExperienceinAGS by Work Location
ggplot(newdata3, aes(x=ExperienceInAGS, y=PredictedProb)) + 
  geom_ribbon(aes(ymin=LL, ymax=UL, fill=WorkLocation), alpha=0.2) +
  geom_line(aes(color=WorkLocation), size=1)

# ProdAvgDuring Notice by WorkLocation
ggplot(newdata3, aes(x = ProdAvgDuringNotice, y = PredictedProb)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = WorkLocation), alpha = .2) +
  geom_line(aes(colour = WorkLocation), size=1)

# ProdAvgDuring Notice by Gender
ggplot(newdata3, aes(x = ProdAvgDuringNotice, y = PredictedProb)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = Gender), alpha = .2) +
  geom_line(aes(colour = Gender), size=1)

# Age Vs Gender
ggplot(newdata3, aes(x = EmployeeAge, y = PredictedProb)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = Gender), alpha = .2) +
  geom_line(aes(colour = Gender), size=1)

# Age By Gender
ggplot(newdata3, aes(x = EmployeeAge, y = PredictedProb)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = MaritalStatus), alpha = .2) +
  geom_line(aes(colour = MaritalStatus), size=1)

# ExperinenceinAGS by WorkLocation
ggplot(newdata3, aes(x=ExperienceInAGS, y=PredictedProb)) + 
  geom_ribbon(aes(ymin=LL, ymax=UL, fill=WorkLocation), alpha=0.2) + 
  geom_line(aes(colour=WorkLocation), size=1)

summary(logit1)
pchisq(3087.0-1341.5, (2234-2191))
pchisq(1341.5, 2191)
anova(logit1, test="Chisq")
drop1(logit1, test="Chisq")



## over / underdispersion  ###
install.packages("AER")
library(AER)
deviance(logit1)/logit1$df.residual

# influential and leverage points,
library(car)
influencePlot(logit1, col = c(1, 2), identify.cex=par("cex"), identify.col=par("col"))
# labels = names(rstud),



