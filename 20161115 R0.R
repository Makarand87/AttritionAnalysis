############# 1.INPUT ############# 

setwd("C:/Users/makarand.ghule/Documents/AttritionAnalysis")
AttritionData <- read.csv("20161116ARFollowUp.csv")

dataset <- AttritionData

nobs <- nrow(dataset); nobs
str(dataset)
summary(dataset)

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



dataset$GenderStatus <- ifelse(dataset$Gender == 'Male', 1, 0)

summary(dataset)


library(Hmisc)
describe(dataset)
library(fBasics)
basicStats(dataset$Attrition, ci=0.95)

kurtosis(dataset[c("MaritalState" , "ExperienceInAGS", "QualAvgDuringNotice", "Last30DaysLeaveCount", "TotalExtraHoursWorked")])
skewness(dataset[c("MaritalState" , "ExperienceInAGS", "QualAvgDuringNotice", "Last30DaysLeaveCount", "TotalExtraHoursWorked")])

library(descr)

CrossTable(dataset$MaritalStatus, dataset$Availability_Filter, expected = TRUE)

chisq.test(table(dataset$MaritalStatus, dataset$Attrition))

chisq.test(table(dataset$Gender, dataset$Availability_Filter))
CrossTable(dataset$Course, dataset$Availability_Filter)
chisq.test(table(dataset$Course, dataset$Availability_Filter),simulate.p.value = TRUE)
str(dataset$ExperienceInAGS)




numeric <- data.frame(x=dataset[c("ExperienceInAGS", "EmployeeAge", "LastReviewRating", "ProdAvgDuringNotice",
             "QualAvgDuringNotice", "Last30DaysLeaveCount", "TotalExtraHoursWorked", "Distance",
             "TravelTime", "Gender")], y= as.factor(dataset$Gender))



str(numeric)
vif_func(numeric)# Toooooooooo Severe for the system
# fisher.test(dataset$Course, dataset$Availability_Filter,simulate.p.value=TRUE, B=1e7)

library(caret)


dummies <- predict(dummyVars(~ Gender, data = dataset), newdata = dataset)
head(dummies, n = 3)




binom <- data.frame(y=runif(1e5), x=runif(1e5), catVar=as.factor(sample(0:4,1e5,TRUE)))
head(binom)
c <- model.matrix(~ x + catVar,binom) 
head(c)
### Strength of Association
library(cramer)
# Toooooooooo Severe for the system
# cramer.test(dataset$ExperienceInAGS, dataset$Attrition)

library(vcd)
assocstats(table(dataset$MaritalStatus, dataset$Attrition))

TRUE########### 3. Data Exploration ############
# cor(dataset$Shift, dataset$Shift_Name)
tapply(dataset$Attrition, dataset$Experience.Range, sum)



tapply(dataset$Attrition, dataset$Shift, sum, na.rm=TRUE)
tapply(dataset$Available, dataset$Shift, sum, na.rm=TRUE)
tapply(dataset$Attrition, dataset$Shift_Name, sum)

table(dataset$Shift,dataset$EmployeeAge)
tapply(dataset$EmployeeAge, dataset$Shift, mean)
names(dataset)



sort(tapply(dataset$Attrition, dataset$RptEmployeeName, sum, na.rm=TRUE), decreasing = TRUE)


tapply(dataset$Distance, dataset$Attrition, mean, na.rm=TRUE)
tapply(dataset$Attrition, dataset$ReasonofLeaving, sum, na.rm=TRUE)

table(dataset$ReasonofLeaving, dataset$Distance)
tapply( dataset$Distance, dataset$ReasonofLeaving,mean, na.rm=TRUE)

tapply(dataset$ProdAvgDuringNotice, dataset$Gender, range)
summary(dataset$LastReviewRating)
table(dataset$Course, dataset$Gender)


# cor(dataset$LastReviewRating, dataset$ProdAvgDuringNotice)

cor(dataset$QualAvgDuringNotice, dataset$ProdAvgDuringNotice)

library(GGally)
ggpairs(dataset$Course, dataset$Gender)

########### 4. Logistic #####################
### Base Table Accuracy ###########
base <- table(dataset$Attrition);base
BaselineAccu =  base[[2]]*100/nrow(dataset);BaselineAccu

### Training and Testing Dataset ###########
library(caTools)

set.seed(100)
sample <- sample.split(dataset$Attrition , SplitRatio=0.75)
train <- subset(dataset, sample==TRUE); nrow(train)
test <- subset(dataset, sample==FALSE); nrow(test) 


logit1 <- glm(Attrition ~ ExperienceInAGS + Gender + MaritalState +
              Shift + WorkLocation + JobRole + 
              ExperienceType + QualAvgDuringNotice + Course + Last30DaysLeaveCount + 
              TotalExtraHoursWorked + Function + TransportMode, 
              family=binomial, data = train); 
summary(logit1)
# plot(logit1)

##
cat(sprintf("Log likelihood: %.3f (%d df)\n",
            logLik(logit1)[1],
            attr(logLik(logit1), "df")))
cat(sprintf("Null/Residual deviance difference: %.3f (%d df)\n",
            logit1$null.deviance-logit1$deviance,
            logit1$df.null-logit1$df.residual))
cat(sprintf("Chi-square p-value: %.8f\n",
            dchisq(logit1$null.deviance-logit1$deviance,
                   logit1$df.null-logit1$df.residual)))
cat(sprintf("Pseudo R-Square (optimistic): %.8f\n",
            cor(logit1$y, logit1$fitted.values)))
cat('\n==== ANOVA ====\n\n')
print(anova(logit1, test="Chisq"))
cat("\n")


##



#============================================================

# Evaluate model performance. 

# Generate an Error Matrix for the Linear model.

# Obtain the response from the Linear model.

MYpr <- as.vector(ifelse(predict(logit1, type="response", newdata=test) > 0.5, "TERMINATED", "ACTIVE"))
MYpr

# Generate the confusion matrix showing counts.

table(test$Attrition, MYpr,dnn=c("Actual", "Predicted"))


# Generate the confusion matrix showing proportions.

pcme <- function(actual, cl)
{
  x <- table(actual, cl)
  nc <- nrow(x)
  tbl <- cbind(x/length(actual),
               Error=sapply(1:nc,
                            function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
  return(tbl)
}
per <- pcme(test$Attrition, MYpr)
round(per, 2)

# Calculate the overall error percentage.

cat(100*round(1-sum(diag(per), na.rm=TRUE), 2))

# Calculate the averaged class error percentage.

cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))



#============================================================

# Evaluate model performance. 
# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the glm model on MFG10YearTerminationData [test].

MYpr <- predict(logit1, type="response", newdata=test)

# Remove observations with missing target.

no.miss   <- na.omit(test$Attrition)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(MYpr[-miss.list], no.miss)
} else
{
  pred <- prediction(MYpr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Linear test$Attrition")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                  label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(MYtest[c(MYinput, MYtarget)]$STATUS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(MYpr[-miss.list], no.miss)
} else
{
  pred <- prediction(MYpr, no.miss)
}
performance(pred, "auc")


















pred1 = predict(logit1, type="response")
summary(pred1)
table(train$Attrition, pred1>=0.5)


logit1Accu <- (1010+734)*100 / (1010+187+304+734);logit1Accu
sensitivity <- 734*100/(734+304);sensitivity
specificity <- 1010*100/(1010+187);specificity

#probabilities for attrition #Validation

tapply(pred1, train$Attrition, mean)



######
library(ROCR)

ROCRpred = prediction(pred1, train$Attrition)

# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")


# ROC
# plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))


# auc
as.numeric(performance(ROCRpred, "auc")@y.values)



############# out of sample ##############


pred2 = predict(logit1, type="response", newdata=test)
summary(pred2)
table(test$Attrition, pred2>=0.5)


logit1Accu2 <- (322+77)*100 / (322+77+118+228);logit1Accu
sensitivity <- 228*100/(228+118);sensitivity
specificity <- 322*100/(322+77);specificity

#probabilities for attrition #Validation

tapply(pred2, test$Attrition, mean)



######
library(ROCR)

ROCRpred = prediction(pred1, train$Attrition)

# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")


# ROC
# plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))


# auc
as.numeric(performance(ROCRpred, "auc")@y.values)




 ###################### Regression Diagnostics  ################
############################################################
library(car)
outlierTest(logit1)
# qqPlot(logit1, main="QQ Plot")
car::vif(logit1)






