# With all 
logit1 <- glm(Availability_Filter ~ Last30DaysLeaveCount + EngagementIndex  
              + ExperienceInAGS + TotalExtraHoursWorked + ProdAvgDuringNotice + Function 
              + JobRole + ExperienceType  + TransportMode + WorkLocation + EmployeeAge 
              + MaritalStatus + Gender + Course + Shift 
              , data=training, family=binomial(link="logit"))


cor(logit1$y, logit1$fitted.values)
# summary(logit1)
logit1$aic

# With -Shift 
logit2 <- glm(Availability_Filter ~ Last30DaysLeaveCount + EngagementIndex  
              + ExperienceInAGS + TotalExtraHoursWorked + ProdAvgDuringNotice + Function 
              + JobRole + ExperienceType  + TransportMode + WorkLocation + EmployeeAge 
              + MaritalStatus + Gender + Course 
              , data=training, family=binomial(link="logit"))


cor(logit2$y, logit2$fitted.values)
# summary(logit1)
logit1$aic
anova(logit1, logit2) 
anova(logit1, logit2, test="LRT") #likelihood ratio test
anova(logit1, logit2, test="Chisq")
anova(logit1, logit2, test="Rao")


drop1(logit1, test="Chisq")
AIC(logit1)
step(logit1)
