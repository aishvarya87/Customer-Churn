library(MASS)
library(plyr)
library(survival)
library(KMsurv) 
library(ggplot2)
library(caret)

setwd("C:\\Users\\Aishvarya\\Desktop\\R analysis\\Cox\\telco")
getwd()

#################################################################################
#Analyze the survival time
#################################################################################

data<-read.csv("telco.csv")
names<-names(data)
head(data)

#Analyzing the survival time

survival<-survfit(Surv(data$tenure, data$mychurn)~1, conf.type="none")
summ<-summary(survival)
df<-data.frame("Time"=summ[[2]],
               "Customer-available"=summ[[3]],
               "Customer-Attrited"=summ[[4]],
               "Survival-Rate"=summ[[6]])

write.csv(file="output.csv",df)
plot(survival, xlab='Days since start',
     ylab='Survival Rate',
     main='Survival Rate at different timepoints')

##Gender##
survGender<-survfit(Surv(data$tenure, data$mychurn)~data$gender, conf.type="none")
plot(survGender, xlab='Days since start',
     ylab='Survival Rate',
     main='Survival Rate for Gender',
     lwd=2,
     col=c("Red","Blue"))
legend(30, .5, c("Female","Male"),lwd=2,col=c('red', 'blue'))

## Survival as per contract type

survContract<-survfit(Surv(data$tenure, data$mychurn)~data$Contract, conf.type="none")
plot(survContract, xlab='Days since start',
     ylab='Survival Rate',
     main='Survival Rate for Contract Type',
     lwd=2,
     col=c("Red","Blue","Green"))
legend(30, .5, c("M2M","1Year","2Year"),lwd=2,col=c('red', 'blue','green'))

##Monthly charges##
survGender<-survfit(Surv(data$MonthlyCharges, data$mychurn)~data$gender, conf.type="none")
plot(survGender, xlab='Days since start',
     ylab='Survival Rate',
     main='Survival Rate as per Monthly charges',
     lwd=2,
     col=c("Red","Blue"))
legend(30, .5, c("Female","Male"),lwd=2,col=c('red', 'blue'))


##############################################################################################
#Cox Regression
##############################################################################################

#Running Regression and getting the Hazard Ratio
cox.ph.model<-coxph(Surv(tenure,mychurn)~Contract+Dependents+MultipleLines+TechSupport+PaymentMethod, data=data)
summary(cox.ph.model)


base.hazard <- survfit(Surv(tenure, mychurn)~1,
                       data=data)

basehaz(cox.ph.model)

######Running Regression with strata of Gender

cox.ph.model<-coxph(Surv(tenure,mychurn)~(Contract+Dependents+MultipleLines+TechSupport+PaymentMethod)*strata(gender), data=data)
summary(cox.ph.model)


#####################################################################
#Random Forest
#####################################################################

df_rf<-read.csv("telco.csv",stringsAsFactors = FALSE)

#check variable types
str(df_rf)

#Summary stats
summary(df_rf)

# Check Target Variable level - counts and %
table(df_rf$Churn)
table(df_rf$Churn)*100/nrow(df_rf)

# Name of  variables
names(df_rf)

# Change Target Variable as Factor if we want to build classification model

df_rf$Churn <- as.factor(df_rf$Churn)

# -----------------  Build Random Forest -----------------------------
library(randomForest)
rf_mod <- randomForest(Churn~MonthlyCharges, data =df_rf )

# Save the Random Forest Model
saveRDS(rf_mod,file="rf_model")

# remove random forest model object from env
rm(rf_mod)
rf_mod

# Load Random Forest 
rf_mod <-readRDS(file="rf_model")
summary(rf_mod)

#############################################################
# Random Forest Model is useful for scoring the new data frame - testing it with a subset from the dataset

index1 <- sample(1:nrow(df_rf),100,replace = F)
names(df_rf)
smp_rf <- df_rf[index1,c(19,21)]
# Score a new data
NewPredictions <- predict(rf_mod, smp_rf) 

head(NewPredictions)

table(NewPredictions)

print(NewPredictions)

