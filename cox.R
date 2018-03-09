install.packages("survival")
install.packages("KMsurv")


library(survival)
library(KMsurv) 

setwd("C:\\Users\\Aishvarya\\Desktop\\R analysis\\Cox")

setwd("\\Learn R\\Surv")

#wsurv.data <- read.csv("wealth_survival.csv",stringsAsFactors = F)
#names <-names(wsurv.data )
#head(wsurv.data[,c(3,4,9,11,12)],10)

data <- read.csv("wealth_survival.csv",stringsAsFactors = F)
names <-names(data)
head(data)

#analysing the survival times - days a customer has been with the provider

w.surv <- survfit(Surv(data$Time2Event, wsurv.data$attrition)~ 1, conf.type="none")
sum.surv <-summary(w.surv)
surv.out.df <- data.frame("Time" =sum.surv[[2]],
                          "Cust-Available"=sum.surv[[3]],
                          "Cust-Attrited"=sum.surv[[4]],
                          "Survival-Rate"=sum.surv[[6]])

#Survival Curve calculates the survival probability or rate at any given point in time.
#The function will start with 100% and gradually go down with the time.
# it gives retention rate (1-Attrition Rate) at any point in time. 
#So, it represents cumulative attrition from the start until a time t.

write.csv(file="survival.csv",surv.out.df )
plot(w.surv,
     xlab="Days since start",
     ylab="Survival Rate",
     main="Survival Rate at different time point")



survival.gender <- survfit(Surv(data$Time2Event, wsurv.data$attrition)~ wsurv.data$GENDER, conf.type="none")
plot(w.surv.gender ,
     xlab="Days since start",
     ylab="Survival Rate",
     main="Survival Rate by Gender",
     lwd=2,
     col=c("red","green"))
legend(30, .5, c("Female", "Male"),lwd=2,col=c("red","green"))

gender.surv <- survdiff(Surv(wsurv.data$Time2Event, wsurv.data$attrition)~ wsurv.data$GENDER,)
gender.surv


