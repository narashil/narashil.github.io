#STAT 6021
#Project 2
#Estrada, Leonard, Narayan, Thompson

#Load required librariies
library(DataExplorer)
library(ROCR)
library(caret)

#not to limit output in the console
options(max.print=9999999)

#read the data - Chicago Uber;January 2019; <20miles; 
udata <- read.csv("jan_19.csv", header=T)

########CLEANING && EDA####################
#check structure of data; Summary; nulls
str(udata)
# we are not considering any date or location columns in this analysis. 
#We are also removing Trip Total as it is a combiantion of many columns + Tip and Trips pooled as it is an accumulating number
udata<-udata[-c(1:3,6:9,13:21)]

#convert seconds to numeric
udata$Trip.Seconds <- as.numeric(gsub(",", "", udata$Trip.Seconds))
#Check types of data and missing data
plot_intro(udata)
udata[!complete.cases(udata),]

#omit NA rows
udata <- na.omit(udata)
udata[!complete.cases(udata),]

#Check for correlation between variables
plot_correlation(udata, type = c("all", "discrete", "continuous"),title='Correlation Matrix for Variables')

##Add a new column to identify weather a Tip was given or not. This is binary.
udata$Tipped.or.Not[udata$Tip>0]<- 'Tip'
udata$Tipped.or.Not[udata$Tip==0]  <- 'No Tip'
head(udata$Tipped.or.Not)

#make sure the variable is read correctly by R
udata$Tipped.or.Not <- as.factor(udata$Tipped.or.Not)
contrasts(udata$Tipped.or.Not)

#remove the column Tip
udata<-udata[-c(4)]
summary(udata$Trip.Miles)

#Check for correlation between variables
plot_correlation(udata, type = c("all", "discrete", "continuous"), title='Correlation Matrix for Variables')
#Check the structure of the data before proceeding with analysis
str(udata)
attach(udata)

##############################Analysis##########################
####Random Sample of Data####
#to make sure everytime we run we get the same results and also across team members
set.seed(111)

##get 5% random sample of data
sample<-sample.int(nrow(udata), floor(.05*nrow(udata)), replace = F)
#create a training dataset
train.udata<-udata[sample, ]
#check levels again to make sure they are same
contrasts(train.udata$Tipped.or.Not)

#check the summary of the new field
# No Tip    Tip 
# 354721  70824
summary(train.udata$Tipped.or.Not)

#use rest of data to test
test.udata<-udata[-sample, ]
contrasts(test.udata$Tipped.or.Not)
# No Tip     Tip 
# 6735089 1350272 
summary(test.udata$Tipped.or.Not)
#atatch training dataset
attach(train.udata)

#check variability of the response variable by different predictors
###The plots are not showing much variation 
plot_boxplot(train.udata, by = "Tipped.or.Not", ncol=3L)
plot_histogram(train.udata, ncol=3L)

#Fitting different models
#Most of the variables are highly correlated. Therefore considering Trip Miles, Fare, Additional Charges and Trips Seconds individually
# as initial models and picking one with lowest AIC and adding other uncorrelated predictors

#Initial Models
#AIC: 381408
mod_train_miles<-glm(Tipped.or.Not ~ Trip.Miles, family="binomial", data=train.udata)
summary(mod_train_miles)

#AIC: 382659
mod_train_seconds<-glm(Tipped.or.Not ~ Trip.Seconds, family="binomial", data=train.udata)
summary(mod_train_seconds)

#AIC: 378432
mod_train_fare<-glm(Tipped.or.Not ~ Fare, family="binomial", data=train.udata)
summary(mod_train_fare)

#AIC: 377277
mod_train_adch<-glm(Tipped.or.Not ~ Additional.Charges, family="binomial", data=train.udata)
summary(mod_train_adch)

#Model 2
#AIC: 376019
#Picking Fare as it has the lowest AIC and adding more predictors
mod_train_fare_adch<-glm(Tipped.or.Not ~ Additional.Charges+Fare, family="binomial", data=train.udata)
summary(mod_train_fare_adch)

############Hypothesis Testing to check which model is better######################
##Model with Trip Total and Trips Pooled

##Model with Additional Charges - checking deltaGsquared to see if it is better than a model without any predictors
#h0:beta1=0; h1:atleast one not zero
#p-value =0; Therefore this model is useful 
1-pchisq(mod_train_adch$null.deviance-mod_train_adch$deviance,1) #0

##Which model is better?
#h0:beta2=0; h1:beta2 not equal to zero
#p-value =0; Therefore we can go with Fare and Additonal Charges
1-pchisq(mod_train_adch$deviance-mod_train_fare_adch$deviance,1)#0

##generate ROC curve for the models which seemed significant with low AIC
#Very close AUC for both models

##Only Additonal Charges##
par(mfrow=c(1,2))
preds_adch<-predict(mod_train_adch,newdata=test.udata, type="response")
rates_adch<- prediction(preds_adch, test.udata$Tipped.or.Not)
roc_result_adch<-performance(rates_adch,measure="tpr", x.measure="fpr")
plot(roc_result_adch, colorize = TRUE, main="ROC Curve for Model 4")
lines(x = c(0,1), y = c(0,1), col="red")

auc_adch<-performance(rates_adch, measure = "auc")
auc_adch@y.values #55.89

##Additonal Charges and Fare##
preds_fare_adch<-predict(mod_train_fare_adch,newdata=test.udata, type="response")
rates_fare_adchl<-prediction(preds_fare_adch, test.udata$Tipped.or.Not)
roc_result_fare_adch <- performance(rates_fare_adchl,measure="tpr", x.measure="fpr")
plot(roc_result_fare_adch, colorize = TRUE, main="ROC Curve for Model 5")
lines(x = c(0,1), y = c(0,1), col="red")

auc_fare_adch<-performance(rates_fare_adchl, measure = "auc")
auc_fare_adch@y.values #58.7


#Table Method
#Threshold 0.5
confusionMatrix(as.factor(ifelse(preds_fare_adch > .50, "No Tip", "Tip")),test.udata$Tipped.or.Not)

#Threshold 0.3
confusionMatrix(as.factor(ifelse(preds_fare_adch > .30, "No Tip", "Tip")),test.udata$Tipped.or.Not)


#Plot Method
par(mfrow=c(1,2))
#Threshold 0.5
fourfoldplot(as.table(table(test.udata$Tipped.or.Not, preds_fare_adch>0.5)),color = c("#CC6666", "#99CC99"), conf.level = 0, margin = 1, main = "Confusion Matrix:0.5 Model 5")

par(mfrow=c(1,2))
#Threshold 0.3
fourfoldplot(as.table(table(test.udata$Tipped.or.Not, preds_fare_adch>0.3)),color = c("#CC6666", "#99CC99"), conf.level = 0, margin = 1, main = "Confusion Matrix:0.3 Model 5")

##Predict Data
#Our model is able to predict that the probability of getting tipped is 30.1% when fare is 27$ and the additional charge is $6
#With trainign data it was 29%. therefore our model is reliable in predicting weather the driver is tipped.
newdata<-data.frame(Fare=27, Additional.Charges=6)
predict(mod_train_fare_adch,newdata, type="response") ##30.1%

 

