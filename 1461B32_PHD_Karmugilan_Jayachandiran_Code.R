# Removing all existing variables from Enviornment & Setting Working Directory

rm(list = ls(all=TRUE))
setwd("C:/Users/Karmugilan/Downloads/INSOFE/Test Reports/PHD")
getwd()


train_target = read.csv("C:/Users/Karmugilan/Downloads/INSOFE/Test Reports/PHD/TrainData/Train.csv")
train_target$CustomerID<-as.character(train_target$CustomerID)
sum(is.na(train_target))

# Getting the structure of the train_target dataframe

str(train_target)

# Reading the data and its structure for train_Claim data

train_claim = read.csv("C:/Users/Karmugilan/Downloads/INSOFE/Test Reports/PHD/TrainData/Train_Claim.csv",na.strings = c("?","-5","MISSINGVALUE","MISSEDDATA"))
str(train_claim)

# Changing data types for variables

train_claim$CustomerID<-as.character(train_claim$CustomerID)
train_claim$AmountOfTotalClaim<-as.integer(as.character(train_claim$AmountOfTotalClaim))



summary(train_claim$NumberOfVehicles)
#it is clear that we have only values between 1-4, hence converting it into Factor
train_claim$NumberOfVehicles<-as.factor(as.character(train_claim$NumberOfVehicles))

summary(train_claim$IncidentAddress)
#1000 levels is a threat to data, but still keeping it as such and will perform further processing later

summary(train_claim$BodilyInjuries)
#it is clear that we have only values between 0-2, hence converting it into Factor
train_claim$BodilyInjuries<-as.factor(as.character(train_claim$BodilyInjuries))


# Splitting DateofIncident into day, month, year, weekday for better idea/understanding and removing DateOfIncident variable

library(lubridate)
train_claim$DateOfIncident<-as.Date(train_claim$DateOfIncident,"%m/%d/%Y")
train_claim$dayofincident<-as.factor(day(train_claim$DateOfIncident))
train_claim$monthofincident<-as.factor(month(train_claim$DateOfIncident))
train_claim$yearofincident<-as.factor(year(train_claim$DateOfIncident))
train_claim$weekdayofincident<-as.factor(weekdays(train_claim$DateOfIncident))
train_claim$DateOfIncident<-NULL


# Checking For NA values column wise

colSums(is.na(train_claim))


# Based on the business case, few varibles left un-imputed. NA values of these variables are converted into another levels

summary(train_claim$TypeOfCollission)
levels(train_claim$TypeOfCollission)<-c("Front Collision","Rear Collision","Side Collision","Not Mentioned")
train_claim$TypeOfCollission[is.na(train_claim$TypeOfCollission)]<-"Not Mentioned"

summary(train_claim$IncidentTime)
train_claim$IncidentTime<-as.factor(as.character(train_claim$IncidentTime))
levels(train_claim$IncidentTime)<-c("0","1","10","11","12","13","14","15","16","17","18","19","2","20","21","22","23","3","4","5","6","7","8","9","Others")
train_claim$IncidentTime[is.na(train_claim$IncidentTime)]<-"Others"

summary(train_claim$PropertyDamage)
levels(train_claim$PropertyDamage)<-c("YES","NO","Not Sure")
train_claim$PropertyDamage[is.na(train_claim$PropertyDamage)]<-"Not Sure"

summary(train_claim$Witnesses)
train_claim$Witnesses<-as.factor(as.character(train_claim$Witnesses))
levels(train_claim$Witnesses)<-c("0","1","2","3","Not Mentioned")
train_claim$Witnesses[is.na(train_claim$Witnesses)]<-"Not Mentioned"

summary(train_claim$PoliceReport)
levels(train_claim$PoliceReport)<-c("NO","YES","Not Mentioned")
train_claim$PoliceReport[is.na(train_claim$PoliceReport)]<-"Not Mentioned"


# Based upon the given data/business case, we can either add Amounts of other claims together to get the NA values of "AmountOfTotalClaim", or can impute. I Imputed. For this NA values, CentralImputation is feasible.

summary(train_claim$AmountOfTotalClaim)
sum(is.na(train_claim))
library(DMwR)
train_claim<-centralImputation(train_claim)



# Reading the Demographics Train data

train_demo = read.csv("C:/Users/Karmugilan/Downloads/INSOFE/Test Reports/PHD/TrainData/Train_Demographics.csv")
str(train_demo)


# Changing data types for variables

train_demo$CustomerID<-as.character(train_demo$CustomerID)



# Checking For NA values column wise

colSums(is.na(train_demo))


# Based on the business case, few varibles left un-imputed. NA values of these variables are converted into another levels

summary(train_demo$InsuredGender)
levels(train_demo$InsuredGender)<-c("FEMALE","MALE","OTHERS")
train_demo$InsuredGender[is.na(train_demo$InsuredGender)]<-"OTHERS"

summary(train_demo$Country)
levels(train_demo$Country)
#zipcode of these two observations have same of other customers, so even this 2 customers belong to india. Hence we have only one country, so removing that column
train_demo$Country<-NULL

summary(train_demo$InsuredAge)
#converting InsuredAge into factor, since it have only 46 values altogether.
train_demo$InsuredAge<-as.factor(as.character(train_demo$InsuredAge))
levels(train_demo$InsuredAge)




summary(train_demo$InsuredZipCode)
train_demo$InsuredZipCode<-as.factor(as.character(train_demo$InsuredZipCode))
levels(train_demo$InsuredZipCode)
#995 levels is a threat to data, but still keeping it as such and will perform further processing later
#remove latitude & Longitude variables and performing 
train_demo$latitude<-NULL
train_demo$longitude<-NULL
str(train_demo)




train_policy = read.csv("C:/Users/Karmugilan/Downloads/INSOFE/Test Reports/PHD/TrainData/Train_Policy.csv",na.strings = "-1")
str(train_policy)
train_policy$CustomerID<-as.character(train_policy$CustomerID)
train_policy$InsurancePolicyNumber<-as.factor(as.character(train_policy$InsurancePolicyNumber))
#policy number doesn't have any duplicate and it make no difference in analysis. Hence removing it.
train_policy$InsurancePolicyNumber<-NULL
summary(train_policy$CustomerLoyaltyPeriod)

# train_policy$CustomerLoyaltyPeriod<-as.factor(as.character(train_policy$CustomerLoyaltyPeriod))
#getting 479 levels, hence for now, not considering changing it into factor
train_policy$DateOfPolicyCoverage<-as.Date(train_policy$DateOfPolicyCoverage,"%m/%d/%Y")

train_policy$yearofpolicy<-as.factor(year(train_policy$DateOfPolicyCoverage))
train_policy$monthofpolicy<-as.factor(month(train_policy$DateOfPolicyCoverage))
train_policy$dayofpolicy<-as.factor(day(train_policy$DateOfPolicyCoverage))
train_policy$weekdayofpolicy<-as.factor(weekdays(train_policy$DateOfPolicyCoverage))
train_policy$DateOfPolicyCoverage<-NULL

summary(train_policy$Policy_Deductible)
# train_policy$Policy_Deductible<-as.factor(as.character(train_policy$Policy_Deductible))
# getting 1496 levels, hence for now, not considering changing it into factor





summary(train_policy$UmbrellaLimit)
train_policy$binumblim = cut(train_policy$UmbrellaLimit,breaks = c(-Inf,-550000,-500000,-450000,-400000,-350000,-300000,-250000,-200000,-150000,-100000,-50000,0,50000,100000,150000,200000,250000,300000,350000,400000,450000,500000,550000,Inf),labels = 1:24)
summary(train_policy$binumblim)
train_policy$UmbrellaLimit<-NULL




colSums(is.na(train_policy))
summary(train_policy$PolicyAnnualPremium)
train_policy<-centralImputation(train_policy)
str(train_policy)




train_vehi = read.csv("C:/Users/Karmugilan/Downloads/INSOFE/Test Reports/PHD/TrainData/Train_Vehicle.csv",na.strings = "???")
str(train_vehi)
sum(is.na(train_vehi))

library(data.table)
train_vehicle<-dcast(train_vehi,CustomerID ~ VehicleAttribute, value.var = "VehicleAttributeDetails")
str(train_vehicle)
train_vehicle$VehicleYOM<-as.factor(train_vehicle$VehicleYOM)
train_vehicle$VehicleModel<-as.factor(train_vehicle$VehicleModel)
train_vehicle$VehicleMake<-as.factor(train_vehicle$VehicleMake)
rm(train_vehi)
#vehicle ID doesn't make much impact, hence removing it.
train_vehicle$VehicleID<-NULL
train_vehicle$CustomerID<-as.character(train_vehicle$CustomerID)
# write.csv(train_vehicle,"vehicle data.csv",row.names = F)
colSums(is.na(train_vehicle))
summary(train_vehicle$VehicleMake)
levels(train_vehicle$VehicleMake)<-c("Accura","Audi","BMW","Chevrolet","Dodge","Ford","Honda","Jeep","Mercedes","Nissan","Saab","Suburu","Toyota","Volkswagen","Others")
train_vehicle$VehicleMake[is.na(train_vehicle$VehicleMake)]<-"Others"

str(train_vehicle)





#merging all these data frames to get combined train data set
train_data<-merge(merge(merge(merge(x = train_vehicle,y = train_claim,by = "CustomerID"),
                              y = train_policy,by = "CustomerID"),
                        y = train_demo,by = "CustomerID"),
                  y = train_target,by = "CustomerID")
sum(is.na(train_data))
str(train_data)



#performing all these pre-processing steps for test data

#Since we don't have target variable in test data and also the sample submission file have random customerID rows, which is not getting supported properly. Hence using Sample submission for the respective customerID.

test_sample = read.csv("C:/Users/Karmugilan/Downloads/INSOFE/Test Reports/PHD/Sample.csv")
str(test_sample)
test_sample$CustomerID<-as.character(test_sample$CustomerID)



test_claim = read.csv("C:/Users/Karmugilan/Downloads/INSOFE/Test Reports/PHD/TestData/Test_Claim.csv",na.strings = c("?","-5","MISSINGVALUE","MISSEDDATA"))

test_claim$CustomerID<-as.character(test_claim$CustomerID)
test_claim$AmountOfTotalClaim<-as.integer(as.character(test_claim$AmountOfTotalClaim))

str(test_claim)
library(lubridate)
test_claim$DateOfIncident<-as.Date(test_claim$DateOfIncident,"%Y-%m-%d")
test_claim$dayofincident<-as.factor(day(test_claim$DateOfIncident))
test_claim$monthofincident<-as.factor(month(test_claim$DateOfIncident))
test_claim$yearofincident<-as.factor(year(test_claim$DateOfIncident))
test_claim$weekdayofincident<-as.factor(weekdays(test_claim$DateOfIncident))
test_claim$DateOfIncident<-NULL


colSums(is.na(test_claim))
summary(test_claim$TypeOfCollission)
levels(test_claim$TypeOfCollission)<-c("Front Collision","Rear Collision","Side Collision","Not Mentioned")
test_claim$TypeOfCollission[is.na(test_claim$TypeOfCollission)]<-"Not Mentioned"

summary(test_claim$IncidentTime)
test_claim$IncidentTime<-as.factor(as.character(test_claim$IncidentTime))
levels(test_claim$IncidentTime)<-c("0","1","10","11","12","13","14","15","16","17","18","19","2","20","21","22","23","3","4","5","6","7","8","9","Others")
test_claim$IncidentTime[is.na(test_claim$IncidentTime)]<-"Others"

summary(test_claim$PropertyDamage)
levels(test_claim$PropertyDamage)<-c("YES","NO","Not Sure")
test_claim$PropertyDamage[is.na(test_claim$PropertyDamage)]<-"Not Sure"

summary(test_claim$Witnesses)
test_claim$Witnesses<-as.factor(as.character(test_claim$Witnesses))
levels(test_claim$Witnesses)<-c("0","1","2","3","Not Mentioned")
test_claim$Witnesses[is.na(test_claim$Witnesses)]<-"Not Mentioned"

summary(test_claim$PoliceReport)
levels(test_claim$PoliceReport)<-c("NO","YES","Not Mentioned")
test_claim$PoliceReport[is.na(test_claim$PoliceReport)]<-"Not Mentioned"

summary(test_claim$AmountOfTotalClaim)
sum(is.na(test_claim))
test_claim<-centralImputation(test_claim)

summary(test_claim$NumberOfVehicles)
test_claim$NumberOfVehicles<-as.factor(as.character(test_claim$NumberOfVehicles))

summary(test_claim$BodilyInjuries)
test_claim$BodilyInjuries<-as.factor(as.character(test_claim$BodilyInjuries))

levels(test_claim$IncidentAddress)
#999 levels is a threat to data, but still keeping it as such and will perform further processing later. Train set have 1000 levels, 1 level is in difference. 


colSums(is.na(test_claim))
str(test_claim)




test_demo = read.csv("C:/Users/Karmugilan/Downloads/INSOFE/Test Reports/PHD/TestData/Test_Demographics.csv")
str(test_demo)
colSums(is.na(test_demo))
test_demo$CustomerID<-as.character(test_demo$CustomerID)

summary(test_demo$InsuredGender)
levels(test_demo$InsuredGender)<-c("FEMALE","MALE","OTHERS")
test_demo$InsuredGender[is.na(test_demo$InsuredGender)]<-"OTHERS"

summary(test_demo$InsuredZipCode)
test_demo$InsuredZipCode<-as.factor(as.character(test_demo$InsuredZipCode))
levels(test_demo$InsuredZipCode)
#994 levels is a threat to data, but still keeping it as such and will perform further processing later. Train set have 995 levels, 1 level is in difference.

levels(test_demo$Country)
summary(test_demo$Country)
#zipcode of these to observations have some of other customers, so even these 4 customers belong to india. Hence we have only one country, so removing that column
test_demo$Country<-NULL

summary(test_demo$InsuredAge)
test_demo$InsuredAge<-as.factor(as.character(test_demo$InsuredAge))
levels(test_demo$InsuredAge)

#remove latitude & Longitude variables and performing 
test_demo$latitude<-NULL
test_demo$longitude<-NULL




test_policy = read.csv("C:/Users/Karmugilan/Downloads/INSOFE/Test Reports/PHD/TestData/Test_Policy.csv",na.strings = "-1")
str(test_policy)

test_policy$CustomerID<-as.character(test_policy$CustomerID)
test_policy$InsurancePolicyNumber<-as.factor(as.character(test_policy$InsurancePolicyNumber))
#policy number doesn't have any duplicate and it make no difference in analysis. Hence removing it.
test_policy$InsurancePolicyNumber<-NULL


test_policy$DateOfPolicyCoverage<-as.Date(test_policy$DateOfPolicyCoverage,"%Y-%m-%d")
test_policy$yearofpolicy<-as.factor(year(test_policy$DateOfPolicyCoverage))
test_policy$monthofpolicy<-as.factor(month(test_policy$DateOfPolicyCoverage))
test_policy$dayofpolicy<-as.factor(day(test_policy$DateOfPolicyCoverage))
test_policy$weekdayofpolicy<-as.factor(weekdays(test_policy$DateOfPolicyCoverage))
test_policy$DateOfPolicyCoverage<-NULL

summary(test_policy$UmbrellaLimit)
test_policy$binumblim = cut(test_policy$UmbrellaLimit,breaks = c(-Inf,-550000,-500000,-450000,-400000,-350000,-300000,-250000,-200000,-150000,-100000,-50000,0,50000,100000,150000,200000,250000,300000,350000,400000,450000,500000,550000,Inf),labels = 1:24)
summary(test_policy$binumblim)
test_policy$UmbrellaLimit<-NULL

sum(is.na(test_policy))
summary(test_policy$PolicyAnnualPremium)
test_policy<-centralImputation(test_policy)
str(test_policy)




test_vehi = read.csv("C:/Users/Karmugilan/Downloads/INSOFE/Test Reports/PHD/TestData/Test_Vehicle.csv",na.strings = "???")
str(test_vehi)
sum(is.na(test_vehi))

library(data.table)
test_vehicle<-dcast(test_vehi,CustomerID ~ VehicleAttribute, value.var = "VehicleAttributeDetails")
str(test_vehicle)
test_vehicle$VehicleYOM<-as.factor(test_vehicle$VehicleYOM)
test_vehicle$VehicleModel<-as.factor(test_vehicle$VehicleModel)
#38 levels in the data, Train set have 39 levels, 1 level is in difference.
test_vehicle$VehicleMake<-as.factor(test_vehicle$VehicleMake)
rm(test_vehi)
#vehicle ID doesn't make much impact, hence removing it.
test_vehicle$VehicleID<-NULL
test_vehicle$CustomerID<-as.character(test_vehicle$CustomerID)
# write.csv(test_vehicle,"vehicle data.csv",row.names = F)
colSums(is.na(test_vehicle))

summary(test_vehicle$VehicleMake)
levels(test_vehicle$VehicleMake)<-c("Accura","Audi","BMW","Chevrolet","Dodge","Ford","Honda","Jeep","Mercedes","Nissan","Saab","Suburu","Toyota","Volkswagen","Others")
test_vehicle$VehicleMake[is.na(test_vehicle$VehicleMake)]<-"Others"

str(test_vehicle)



#merging all these data frames to get combined test data set
test_data<-merge(merge(merge(merge(x = test_vehicle,y = test_claim,by = "CustomerID"),
                             y = test_policy,by = "CustomerID"),
                       y = test_demo,by = "CustomerID"),
                 y = test_sample,by = "CustomerID")
write.csv(test_data,"test_data binned.csv",row.names = F)
str(test_data)



train_data<- read.csv("train_data binned.csv",header = T)
test_data<-read.csv("test_data binned.csv",header = T)
str(train_data)




#correlation plot
install.packages("corrplot")
library(corrplot)
corrplot(train_data[,c(18,19,20,21,26,29,30,40,41)],train_data[,c(18,19,20,21,26,29,30,40,41)],method = "square",corr = F,conf.level=conf.level)






#Logistic regression model
library(glmnet)
logreg <-glm(ReportedFraud ~ ., data=train_data[,-1], family=binomial) 
preds<-predict(logreg,newdata=test_data[,-1],type="response")
pred_glm<-ifelse(preds>0.50, "Y", "N")
write.csv(x = pred_glm,file = "predictions GLM.csv")
# Your answer passed the tests! Your score is 62.36%
# Auxiliary metrics => Precision=60.74587% and Recall=64.07137%




#Decision Trees - C50
library(C50)
c5_tree <- C5.0(ReportedFraud~., train_data[,-1])
preds <- predict(c5_tree, test_data[,-1])
write.csv(x = preds,file = "predictions c50.csv")

# Your answer passed the tests! Your score is 89.1%
# Auxiliary metrics => Precision=95.24689% and Recall=83.6983%


# Decision Trees - CART
library(rpart)
cart_tree <- rpart(ReportedFraud ~.,data=train_data[,-1],method="class")
cartpred=predict(cart_tree,newdata=test_data[,-1],type = "class")
write.csv(x = cartpred,file = "predictions cart.csv")

# Your answer passed the tests! Your score is 89.1%
# Auxiliary metrics => Precision=95.24689% and Recall=83.6983%



#improving by each variable
train_data$AmountOfTotalClaim<-NULL
test_data$AmountOfTotalClaim<-NULL

c5_tree <- C5.0(ReportedFraud~., train_data[,-1])
preds <- predict(c5_tree, test_data[,-1])
write.csv(x = preds,file = "predictions c50.csv")
#don't have any impact on the output.
# Your answer passed the tests! Your score is 89.1%
# Auxiliary metrics => Precision=95.24689% and Recall=83.6983%

cart_tree <- rpart(ReportedFraud ~.,data=train_data[,-1],method="class")
cartpred=predict(cart_tree,newdata=test_data[,-1],type = "class")
write.csv(x = cartpred,file = "predictions cart.csv")
#don't have any impact on the output.
# Your answer passed the tests! Your score is 89.1%
# Auxiliary metrics => Precision=95.24689% and Recall=83.6983%




train_data$CustomerLoyaltyPeriod<-as.factor(as.character(train_data$CustomerLoyaltyPeriod))
test_data$CustomerLoyaltyPeriod<-as.factor(as.character(test_data$CustomerLoyaltyPeriod))

c5_tree <- C5.0(ReportedFraud~., train_data[,-1])
preds <- predict(c5_tree, test_data[,-1])
write.csv(x = preds,file = "predictions c50.csv")
#don't have any impact on the output.
# Your answer passed the tests! Your score is 89.1%
# Auxiliary metrics => Precision=95.24689% and Recall=83.6983%

cart_tree <- rpart(ReportedFraud ~.,data=train_data[,-1],method="class")
cartpred=predict(cart_tree,newdata=test_data[,-1],type = "class")
write.csv(x = cartpred,file = "predictions cart.csv")
#don't have any impact on the output.
# Your answer passed the tests! Your score is 89.1%
# Auxiliary metrics => Precision=95.24689% and Recall=83.6983%





# Random Forest
library(randomForest)
mergeddata<-rbind(train_data,test_data)
test_data$ReportedFraud <- preds
modelrf<- randomForest(mergeddata[,-c(1,45)],mergeddata$ReportedFraud,do.trace = T, ntree=500)
summary(modelrf)
predrf <- predict(modelrf,mergeddata[,-c(1,45)])
write.csv(x = predrf,file = "predictions rf.csv")
#splitting data for respective data and tested result
# Your answer passed the tests! Your score is 48.09%
# Auxiliary metrics => Precision=89.701% and Recall=32.84672%




# H2o BGM
library(h2o)
localH2O = h2o.init()
# Initialize H2O
h2o.init()
# Prepare train and test data
train_data$DateOfPolicyCoverage<-as.numeric(train_data$DateOfPolicyCoverage)
test_data$DateOfPolicyCoverage<-as.numeric(test_data$DateOfPolicyCoverage)

train_data$DateOfIncident<-as.numeric(train_data$DateOfIncident)
test_data$DateOfIncident<-as.numeric(test_data$DateOfIncident)

str(train_data)
str(test_data)

# Define dependent and independent variables
h2o.train=as.h2o(train_data[,-1])
h2o.test=as.h2o(test_data[,-1])
y.dep=40
x.indep=1:39

#Generate GBM in H2O
gbm.model <- h2o.gbm(y=y.dep, x=x.indep, training_frame = h2o.train, ntrees = 1000, max_depth = 4, learn_rate = 0.01, seed = 1082)
predict.gbm <- as.data.frame(h2o.predict(gbm.model, h2o.test))
predictionsGBM<-predict.gbm$predict
write.csv(x = predictionsGBM,file = "predictions h2o GBM no pp.csv")
# Your answer passed the tests! Your score is 85.74%
# Auxiliary metrics => Precision=87.93691% and Recall=83.65775%

#Generate rf in H2O
rf.model <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = h2o.train, ntrees = 2000, max_depth = 4,mtries = 3,seed = 845)
predict.rf <- as.data.frame(h2o.predict(rf.model, h2o.test))
predictionsrf<-predict.rf$predict
write.csv(x = predictionsrf,file = "predictions h2o rf no pp.csv")
# Your answer passed the tests! Your score is 89.1%
# Auxiliary metrics => Precision=95.24689% and Recall=83.6983%





#load e1071 library and invoke naiveBayes method
library(e1071)
nb_model <- naiveBayes(ReportedFraud~.,data = train_data[,-1])
#Lets test the model
nb_test_predict <- predict(nb_model,test_data[,-1])
write.csv(x = nb_test_predict,file = "predictions NB.csv")
# Your answer passed the tests! Your score is 59.45%
# Auxiliary metrics => Precision=55.21558% and Recall=64.39578%
