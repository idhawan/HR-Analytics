myproject <- "F:/Purdue/MGMT 59000 Using R for Analytics/R project/HR_comma_sep.csv"
hr<- read.table(file=myproject, header=T, sep = ",")


library(magrittr)
library(dplyr)
library(corrplot)
HR_correlation <- hr %>% select(Satisfaction.Level:Promotion.in.last.5yrs)
M <- cor(HR_correlation)
corrplot(M, method="circle")


hr_hist <- hr %>% filter(Left==1)
par(mfrow=c(1,3))
hist(hr_hist$Satisfaction.Level,col="#3090C7", main = "Satisfaction level") 
hist(hr_hist$Last.Evaluation,col="#3090C7", main = "Last evaluation")
hist(hr_hist$Avg.monthly.hours,col="#3090C7", main = "Average montly hours")


par(mfrow=c(1,2))
hist(hr_hist$Work.Accident,col="#3090C7", main = "Work accident")
plot(hr_hist$Salary,col="#3090C7", main = "Salary")


hr_leaving_people <- hr[hr$Left==1,]
hr_good_leaving_people <- hr_leaving_people %>% filter(Last.Evaluation >= 0.70 | Time.spent.on.job >= 4 | No.of.Projects > 5)
nrow(hr_good_leaving_people)


hr_good_leaving_people2 <- hr %>% filter(Last.Evaluation >= 0.70 | Time.spent.on.job >= 4 | No.of.Projects > 5)
hr_good_people_select <- hr_good_leaving_people2 %>% select(Satisfaction.Level, No.of.Projects: Promotion.in.last.5yrs)
M <- cor(hr_good_people_select)
corrplot(M, method="circle")
+

hr_model <- hr %>% filter(Last.Evaluation >= 0.70 | Time.spent.on.job >= 4 | No.of.Projects > 5)
summary(hr_model)


# Set the target variable as a factor
hr_model$Left <- as.factor(hr_model$Left)
## install.packages("caret") 
library("caret")
# cross-validation
train_control<- trainControl(method="cv", number=5, repeats=3)
head(train_control)

library("rpart")
library("rpart.plot")
# train the model 
rpartmodel<- train(Left~., data=hr_model, trControl=train_control, method="rpart")
# make predictions
predictions<- predict(rpartmodel,hr_model)
hr_model_tree<- cbind(hr_model,predictions)
# summarize results
confusionMatrix<- confusionMatrix(hr_model_tree$predictions,hr_model_tree$Left)
confusionMatrix


e1071model2 <- train(Left~., data=hr_model, trControl=train_control, method="nb")
# make predictions
predictions<- predict(e1071model2,hr_model)
e1071modelbinded <- cbind(hr_model,predictions)
# summarize results
confusionMatrix<- confusionMatrix(e1071modelbinded$predictions,e1071modelbinded$Left)
confusionMatrix

hr1<-subset(hr, select=-c(7:11))
library(caTools)
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)
library(klaR)
library(MASS)
library(knitr)
library(stringr)
library(grid)
library(ggplot2)
library(lattice)
library(dplyr)
library(lazyeval)
library(readxl)
library(reglogit)
library(ISLR)
library(lmtest)
library(pscl)
library(MKmisc)
gmlmodel <- train(Left~.,hr_model, trControl=train_control, method="LogitBoost")
# make predictions
predictions<- predict(gmlmodel,hr_model)
gmlmodelbinded <- cbind(hr_model,predictions)
# summarize results
confusionMatrix<- confusionMatrix(gmlmodelbinded$predictions,gmlmodelbinded$Left)
confusionMatrix





library(e1071)
# train the model 
e1071model2 <- train(Left~., data=hr_model, trControl=train_control, method="nb")
# make predictions
predictions<- predict(e1071model2,hr_model)
e1071modelbinded <- cbind(hr_model,predictions)
# summarize results
confusionMatrix<- confusionMatrix(e1071modelbinded$predictions,e1071modelbinded$Left)
confusionMatrix



library(caTools)
# make predictions
predictions<- predict(gmlmodel,hr_model)
gmlmodelbinded <- cbind(hr_model,predictions)
# summarize results
confusionMatrix<- confusionMatrix(gmlmodelbinded$predictions,gmlmodelbinded$left)
confusionMatrix


set.seed(100)
# Keep some data to test again the final model
inTraining <- createDataPartition(hr$Left, p = .75, list = FALSE)
training <- hr_model[ inTraining,]
testing  <- hr_model[-inTraining,]
# Estimate the drivers of attrition
logreg = glm(Left ~ Satisfaction.Level + Last.Evaluation + Avg.monthly.hours, family=binomial(logit), data=hr)
# Make predictions on the out-of-sample data
ProbaToLeave=predict(logreg,newdata=hr,type="response")
# Structure the prediction output in a table
predattrition = data.frame(ProbaToLeave)
# Add a column to the predattrition dataframe containing the performance
predattrition$Employee.ID=hr$Employee.ID
predattrition$Performance=hr$Last.Evaluation
predattrition$Satisfaction = hr$Satisfaction.Level
predattrition$priority=predattrition$Performance*predattrition$Performance
plot(predattrition$ProbaToLeave,predattrition$Performance)
plot(predattrition$ProbaToLeave,predattrition$Satisfaction)


for(i in 1:nrow(hr1)){
  if(predattrition[i,1] > 0.5 & predattrition[i,3] > 0.7 & predattrition[i,4] < 0.5 ){predattrition[i,"Result"] = "Promote"}
else{predattrition[i,"Result"] = "No Action needed"}
}


library(pscl)
pR2(logreg)["McFadden"]


