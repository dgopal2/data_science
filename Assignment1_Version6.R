library(rpart)
library(rpart.plot)
library(ROCR)
library(C50)

################################ Functions ##################################

roc_curve <- function(x){
  pred<- prediction(x$score, x$RESPONSE)
  roc <- performance(pred, measure = "tpr", x.measure = "fpr")
  plot(roc)
  abline(a=0,b=1)
  
  auc <- performance(pred,measure="auc")
  print(c("Area Under Curve:",auc@y.values))
  return(auc@y.values)
}

lift_chart <- function(x,y){
  
  trnSc <- subset(y, select=c("RESPONSE"))  
  trnSc$score<-x[, 2] 
  trnSc<-trnSc[order(trnSc$score, decreasing=TRUE),]
  str(trnSc)
  
  trnSc$RESPONSE<-as.numeric(as.character(trnSc$RESPONSE))
  str(trnSc)
  
  trnSc$cumDefault<-cumsum(trnSc$RESPONSE)
  head(trnSc)
  plot(seq(nrow(trnSc)), trnSc$cumDefault,type = "l", xlab='#cases', ylab='#good',main="Lift Chart")
  return(trnSc) 
}

perf_metrics <- function(x){
  print("confusion_matrix:")
  print(x)
  accuracy <- round(((x[1] + x[4])/sum(x))*100,2)
  precision <- round((x[4]/(x[3]+x[4]))*100,2)
  recall <- round((x[4]/sum(x[2,]))*100,2)
  FP_rate <- round((x[3]/sum(x[1,]))*100,2)
  print(list(c("Accuracy",accuracy),c("Precision:",precision),c("Recall:",recall),c("Specificity:",100-FP_rate)))
  
}

###########################Start of Data ##########################
cdata <- read.csv("C:/Users/DilipKumar/Desktop/572 data mining/Assignment 1/GermanCredit_assgt1_F18.csv",stringsAsFactors = FALSE)

#Missing Value Treatment 
names(cdata)[colSums(is.na(cdata)) > 0]
cdata$AGE[is.na(cdata$AGE)] <- round(mean(cdata$AGE,na.rm=TRUE),0)
cdata[is.na(cdata)] <- 0 

#Data Cleaning
cdata <- cdata[,c(-1)]
cdata_temp <- data.frame(apply(cdata[,c(-2,-22,-10)],2,as.factor))
cdata <- cbind(cdata_temp,cdata[,c(2,22,10)])

############################# Data Transformation#####################

cdata['Category'] <- ifelse(cdata$NEW_CAR==1,"New_Car",ifelse(cdata$USED_CAR==1,"Used_Car",
                     ifelse(cdata$FURNITURE==1,"Furniture",ifelse(cdata$RADIO.TV==1,"Radio_TV",
                     ifelse(cdata$EDUCATION==1,"Education",ifelse(cdata$RETRAINING==1,"Retraining","Not_Available"))))))

cdata['Property'] <- ifelse(cdata$REAL_ESTATE==1,"Real_Estate",ifelse(cdata$RENT==1,"Rent",ifelse(cdata$OWN_RES==1,"Own House",ifelse(cdata$PROP_UNKN_NONE==1,"No Property","NA"))))

cdata['Marital_Status'] <- ifelse(cdata$MALE_DIV==1,"Divorced",ifelse(cdata$MALE_SINGLE==1,"single",ifelse(cdata$MALE_MAR_or_WID==1,"Married/Widow","NA")))
cdata_n = cdata[names(cdata) %in% c("CHK_ACCT","DURATION","AGE","AMOUNT","HISTORY","SAV_ACCT","EMPLOYMENT","INSTALL_RATE","CO.APPLICANT","GUARANTOR","PRESENT_RESIDENT","AGE","OTHER_INSTALL","NUM_CREDITS","JOB","NUM_DEPENDENTS","TELEPHONE","FOREIGN","RESPONSE","Category","Property","Marital_Status")]

#Numerical Data
barplot(table(cdata_n$RESPONSE,cdata_n$Category),beside=T,col=c("pink","blue"),legend.text = c("Bad","Good"),args.legend = list(x = "topright"))
#Finding
cdata_n['Age_bucket'] <- ifelse(cdata_n$AGE>=19 & cdata_n$AGE<=27, "19-27",
                        ifelse(cdata_n$AGE>=28 & cdata_n$AGE<=35, "28-35",
                        ifelse(cdata_n$AGE>=36 & cdata_n$AGE<=42, "36-42",
                        ifelse(cdata_n$AGE>=43 & cdata_n$AGE<=49, "43-49",
                        ifelse(cdata_n$AGE>=50 & cdata_n$AGE<=56, "50-56","56+")))))
cdata_n['period'] <- ifelse(cdata_n$DURATION<=6,"6 months",
                     ifelse(cdata_n$DURATION<=12,"1 Year",
                     ifelse(cdata_n$DURATION<=18,"1 1/2 Year",
                     ifelse(cdata_n$DURATION<=24,"2 Year",
                     ifelse(cdata_n$DURATION<=36,"3 Year","3+ Year")))))
barplot(table(cdata_n$RESPONSE,cdata_n$period),beside=T,col=c("pink","blue"),legend.text = c("Bad","Good"),args.legend = list(x = "topright"))

cdata_n['amount_b'] <- ifelse(cdata_n$AMOUNT<=1000,"> $1000",
                            ifelse(cdata_n$AMOUNT<=2000,"> $2000",
                                   ifelse(cdata_n$AMOUNT<=3000,"> $3000",
                                          ifelse(cdata_n$AMOUNT<=4000,"> $4000",
                                   ifelse(cdata_n$AMOUNT<=5000,"> $5000","< $5000")))))
barplot(table(cdata_n$RESPONSE,cdata_n$amount_b),beside=T,col=c("pink","blue"),legend.text = c("Bad","Good"),args.legend = list(x = "topright"))

cdata_n <- cdata_n[!names(cdata_n)%in% c("AGE","amount_b","period")]

###################################### Model Building ###################
#Without Any extra parameters 
model <- rpart(RESPONSE~.,data=cdata_n,method="class")
rpart.plot::prp(model, type=2, extra=1)


#Accuracy 
prtn <- predict(model,data=cdata_n,type="class")
perf_metrics(table(cdata_n$RESPONSE,prtn))

#LIFT CHART

predTrnProb <- predict(model, cdata_n, type='prob')
trnSc<- lift_chart(predTrnProb,cdata_n)
AUC <- roc_curve(trnSc)

#With Any extra parameters 
model <- rpart(RESPONSE~.,data=cdata_n,method="class",control=rpart.control(minsplit = 30,cp=0.015,maxdepth = 7,xval = 10))
rpart.plot::prp(model, type=2, extra=1)


#Accuracy 
prtn <- predict(model,data=cdata_n,type="class")
perf_metrics(table(cdata_n$RESPONSE,prtn))

#LIFT CHART

predTrnProb <- predict(model, cdata_n, type='prob')
trnSc<- lift_chart(predTrnProb,cdata_n)
AUC <- roc_curve(trnSc)



#### Splitting train and test data 

nr <- nrow(cdata_n)
#Without Any parameters
index <-  sample(1:nr, size = round(0.5*nr), replace=FALSE) 
train <- cdata_n[index,]
test <- cdata_n[-index,]
model3 <- rpart(RESPONSE~.,data=train,method="class")
#control=rpart.control(minsplit = 30,cp=0.015,maxdepth = 6,xval = 10))
rpart.plot::prp(model3, type=2, extra=1)

prtn <- predict(model3,train,type="class")
perf_metrics(table(train$RESPONSE,prtn))

predTrnProb <- predict(model3, train, type='prob')
trnSc<- lift_chart(predTrnProb,train)
AUC <- roc_curve(trnSc)


#predict on test data
prtn <- predict(model3,test,type="class")
perf_metrics(table(test$RESPONSE,prtn))

#With  parameters
model4 <- rpart(RESPONSE~.,data=train,method="class",control=rpart.control(minsplit = 30,cp=0.015,maxdepth = 6,xval = 10))
rpart.plot::prp(model4, type=2, extra=1)

prtn <- predict(model4,train,type="class")
perf_metrics(table(train$RESPONSE,prtn))
predTrnProb <- predict(model4, train, type='prob')
trnSc<- lift_chart(predTrnProb,train)
AUC <- roc_curve(trnSc)


#predict on test data
prtn <- predict(model4,test,type="class")
perf_metrics(table(test$RESPONSE,prtn))

################################# C50 Library ##############################
index <-  sample(1:nr, size = round(0.80*nr), replace=FALSE) 
train <- cdata_n[index,]
test <- cdata_n[-index,] 

ruleModel <- C5.0(RESPONSE ~ ., data = train,Trial=TRUE,ruleModel=TRUE,method= "class")
summary(ruleModel)
#Variable Importance 
C5imp(ruleModel)

#Accuray
prtn <- predict(ruleModel,train,type="class")
perf_metrics(table(train$RESPONSE,prtn))
#LIFT CHART
predTrnProb <- predict(ruleModel, train, type="prob")
trnSc1<- lift_chart(predTrnProb,train)
AUC <- roc_curve(trnSc1)


prtn <- predict(ruleModel,test,type="class")
perf_metrics(table(test$RESPONSE,prtn))
#LIFT CHART
predTrnProb <- predict(ruleModel, test, type="prob")
trnSc1<- lift_chart(predTrnProb,test)
AUC <- roc_curve(trnSc1)
######################Misclassification costs###################

costMatrix <- matrix(c(0,100,500,0),byrow=TRUE,nrow=2)
model1 <- rpart(RESPONSE~.,data=cdata_n,method="class",control=rpart.control(minsplit = 30,cp=0.015,maxdepth = 6,xval = 10))
prtn <- predict(model1,data=cdata_n,type="class")
perf_metrics(table(cdata_n$RESPONSE,prtn))
#Threshold at 0.5

cm <- table(cdata_n$RESPONSE,prtn)
perf_metrics(cm)
mis_class <- cm[2]*costMatrix[3] + cm[3]*costMatrix[2]

#ROC Curve Analysis

predTrnProb <- predict(model1,cdata_n, type= "prob")
trnSc1<- lift_chart(predTrnProb,cdata_n)
AUC <- roc_curve(trnSc1)

pred <- prediction(trnSc1$score,cdata_n$RESPONSE)
roc <- performance(pred, measure = "tpr", x.measure = "fpr")
cutoffs <- data.frame(cut=roc@alpha.values[[1]], fpr=roc@x.values[[1]],tpr=roc@y.values[[1]])

new_pred <- ifelse(predTrnProb[,2]>=0.83,1,0)
cm <- table(cdata_n$RESPONSE,new_pred)
perf_metrics(cm)

mis_class <- cm[2]*costMatrix[3] + cm[3]*costMatrix[2]
costMatrix <- matrix(c(0,100,500,0),byrow=TRUE,nrow=2)
th <- costMatrix[2,1]/(costMatrix[2,1] + costMatrix[1,2])
th


################# Model With misclassification costs ###################
model1 <- rpart(RESPONSE~.,data=train,method="class",control=rpart.control(maxdepth = 6,cp=0.015,minsplit=30,xval=10),parms=list(loss=costMatrix))
prtn <- predict(model1,train,type="class")
perf_metrics(table(train$RESPONSE,prtn))
rpart.plot::prp(model1, type=2, extra=1)
predTrnProb <- predict(model1,train, type= "prob")
trnSc1<- lift_chart(predTrnProb,train)
AUC <- roc_curve(trnSc1)

prtn <- predict(model1,test,type="class")
perf_metrics(table(test$RESPONSE,prtn))

#C5.0
model2 <- C5.0(RESPONSE~.,data=train,winnow = TRUE,CF=0.2,parms=list(loss=costMatrix))
prtn <- predict(model2,train,type="class")
perf_metrics(table(train$RESPONSE,prtn))

predTrnProb <- predict(model2,train, type= "prob")
trnSc1<- lift_chart(predTrnProb,train)
AUC <- roc_curve(trnSc1)

prtn <- predict(model2,test,type="class")
perf_metrics(table(test$RESPONSE,prtn))
######################### Best Model #################################
nr <- nrow(cdata_n)
index <-  sample(1:nr, size = round(0.7*nr), replace=FALSE) 
train <- cdata_n[index,]
test <- cdata_n[-index,] 

ruleModel <- C5.0(RESPONSE ~ ., data = train,winnow=TRUE,CF=0.2,method= "class")
summary(ruleModel)

#Accuray
prtn <- predict(ruleModel,train,type="class")
perf_metrics(table(train$RESPONSE,prtn))
#LIFT CHART
predTrnProb <- predict(ruleModel, train, type="prob")
trnSc1<- lift_chart(predTrnProb,train)
AUC <- roc_curve(trnSc1)


prtn <- predict(ruleModel,test,type="class")
perf_metrics(table(test$RESPONSE,prtn))
######################### Cumulative Cost/benefit #####################
nr <- nrow(cdata_n)
index <-  sample(1:nr, size = round(0.7*nr), replace=FALSE) 
train <- cdata_n[index,]
test <- cdata_n[-index,] 

model4 <- rpart(RESPONSE~.,data=train,method="class",control=rpart.control(minsplit = 30,cp=0.015,maxdepth = 6,xval = 10))
pred_prob <- as.data.frame(predict(model4,test,type="prob"))
colnames(pred_prob) <- c("Bad","Good")
test$Good <- as.double(pred_prob$Good)

test['cost'] <- ifelse(test$RESPONSE==1,100,-500)
test <- test[order(-test$Good),]
test['cum_cost'] <- cumsum(test$cost)
test['labels'] <- round(test$Good,2)
rownames(test) <- NULL
plot(test$labels,test$cum_cost,type="l",main="Cumulative Cost/Benefit",xlab="Predicted Probabilty",ylab="Cumulative Cost",col="blue")
View(test[which(test$cum_cost==max(test$cum_cost)),])
