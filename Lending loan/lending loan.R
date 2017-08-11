library(Amelia)
library(nnet)
library(lubridate)
library(class)
library(e1071)
library(mice)
library(car)
library(gmodels)
library(rpart)
library(caret)
library(e1071)
loan <- read.csv("F:/test/lendloan.csv")
class(loan)
#finding the missing values
missmap(loan,main="Missing values vs observed values")
sum(is.na(loan$pub_rec_bankruptcies))
str(loan)
loan$loan_status = as.factor(loan$loan_status)
loan$term= as.factor(loan$term)
loan$delinq_2yrs= as.factor(loan$delinq_2yrs)
loan$pub_rec= as.factor(loan$pub_rec)
loan$inq_last_6mths= as.factor(loan$inq_last_6mths)
loan$pub_rec_bankruptcies = as.factor(loan$pub_rec_bankruptcies)


sapply(loan_1,function(x) sum(is.na(x)))

testmiss1 <- subset.matrix(loan,is.na(loan$pub_rec_bankruptcies)) 
trainmiss1 <- subset.matrix(loan,!is.na(loan$pub_rec_bankruptcies))

model_miss1 <- multinom(pub_rec_bankruptcies ~ (.)-revol_util-total_acc-pub_rec
                        -open_acc-inq_last_6mths-delinq_2yrs,trainmiss1)
predmiss1 <- predict(model_miss1,testmiss1)        
testmiss1$pub_rec_bankruptcies <- predmiss1
which((is.na(testmiss1$pub_rec_bankruptcies)))
testmiss1[-which(is.na(testmiss1$pub_rec_bankruptcies)),]
loan_1 <- rbind(trainmiss1,testmiss1)

which((is.na(loan_1$pub_rec_bankruptcies)))
loan_1<- loan_1[-which(is.na(loan_1$annual_inc)),]

newdata <- na.omit(loan_1)
sapply(newdata,function(x) sum(is.na(x)))




plot(newdata$annual_inc)
plot(newdata$open_acc)
hist(log(newdata$revol_bal))




hist(newdata$last_pymnt_amnt)
plot(newdata$last_pymnt_amnt)
summary(newdata)
#Dealing with outliers
newdata_sub1 <- subset(newdata,annual_inc<150000 & open_acc <40 & recoveries <6000 
                       & collection_recovery_fee <6000)
newdata1 = newdata_sub1
plot(newdata$loan_status)

#imbalanced classification
install.packages("ROSE")
library(ROSE)
table(newdata$loan_status)
dim(newdata)
rows=1:40337
sam =sample(rows,28235)
trainset1 <- newdata_sub1[sam,]
testset1<- newdata_sub1[-sam,]
model_lr<- glm(loan_status ~ (.)-delinq_2yrs-inq_last_6mths,family = "binomial", data = trainset1)
summary(model_lr)
model_lr$xlevels[["delinq_2yrs"]] <- union(model_lr$xlevels[["delinq_2yrs"]],levels(testset1$delinq_2yrs))
model_lr$xlevels[["inq_last_6mths"]] <- union(model_lr$xlevels[["inq_last_6mths"]],levels(testset1$inq_last_6mths))
  
pred1 <- predict(model_lr,newdata = testset1,"response")
table(testset1$loan_status)

confusionMatrix(pred1)
accuracy.meas(testset1$loan_status,pred1)
roc.curve(testset1$loan_status,pred1,plotit = F)
summary(model_lr)
# part 1 oversampling

newdata_bal_over <- ovun.sample(loan_status ~.,data = trainset1,method = "over",N = 80674)$data
table(newdata_bal_over$loan_status)

newdata_bal_under <- ovun.sample(loan_status~.,data  = trainset1,method = "under",N=7000,seed= 1)$data
table(newdata_bal_under$loan_status)

newdata_bal_both <- ovun.sample(loan_status~.,data = trainset1,method = "both",p=0.5,N=7000,seed=1)$data
table(newdata_bal_both$loan_status)

newdata_rose <- ROSE(loan_status~.,data = trainset1,seed=1)$data
table(newdata_rose$loan_status)

# accuracy
str(lr_model_rose)
lr_model_rose <- glm(loan_status~(.)-delinq_2yrs-inq_last_6mths,family = "binomial",data = newdata_rose)
table(newdata_rose$loan_status)
lr_model_over <- glm(loan_status~(.)-delinq_2yrs-inq_last_6mths,family = "binomial",data = newdata_bal_over)
lr_model_under <- glm(loan_status~(.)-delinq_2yrs-inq_last_6mths-pub_rec,family = "binomial",data = newdata_bal_under)
lr_model_both <- glm(loan_status~(.)-delinq_2yrs-inq_last_6mths-pub_rec,family = "binomial",data = newdata_bal_both )
# for error Error in model.frame.default(Terms, newdata, na.action = na.action, xlev = object$xlevels) : 
#factor delinq_2yrs has new levels 8, 9
lr_model_rose$xlevels[["delinq_2yrs"]] <- union(lr_model_rose$xlevels[["delinq_2yrs"]], levels(testset1$delinq_2yrs))
lr_model_rose$xlevels[["inq_last_6mths"]] <- union(lr_model_rose$xlevels[["inq_last_6mths"]],levels(testset1$inq_last_6mths))

lr_model_over$xlevels[["inq_last_6mths"]] <- union(lr_model_over$xlevels[["inq_last_6mths"]],levels(testset1$inq_last_6mths))
lr_model_over$xlevels[["delinq_2yrs"]] <- union(lr_model_over$xlevels[["delinq_2yrs"]],levels(testset1$delinq_2yrs))

lr_model_under$xlevels[["delinq_2yrs"]] <- union(lr_model_under$xlevels[["delinq_2yrs"]],levels(testset1$delinq_2yrs))
lr_model_under$xlevels[["inq_last_6mths"]] <- union(lr_model_under$xlevels[["inq_last_6mths"]],levels(testset1$inq_last_6mths))
lr_model_under$xlevels[["pub_rec"]] <- union(lr_model_under$xlevels[["pub_rec"]],levels(testset1$pub_rec))

lr_model_both$xlevels[["delinq_2yrs"]] <-union(lr_model_both$xlevels[["delinq_2yrs"]],levels(testset1$delinq_2yrs))
lr_model_both$xlevels[["inq_last_6mths"]] <- union(lr_model_both$xlevels[["inq_last_6mths"]],levels(testset1$inq_last_6mths))
lr_model_both$xlevels[["pub_rec"]] <- union(lr_model_both$xlevels[["pub_rec"]],levels(testset1$pub_rec))

pred_rose <- predict(lr_model_rose,newdata=testset1,"response")
pred_over <- predict(lr_model_over,newdata=testset1,"response")
pred_under <- predict(lr_model_under,newdata=testset1,"response")
pred_both <- predict(lr_model_both, newdata=testset1,"response")

#AUC ROSE
roc.curve(testset1$loan_status,pred_rose)

#AUC Oversampling
roc.curve(testset1$loan_status, pred_over)


#AUC Undersampling
roc.curve(testset1$loan_status, pred_under)


#AUC Both
roc.curve(testset1$loan_status, pred_both)

cutoff =seq(0.05,0.99,.01)
sens <- numeric(length(cutoff))
spec <- numeric(length(cutoff))
set.seed(500)
for(i in 1:length(cutoff))
{
  newpred <- ifelse(pred_rose>= cutoff[i],1,0)
  n <- cbind(testset1$loan_status,newpred) 
  n[,1]= as.character(n[,1])
  n[,2]= as.character(n[,2])
  te = table(n[,1],n[,2])
  sens[i] <- te[2,2]/(te[2,2]+te[2,1])
  spec[i] <- te[1,1]/(te[1,1]+te[1,2])
  i = i+ 1 
}
cbind(sens,spec)
