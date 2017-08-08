library(rpart)
library(Amelia)
library(pROC)
install.packages("rpart.plot")
library(rpart.plot)
trainset <- read.csv("F:/test/titanic/train.csv")
testset <- read.csv("F:/test/titanic/test.csv")
testset$Survived <- 0
missmap(trainset)
##CLEANING DATA##
combine <- rbind(trainset,testset)
combine$Name <- as.character(combine$Name)
strsplit(combine$Name,split = '[,.]')
strsplit(combine$Name,split='[,.]')[[1]]
strsplit(combine$Name,split = '[,.]')[[1]][[2]]
combine$Title <-sapply(combine$Name,FUN = function(x){strsplit(x,split = '[,.]')[[1]][[2]]})
combine$Title
combine$Title <- sub('','',combine$Title)
combine$Title[combine$PassengerId==797] <-'Mrs'
unique(combine$Title)
table(combine$Title)
a=combine$Title[combine$Title %in% c('lady','the Countess','Mlle','Mme','Ms')] <- 'Miss'
b=combine$Title[combine$Title %in% c('Capt', 'Don', 'Major', 'Sir', 'Col', 'Jonkheer', 'Rev', 'Dr', 'Master')] <- 'Mr'
c=combine$Title[combine$Title %in% c('Dona')]<-'Mrs'
combine$Title <- as.factor(combine$Title)
str(combine$Title)
combine$Family <- combine$SibSp+combine$Parch +1
predcit_age <- rpart(Age ~ Pclass+Sex+SibSp+Parch+Fare+
             Embarked+Title+Family,combine[!is.na(combine$Age),],method="anova")
combine$Age[is.na(combine$Age)]<- predict(predcit_age,combine[is.na(combine$Age),])
combine$Age <- round((combine$Age),0)

dim(combine)
rows=1309
set.seed(100)
sam = sample(rows,1000)
train <- combine[sam,]
test <- combine[-sam,]
train$Cabin
train$Cabin <- substr(train$Cabin,1,1)
table(train$Cabin)
train$Cabin[train$Cabin== ""] <-"H"
train$Cabin[train$Cabin == "T"] <- "H"
test$Cabin
test$Cabin <-substr(test$Cabin,1,1)
table(test$Cabin)
test$Cabin[test$Cabin == ""] <- "H"

train$Cabin <- as.factor(train$Cabin)
test$Cabin <- as.factor(test$Cabin)

my_tree <- rpart(Survived ~ Age + Sex + Pclass+Family, data = train, method = "class", control=rpart.control(cp=0.0001))
summary(my_tree)
prp(my_tree, type = 4, extra = 100)

my_prediction <- predict(my_tree, test, type = "class")
head(my_prediction)
vector_passengerid <- test$PassengerId
my_solution <- data.frame(PassengerId = vector_passengerid, Survived = my_prediction)
head(my_solution,100,200)
