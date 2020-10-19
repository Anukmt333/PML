install.packages("caret")
install.packages("randomForest")
install.packages("rpart")
install.packages("rpart.plot")
library(rpart.plot)
library(caret)
library(randomForest) 
library(rpart) 
#Here we set the seed for reproduceability
set.seed(1234)
#Load data
train <- read.csv("E:/RStudio/pml-training.csv", na.strings=c("NA","#DIV/0!", ""))
test <- read.csv("E:/RStudio/pml-testing.csv", na.strings=c("NA","#DIV/0!", ""))
dim(train)
dim(test)
#Eliminate missing values
train<-train[,colSums(is.na(train)) == 0]
test <-test[,colSums(is.na(test)) == 0]
train   <-train[,-c(1:7)]
test <-test[,-c(1:7)]
#Dimensions
dim(train)
dim(test)
head(train)
head(test)
ssample <- createDataPartition(y=train$classe, p=0.75, list=FALSE)
sTrain <- train[ssample, ] 
sTest <- train[-ssample, ]
dim(sTrain)
dim(sTest)
head(sTrain)
head(sTest)
plot(sTrain$classe, col="red", main="Plot of variable classe data", xlab="classe", ylab="Freq.")
m1 <- rpart(classe ~ ., data=sTrain, method="class")
p1 <- predict(m1, sTest, type = "class")
rpart.plot(m1, main="Classification Tree", extra=102, under=TRUE, faclen=0)
confusionMatrix(p1, sTest$classe)
m2 <- randomForest(classe ~. , data=sTrain, method="class")
p2 <- predict(m2, sTest, type = "class")
confusionMatrix(p2, sTest$classe)
pFinal <- predict(m2, testingset, type="class")
pFinal
