# GRIP_TSF_TASKS

#AUTHOR: KIRTHIK.D
#TASK_6
#Prediction using Decision Tree Algorithm

library(rpart)


library(rpart.plot)
library(e1071)
head(iris)
iris
decision_tree_model<-rpart(Species ~ . ,data = iris, method = "class")
decision_tree_model
rpart.plot(decision_tree_model)

iris $ Species_Predicted<- predict(decision_tree_model,newdata=iris,type="class")

table(iris $ Species,iris$Species_Predicted)

library(caTools)
set.seed(123)
split <- sample.split(iris,SplitRatio=0.7)
split
train <- subset(iris,split=="TRUE")
test<- subset(iris,split=="FALSE")
train
test

decision_tree_model<- rpart(Species ~ . ,data = train, method = "class")
summary(decision_tree_model)

plot(decision_tree_model,uniform=TRUE , branch=0.6 ,margin=0.1)
text(decision_tree_model,all=TRUE , use.n=TRUE)

rpart.plot(decision_tree_model)

test$Species_Predicted<- predict(decision_tree_model,newdata= test,type="class")
table(test$Species,test$Species_Predicted)

library(caret)
confusionMatrix(table(test$Species,test$Species_Predicted))

printcp(decision_tree_model)
?printcp

plotcp(decision_tree_model)
?plotcp

min(decision_tree_model$cptable[,"xerror"])
which.min(decision_tree_model$cptable[,"xerror"])
cpmin <- decision_tree_model$ cptable[3,"CP"]

decision_tree_pruned= prune(decision_tree_model,cp=cpmin)
rpart.plot(decision_tree_model)

test$Species_Predicted<- predict(decision_tree_pruned,newdata=test,type="class")
table(test$Species,test$Species_Predicted)
confusionMatrix(table(test$Species,test$Species_Predicted))

