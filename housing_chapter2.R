install.packages("caret")
library(caret)
install.packages("kernlab")
install.packages('e1071', dependencies=TRUE)
library(kernlab)
install.packages('e1071', dependencies=TRUE)
install.packages("caTools")
library(caTools)
library(e1071)
housing = read.csv("WestRoxbury.csv",header = TRUE)
dim(housing)
head(housing)
View(housing)

#variables
names(housing)

#LINEAR REGRESSION
linear=lm(TOTAL.VALUE~GROSS.AREA,data=housing)
summary(linear)
plot(housing$TOTAL.VALUE,housing$GROSS.AREA,col = "blue",main = "PRICE AND GROSS AREA REGRESSION",
     abline(lm(housing$TOTAL.VALUE~housing$GROSS.AREA)),cex = 1.3,pch = 16,xlab = "Price",ylab = "Size")


#MULTIPLE REGRESSION
multiple=lm(TAX~GROSS.AREA+LIVING.AREA+LOT.SQFT,data=housing)
summary(multiple)







#KNN
set.seed(1234)
#partitioning
intrain = createDataPartition(y=housing$REMODEL,p=0.75,list = F)
intrain
training = housing[intrain,]
#allotting data to training and testing
testing = housing[-intrain,]
dim(training)
dim(testing)
head(training)
#Step 3 - Model creating
modelfit1=train(REMODEL~.,data = training,method="knn")
modelfit1
#step 4 testing
predictions=predict(modelfit1,newdata=testing)
predictions
#to remove scientific notation from confusion matrix
options(scipen=999)
#confusion matrix
confusionMatrix(factor(predictions),factor(testing$REMODEL))


levels(predictions)
levels(housing$REMODEL)
levels(housing$REMODEL)=levels = c("None","Old","Recent")












housing5=housing
#linear model
intrain1 = createDataPartition(y=housing5$GROSS.AREA,p=0.75,list = F)
intrain1
training3 = housing5[intrain1,]
# #allotting data to training and testing
testing3 = housing5[-intrain1,]
modelfit=train(GROSS.AREA~.,data = training3,method="lm")
summary(modelfit)
#Predictions or Model validation
prediction=predict(modelfit,newdata = testing3)
prediction
table(prediction)
table(testing$LOT.SQFT)

#plotting the values
plot(testing3$GROSS.AREA,type="l",col="red")
lines(prediction,col="green")























#clustering
housing
housing1=housing[,-14]
housing1
housing2=housing1[,-13]
housing2

clu=kmeans(dist(housing2),3)
clu
clu$cluster

library(dplyr)
housing3=housing2%>%
  mutate(clusters=clu$cluster)
housing3

table(housing3$cluster)
plot(housing3$LOT.SQFT,housing3$clusters)














#hierarchical clustering
h=hclust(dist(housing2),method = "ward.D")
h
plot(h)
plot(h,cex=0.3)
cut2=cutree(h,3)
table(cut2)
table(housing$REMODEL,cut2)












#naive bayes algo
split=sample.split(housing,SplitRatio = 0.75)
train_naive=subset(housing,split=="TRUE")
test_naive=subset(housing,split=="FALSE")

set.seed(123)
classifier_naive = naiveBayes(REMODEL~.,data = train_naive)
classifier_naive

pred=predict(classifier_naive,newdata = test_naive)
summary(pred)
levels(test_naive$REMODEL)
confusionMatrix(factor(test_naive$REMODEL),factor(pred))
levels(pred)
levels(test_naive$REMODEL)=levels = c("None","Old","Recent")
levels(pred)=levels = c("None","Old","Recent")
