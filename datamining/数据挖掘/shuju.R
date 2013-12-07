zmr<-read.csv("Housing.csv",header=T)
zmr
library(class)
library(e1071)
attach(zmr)


m1<-naiveBayes(crim~.,data=zmr)
m2<-svm(crim~.,data=zmr)
