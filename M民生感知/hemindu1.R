library(ggplot2)
setwd("C:\\Users\\nixujun\\Desktop")
aa<-read.csv("1.csv")
qplot(score, data = aa, geom = "density",colour=element)