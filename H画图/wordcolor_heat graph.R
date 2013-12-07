x<-1:10;names(x)<-letters[1:10] #names是用来为x中变量赋变量名的
barplot(x,col=rev(heat.colors(10)))