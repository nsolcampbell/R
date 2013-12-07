#?scan

#例如，scan可以用来在R里直接输入数据，或者从working direction里读取数据
setwd("C:\\Users\\nixujun\\Desktop")
nums<-scan('numbers.txt')
matrix(scan('numbers.txt'),nrow=4,byrow=T)

numsurl<-scan('http://www.math.ccu.edu.tw/~yshih/data/irisdat.txt')