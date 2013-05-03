setwd("D:\\dropbox\\Dropbox\\手头工作\\毕业论文\\Data")
library(foreign)
aaa<-read.dta("111.dta")
attach(aaa)
nls(bollwormusepermu~a*taikang^b*fugou^c*liangshan^d)


nls(bollwormusepermu~A* (taikang^B) *(fugou^C) *(liangshan^D),start=list(A=3.1,B=0.41,C=0.321,D=0.4),trace=T)