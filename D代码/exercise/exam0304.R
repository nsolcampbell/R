plot((ecdf(w),verticals=TRUE, do.p=FALSE)
x=44:78
lines(x,pnorm(x,mean(w),sd(w)))