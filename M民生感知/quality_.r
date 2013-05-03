quality=read.table("H:/在研课题/民生感知/quality2.txt", header=TRUE)

#计算列平均值
colMeans(quality[,1:18])
#计算大于某值的样本数
sum(quality[,1]>=0)

########什么人想培训
mytest=table(quality[,2],quality[,1])
test=fisher.test(mytest,workspace=200000,alternative="two.sided",simulate.p.value = TRUE, B = 2000)
print(prop.table(mytest,R2)*100,digits=2)

############互联网熟悉程度
t3_19=table(quality[,3],quality[,19])
t3_19=cbind(t3_19,rowSums(t3_19))
print(prop.table(t3_19,2)*100,digits=2)





table(quality[,22],quality[,23])

t1_19=table(quality[,1],quality[,19])
t1_19
t1_19=cbind(t1_19,rowSums(t1_19))
t1_19
print(prop.table(t1_19,2)*100,digits=2)


######################################################################fisher检验与列联表begin
for(j in 19:24){

for (i in 1:18) {

mytest=table(quality[1:502,i],quality[1:502,j])
mytest
	if(i!=17){
	test=fisher.test(mytest,workspace=200000,alternative="two.sided",simulate.p.value = TRUE, B = 2000)

	print("=======================================begin") 
	
	print("问题:")
	print(i)
	print("与问题")
	print(j)
	print("的")
	print("费雪检验P值:")
	print(test$p.value)
	if (test$p.value<=0.05) {
	print(prop.table(mytest,2)*100,digits=2)
	}

	} #end if i!=17 loop
} #end if i loop
} #end of J loop


