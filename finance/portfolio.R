#R语言与马克维茨资产组合理论学习笔记（利用fportfolio包实现）
library(fPortfolio)

#模型设定
mvspec<-portfolioSpec()
setRiskFreeRate(mvspec)<-0
setSolver(mvspec)<-"solveRshortExact"
print(mvspec)
data<-100*LPP2005
Data<-portfolioData(100*LPP2005.RET,mvspec)#100*LPP2005.RET一些股票的收益率
print(Data)
constrains<-"Short"
portfolioConstraints(data,mvspec,constrains)


#方差最小组合求解
globminportfolio<-minvariancePortfolio(Data,mvspec,constrains)
print(globminportfolio)

#求解特定组合的均值方差
m1vspec<-portfolioSpec()
data1<-100*LPP2005.RET
Data1<-portfolioData(100*LPP2005.RET,m1vspec)
n<-ncol(data1)
setWeights(m1vspec)<-rep(1/n,n)
m1vPortfolio<-feasiblePortfolio(Data1,m1vspec,constraints="LongOnly")
print(m1vPortfolio)

#在上面同等收益下，优化组合
mvspec1<-portfolioSpec()
setRiskFreeRate(mvspec1)<-0.05
targetReturn<-getTargetReturn(m1vPortfolio@portfolio)["mean"]
setTargetReturn(mvspec1)<-targetReturn
efficientportfolio<-efficientPortfolio(Data,spec=mvspec1)
weightsPie(efficientportfolio)
efficientportfolio

#做出有效前沿
data2<-100*LPP2005.RET
lppspec<-portfolioSpec()
setRiskFreeRate(lppspec)<-0.005
frontier<-portfolioFrontier(data2,lppspec)
#plot(frontier)
tailoredFrontierPlot(frontier)
frontierPlot(frontier)
cmlPoints(frontier,col=2)
frontier
weightsPlot(frontier)
