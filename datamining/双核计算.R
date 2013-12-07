system.time({
  library(MASS)
  result <- kmeans(Boston,4,nstart=100000)
})
#可看到单核心计算共耗费了61秒，再用本地计算机上用双核心构建集群进行比较。在安装和加载snow包后，首先用makeCluster函数构建本地计算集群，通讯方式使用socket，集群命名为cl。然后将运算所需的Boston数据分发到集群上。之后使用clusterApply函数将计算任务分配到两个核心上，其中第一个参数是集群名称，第二个参数表示每个核心分别计算5万次，第三个参数是一个匿名函数，规定了核心的计算任务。两个核心的计算结果分别存在results的list元素中。比较两个核心的结果取较优的一个，最后关闭集群。

install.packages('snow')
library(snow)
system.time({
  data(Boston)
  cl <- makeCluster(2,type='SOCK')
  clusterExport(cl,'Boston')
  results <- clusterApply(cl,rep(50000,2),function(nstart) kmean(Boston,4,nstart=nstart))
  i <- sapply(results,function(result) result$tot.withinss)
  result <- results[[which.min(i)]]
  stopCluster(cl)
})