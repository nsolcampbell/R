
def euclidean(v1,v2): #计算空间距离
       d=0.0
       for i in range(len(v1)): #计算v1有几维
         d+=(v1[i]-v2[i])**2
return math.sqrt(d)

def getdistances(data,vec1): #计算所有的点间的距离
       distancelist=[]
       for i in range(len(data)):
         vec2=data[i]['input']
         distancelist.append((euclidean(vec1,vec2),i))
         #将距离值添加到List，eg. [（v1，v2,3.00),(v2,v3,2.13) ...]
       distancelist.sort() #对结果进行排序
return distancelist

#KNN分类

def knnestimate(data,vec1,k=3):  #计算vec1结点的knn均值
      # 获取排序后的距离值
      dlist=getdistances(data,vec1)
      avg=0.0
      # 计算top K个元素的平均值
      for i in range(k):
         idx=dlist[i][1]
         avg+=data[idx]['result']
      avg=avg/k
return avg
	  
#一个例子

#先输入数据


电影名称	 打斗次数	 接吻次数	 电影类型
California Man  3	 104	 Romance
He’s Not Really into Dudes  2	 100	 Romance
Beautiful Woman  1	 81	 Romance
Kevin Longblade  101	 10	 Action
Robo Slayer 3000  99	 5	 Action
Amped II  98	 2	 Action
未知	 18	 90	 Unknown


import numpy as np
from sklearn import neighbors
knn = neighbors.KNeighborsClassifier() #取得knn分类器
data = np.array([[3,104],[2,100],[1,81],[101,10],[99,5],[98,2]]) # data对应着打斗次数和接吻次数
labels = np.array([1,1,1,2,2,2]) #labels则是对应Romance和Action
knn.fit(data,labels) #导入数据进行训练
knn.predict([18,90])	


