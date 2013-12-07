from cv import *  
import numpy as np  
  
class Cluster:  
    center = []  
    pre_center = []  
    pts = []  
    color = ()  
  
def is_good_result(cluster):  
    for c in cluster:  
        if np.linalg.norm(c.center - c.pre_center) > 1.0:  
            return False  
    return True  
  
def update_center(cluster):  
    for c in cluster:  
        c.pre_center = c.center  
        #print len(c.pts)  
        c.center = np.sum(c.pts,0) / len(c.pts)  
        s = 0  
        for p in c.pts:  
            s = s + (p[0] - c.center[0])**2 + (p[1] - c.center[1])**2  
        print s#输出方差，可以看到运行的时候不断减小  
        c.pts = []  
    return cluster  
  
def get_rand_pts(K,img_size,num):#这里根据K直接随机生成相应的椭圆区域，不再是矩形区域了  
    #pts = np.random.rand(num,2) * img_size  
    center = np.random.rand(K,2) * (np.array(img_size) - np.array([300,300])) + np.array([150,150])  
    r = np.random.rand(K,2) * (200,200) + (100,100)  
  
    pts = []  
    for i in xrange(num):  
        tmp = np.random.rand(2) * np.pi  
        tmp[0] = np.cos(tmp[0])  
        tmp[1] = np.sin(tmp[1])  
        pts.append(center[i % K] + r[i % K] * np.random.rand(2) * tmp)  
    return pts  
  
def show_outcome(img,cluster):  
##    K = len(cluster)#这里注释掉的内容是因为太耗时间，不知道怎么回事，现在还解决不了  
##    for y in xrange(img.height):  
##        for x in xrange(img.width):  
##            min_k = 0  
##            min_val = 100000  
##            for k in xrange(K):  
##                p = (x,y)  
##                #val = np.sqrt((p[0] - cluster[k].center[0])**2 + (p[1] - cluster[k].center[1])**2)  
##                val = np.linalg.norm(p - cluster[k].center)  
##                if val < min_val:  
##                    min_k = k  
##                    min_val = val  
##            img[y,x] = cluster[min_k].color  
    for c in cluster:  
        #print c.pts  
        Circle(img,(int(c.center[0]),int(c.center[1])),10,c.color,CV_FILLED)  
        for x, y in np.int32(c.pts):  
            Circle(img,(x,y),3,c.color,CV_FILLED)  
            #Line(img,(x + 5,y),(x - 5,y),c.color,2)  
            #Line(img,(x,y + 5),(x,y - 5),c.color,2)  
    NamedWindow("Image")  
    ShowImage("Image",img)  
    WaitKey(0)  
    DestroyWindow("Image")  
  
def main():  
    K = 4  
    PTS_NUM = 600  
    img = CreateImage((1200,800),IPL_DEPTH_8U,3)  
    pts = get_rand_pts(K,(img.width,img.height),PTS_NUM)  
      
      
    cluster = [Cluster() for i in xrange(K)]  
    init_k = np.arange(0,PTS_NUM - 1)  
    np.random.shuffle(init_k)  
    init_k = init_k[:K]  
      
    for i in xrange(K):  
        cluster[i].pre_center = [0,0]  
        cluster[i].center = pts[init_k[i]]  
        cluster[i].color = map(int,np.random.randint(0,1024,3) * 4 % 255)  
        cluster[i].pts = []  
        #print cluster[i].center  
    times = 0  
  
    while(True):  
        for p in pts:  
            min_k = 0  
            min_val = 100000  
            for j in xrange(K):  
                val = np.linalg.norm(p - cluster[j].center)  
                if val < min_val:  
                    min_k = j  
                    min_val = val  
            cluster[min_k].pts.append(p)  
        times = times + 1  
        if is_good_result(cluster):  
            break  
        print "Times: %d"%(times)  
        update_center(cluster)  
       
    show_outcome(img,cluster)  
  
if __name__ == "__main__":  
    main()  