//首先是主体：
[cpp] view plaincopyprint?
int iter_times = 0;//迭代次数  
    while(!good_result())//检查是否是需要的聚类中心  
    {  
        for(int i = 0;i < POINT_NUM;i++)  
        {  
            double min = 10000;  
            int min_k = 0;  
            for(int j = 0;j < K;j++)  
            {  
                double tmp = DIS(c[j].center,s[i].p);   
                if(tmp < min)  
                {  
                    min = tmp;  
                    min_k = j;   
                }  
            }  
            s[i].cluster = min_k;  
            c[min_k].count++;  
        }  
        update_center();//更新聚类中心  
        iter_times++;  
        show_outcome();  
    }  

//然后是其他函数的实现：
[cpp] view plaincopyprint?
void update_center()  
{  
    double x[K],y[K];  
    memset(x,0,sizeof(x));  
    memset(y,0,sizeof(y));  
    for(int i = 0;i < POINT_NUM;i++)  
    {  
        x[s[i].cluster] += s[i].p.x;  
        y[s[i].cluster] += s[i].p.y;  
    }  
    for(int i = 0;i < K;i++)  
    {  
        c[i].pre_center = c[i].center;  
        c[i].center.x = x[i] / c[i].count;  
        c[i].center.y = y[i] / c[i].count;  
        c[i].count = 0;  
    }  
}  
//判断是否是需要的：
[cpp] view plaincopyprint?
bool good_result()  
{  
    for(int i = 0;i < K;i++)  
    {  
        if(DIS(c[i].center,c[i].pre_center) > TH)  
            return false;  
    }  
    return true;  
}  
//显示结果：
[cpp] view plaincopyprint?
void show_outcome()  
{  
    for(int y = 0;y < HEIGHT;y++)//这里将平面中所有的点都标记，就可以看到平面是怎样被划分的了  
        for(int x = 0;x < WIDTH;x++)  
        {  
            double min = 1000;  
            int min_k = 0;  
            CvPoint p = cvPoint(x,y);  
            for(int i = 0;i < K;i++)  
            {  
                double tmp = DIS(c[i].center,p);   
                if(tmp < min)  
                {  
                    min = tmp;  
                    min_k = i;   
                }  
            }  
            IMG_B(img,x,y) = color[min_k].val[0];  
            IMG_G(img,x,y) = color[min_k].val[1];  
            IMG_R(img,x,y) = color[min_k].val[2];  
            IMG_A(img,x,y) = 200;//4通道图像，就是说可以是透明的，纯试验而已，哪知道直接显示没效果，要保存之后才能看出来。  
        }  
    CvScalar scalar = cvScalar(255,255,255,255);  
    for(int i = 0;i < POINT_NUM;i++)//画每个样本点  
    {  
        int x = s[i].p.x;  
        int y = s[i].p.y;  
        cvLine(img,cvPoint(x - 5,y),cvPoint(x + 5,y),scalar,2);  
        cvLine(img,cvPoint(x,y - 5),cvPoint(x,y + 5),scalar,2);  
    }  
    for(int i = 0;i < K;i++)//画聚类中心  
    {  
        int x = c[i].center.x;  
        int y = c[i].center.y;  
        cvCircle(img,cvPoint(x,y),20,scalar,2);  
    }  
    cvShowImage("Image",img);  
    cvWaitKey(100);//100毫秒是个差不多的数值，可以完整的看到聚类过程  
}  