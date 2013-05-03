fzero=function (f, a, b, eps=1e-5){
    if (f(a)*f(b)>0)
	    list (fail="finding root is fail!")
	else{
	   repeat {
	      if (abs(b-a)<eps) break
		  x<-(a+b)/2
		  if (f(a)*f(x)<0) b<-x else a<-x
		  }
		}
	}