# DTW implement

vec1=c(71, 73, 75, 80, 80, 80, 78, 76, 75, 73, 71, 71, 71, 73, 75, 76, 76, 68, 76, 76, 75, 73, 71, 70, 70, 69, 68, 68, 72, 74, 78, 79, 80, 80, 78)
vec2=c(69, 69, 73, 75, 79, 80, 79, 78, 76, 73, 72, 71, 70, 70, 69, 69, 69, 71, 73, 75, 76, 76, 76, 76, 76, 75, 73, 71, 70, 70, 71, 73, 75, 80, 80, 80, 78)


DTW_dist=function(v1,v2){
  n=length(v1)
  m=length(v2)
  DTW=matrix(0,n,m)
  
  for(i in 1:n){
    DTW[i,1]=Inf
  }
  for(i in 1:m){
    DTW[1,i]=Inf
  }
  
  DTW[1,1]=abs(v1[1]-v2[1])
  res=dtw(vec1,vec2)
  
  
  for(i in 2:n){
    for(j in 2:m){
      DTW[i,j]=abs(v1[i]-v2[j])+min(DTW[i,j-1],DTW[i-1,j-1],DTW[i-1,j])
    }
  }
  
  
  DTW
  
}

res=DTW_dist(vec1,vec2)

Fibo=function(n){
  i=n
  while(i>0){
    if(i==1){
      ans=1
      break
    }else if(i==2){
      ans=1
      break
    }  
    else{
      ans=Fibo(i-1)+Fibo(i-2)
      break
    }
    i=i-1
  }
  ans
}
