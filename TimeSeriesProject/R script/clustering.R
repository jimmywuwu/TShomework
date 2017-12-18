####### 分析過程 ########

# 全域變數定義
# time_start (所有人都有的開頭)
# time_end (所有人都有的結尾)
# n (基金數目)
# asset (基金檔名)
# correct_time (交集所有基金都有的時間)

# 1.資料預處理：原資料序列 data_list、資料完整 series_na_fix(為了做Mkwtz分析、model-based method)
# 2.分群分析 
# 2.1 DTW distance method (shape-based method)
# 2.1.1 計算 distance 
# 2.1.2 Hierachichal method
# 2.2 cepstrum (model-based method)
# 3. 效率前緣比較
# 3.1 DTW - Hierachichal 
# 3.2 Cepstrum Method
# 4 結論

######## 套件入 #########

install.packages('dtwclust')
library(dtwclust) # Calculate DTW distance
library(cluster) # hierachichal clustering
library(stringr) # replace string
library(magrittr) # pipe line 
library(zoo) # fix na
setwd('~/Desktop/TimeSeriesProject/Asset/')


####### 全域變數 #######

# 計算每檔基金都有的時間
ts1=asset[1]%>%read.csv(header=F) %>% tail(1000)
correct_time=as.character(ts1[,2])
for(i in 1:n){
  file1=asset[i]%>%read.csv(header=F) %>% tail(1000)
  correct_time=base::intersect(correct_time,as.character(file1[,2]))
}

# 所有基金都有的開頭、結尾
time_start = as.Date("2014-10-08")
time_end   = as.Date("2017-10-16")

# n 為總共基金基金數
n=length(list.files())

# asset 為所有檔案的名稱
asset=list.files()

########### 1. data preprocessing #################

# data_list (將所有資料讀入、存放在該list變數)

data_list=list()
for(i in 1:n){
  file1=asset[i]%>%read.csv(header=F)
  file1[,2]=as.Date(file1[,2])
  file1=file1[which((file1[,2]>=time_start)*(file1[,2]<=time_end)==1),]
  data_list[[asset[i] %>% str_replace(".csv","")]]=file1
}

#  series_na_fix (為了讓資料符合model , Mkwitz framework 
#  將每筆資料的日期補到完整、並將missing vale補齊存入該data.frame變數)
#  Deal with missing value https://stackoverflow.com/questions/27368195/r-ts-with-missing-values

allDates <- seq.Date(
  as.Date("2014-10-08"),
  as.Date( "2017-10-16"),
  "day")

series_na_fix=list()
for(i in data_list){
  allValues <- merge(x=data.frame(V2=allDates),
                     y=i,
                     all.x=TRUE
  )
  allValues[,2]=NULL
  approx=na.approx(zoo(allValues$V3,allValues$V2))
  series_na_fix[[as.character(i[1,1])]]=as.vector(approx)
}
series_na_fix=as.data.frame(series_na_fix)
series_norm=series_na_fix
for(i in 1:n){
  series_norm[,i]=(series_na_fix[,i]-series_na_fix[1,i])/series_na_fix[,i]
}


# 計算每檔基金價格序列的平均日報酬、標準差（用於最後的Markowitz效率前緣分析）
mm= as.data.frame(series_na_fix[seq(1,1105,10),]) %>% apply(MARGIN = 2,FUN =log) %>% apply(MARGIN = 2,FUN =diff) 
ss=mm %>% apply(MARGIN = 2,FUN = sd)
m_=apply(mm,MARGIN = 2,FUN = mean)
cov_=cov(mm)

solve(cov_)

plot(ss,m_)


########### 2. clustering analysis #################

### 2.1 Shape-based method (DTW - Hierachichal)

#### 2.1.1 計算 DTW distance matrix ####

dist_matrix=matrix(0,n,n)
# 為了去除 size effect , 每檔基金的價格序列都對 time_start 時的價格normalize
for(i in 1:n){
  file1=asset[i]%>%read.csv(header=F)
  file1[,2]=as.Date(file1[,2])
  file1=file1[which((file1[,2]>time_start)*(file1[,2]<time_end)==1),]
  for(j  in (i+1):n){
    print(i)
    print(j)
    file2=asset[j]%>%read.csv(header=F) 
    file2[,2]=as.Date(file2[,2])
    file2=file2[which((file2[,2]>time_start)*(file2[,2]<time_end)==1),]
    dist_matrix[i,j]=dtw((file1[,3]-file1[1,3])/file1[1,3],(file2[,3]-file2[1,3])/file2[1,3],keep=TRUE)$distance
  }
}
colnames(dist_matrix)=list.files() %>% str_replace(".csv","")
row.names(dist_matrix)=list.files() %>% str_replace(".csv","")

#### 2.1.2 分群分析 ####
# 將 dist_matrix 轉變為dist物件（為了配合hclust function）
dist_matrix[lower.tri(dist_matrix)]=NA
dist_matrix=as.dist(t(dist_matrix))
h.M.cluster <- hclust(dist_matrix,method = "complete") 
rrresult3=cutree(h.M.cluster, k=4)

plot(ss,m_,col=rrresult3)
plot.ts(series_norm[,which(rrresult3==3)] %>% apply(MARGIN=1,FUN=mean),col="cyan")
ts.plot(series_norm[,which(rrresult3==4)],col=col_clust_light[3])
lines(series_norm[,which(rrresult3==4)] %>% apply(MARGIN=1,FUN=mean),col=col_clust_heavy[3],lwd=3)
legend("bottomright", legend=c("cluster 4 member", "cluster mean"),
       col=c(col_clust_light[3], col_clust_heavy[3]),lwd = c(1,3))
plot(h.M.cluster)

# plot sesries
col_clust_light=c(rgb(138/250,167/250,193/250),rgb(222/250,123/250,173/250),rgb(191/250,59/250,95/250),rgb(239/250,201/250,90/250))
col_clust_heavy=c(rgb(138/300,167/300,193/300),rgb(222/300,123/300,173/300),rgb(191/300,59/300,95/300),rgb(239/300,201/300,90/300))

png(filename="TS_clust_DTW_hier.png",width=1000, height=1000)
par(mfrow=c(2,2))
for(i in 1:4){
  ts.plot(series_norm[,which(rrresult3==i)],col=col_clust_light[i],ylab="Normalized return")
  lines(series_norm[,which(rrresult3==i)] %>% apply(MARGIN=1,FUN=mean),col=col_clust_heavy[i],,lwd=3)
  legend("bottomright", legend=c(paste("cluster",i,"member"), "cluster mean"),
         col=c(col_clust_light[i], col_clust_heavy[i]),lwd = c(1,3))
}
title(outer = TRUE,main ="DTW Method with HAC" ,line=-2.5,cex.main=1.5)
dev.off()

# plot mean v.s std
par(mfrow=c(1,1))
color_clust=c()
for(i in 1:n){
    color_clust=cbind(color_clust,col_clust_light[rrresult3[i]])
}
color_clust

png(filename="TS_clust_MvsS_DTW_HAC.png",width=800, height=800)
plot(ss,m_,col=color_clust,pch=19,cex=2,xlab="std",ylab="Return")
legend("topright",col=col_clust_light,pch=19,legend = c("cluster 1","cluster 2","cluster 3","cluster 4"))
title(main="Mean v.s. Std (DTW with HAC)")
dev.off()
# 代表點選取

#!





### 2.2 Model-based method (Cepstrum Method) ###

# function designed to calculate the cepstral coefficient
cepstral=function(n,p,Tseries){
  model_in=arima(Tseries,order = c(p,1,0))
  model_coef=model_in$coef
  cep=c()
  cep[1]=-model_coef[1]
  for(i in 2:p){
    cep[i]=-model_coef[i]
    for(j in 1:(i-1)){
      cep[i]=cep[i]-(1-j/i)*model_coef[j]*cep[i-j]
    }
  }
  if(n>p){
    for(i in (p+1):n){
      tmp=0
      for(j in 1:p){
        tmp=tmp-(1-j/i)*model_coef[j]*cep[i-j]
      }
      cep[i]=tmp
    }
  }
  cep
}


# 計算 每檔基金的Cepstrum 係數

ceps=data.frame()
for( i in 1:n){
  ceps=rbind(ceps,cepstral(15,10,series_na_fix[,i]))
}
row.names(ceps)=list.files() %>% str_replace(".csv","")

# 分群
km.res_2<- kmeans(scale(ceps),2, nstart = 25)
km.res_3<- kmeans(scale(ceps),3, nstart = 25)
km.res_4<- kmeans(scale(ceps),4, nstart = 25)

model_based_h=hclust(dist(ceps),method = "complete")
model_based_cut=cutree(model_based_h,4)

plot(ss,mm,col=km.res_2$cluster)
plot(ss,mm,col=km.res_3$cluster)
plot(ss,mm,col=km.res_4$cluster)
plot(ss,mm,col=model_based_cut)
plot.ts(series_na_fix[,which(km.res_4$cluster==2)],col=colors()[100])
plot.ts(series_na_fix[,which(km.res_4$cluster==2)[1:10]]%>%apply(MARGIN = 2,FUN =diff),col=colors()[100])
fviz_nbclust(scale(ceps), 
             FUNcluster = hcut,  # hierarchical clustering
             method = "silhouett",     # total within sum of square
             k.max = 12          # max number of clusters to consider
) + 
  
  labs(title="Elbow Method") +
  
  geom_vline(xintercept = 4,       # 在 X=4的地方 
             linetype = 2)         # 畫一條虛線
# 效率前緣比較
efficiency_front=function(cova,mu){
  M=cbind(mu,1)
  B=t(M)%*%solve(cova)%*%M
  m0_tu_1=c(0.001,1)
  m0_tu_2=c(0.004,1)
  w_1=solve(cova)%*%M%*%solve(B)%*%t(t(m0_tu_1))
  w_2=solve(cova)%*%M%*%solve(B)%*%t(t(m0_tu_2))
  
  mu_1=t(w_1)%*%mu
  mu_2=t(w_2)%*%mu
  sd_1=t(w_1)%*%cova%*%t(t(w_1))
  sd_2=t(w_2)%*%cova%*%t(t(w_2))
  
  x=c()
  y=c()
  for (alp in seq(-10,10,0.01)){
    w_=alp*w_1+(1-alp)*w_2
    x=c(x,t(w_)%*%mu)
    y=c(y,t(w_)%*%cova%*%t(t(w_)))
  }
  cbind(x,y)
}

eff=efficiency_front(cov_,m_)

plot.new()
plot.window(xlim=c(0,0.05), ylim=c(-0.02,0.02))
axis(1)
axis(2)
title(main="Std v.s Mean", 
      xlab="Standard Deviation", ylab="10 day Returns")
lines(sqrt(eff[,2]),eff[,1],lty=2)
legend('topright',c('Efficient Frontier(All)','Efficient Frontier(Mean)','Cluster 1','Cluster 2','Cluster 3','Cluster 4'),
       lty = c(2,9,NA,NA,NA,NA), pch = c(NA,NA,19,19,19,19),bg='white',ncol=1,col=c('black','black',col_clust_light))
points(ss,m_,col=color_clust,pch=20)

test=mm[,1:4]
for(i in 1:4){
  test[,i]=mm[,which(rrresult3==i)] %>% apply(MARGIN=1,FUN=mean)
}

points(test %>% apply(MARGIN=2,FUN=sd),test %>% apply(MARGIN=2,FUN=mean),col=col_clust_light,pch=2)
eff_clust=efficiency_front(cov(test),test %>% apply(MARGIN=2,FUN=mean))
lines(sqrt(eff_clust[,2]),eff_clust[,1],lty=9)
colfunc <- colorRampPalette(c(col_clust_light[1],col_clust_heavy[1] ))
for(i in 1:20){
  eff_clust=efficiency_front(cov(mm[,best_n(i)]),mm[,best_n(i)] %>% apply(MARGIN=2,FUN=mean))
  lines(sqrt(eff_clust[,2]),eff_clust[,1],lty=9,col=colfunc(30)[i],lwd=2)
}


best_n=function(n){
  clust1_dist=c()
  for(i in which(rrresult3==1)){
    clust1_dist=c(clust1_dist,dtw(mm[,i],test[,1])$distance)
  }
  clust2_dist=c()
  for(i in which(rrresult3==2)){
    clust2_dist=c(clust2_dist,dtw(mm[,i],test[,2])$distance)
  }
  clust3_dist=c()
  for(i in which(rrresult3==3)){
    clust3_dist=c(clust3_dist,dtw(mm[,i],test[,3])$distance)
  }
  clust4_dist=c()
  for(i in which(rrresult3==4)){
    clust4_dist=c(clust4_dist,dtw(mm[,i],test[,4])$distance)
  }
  ticker=c(
    names(rrresult3)[which(rrresult3==1)][which(order(clust1_dist)<=n)],
    names(rrresult3)[which(rrresult3==2)][which(order(clust2_dist)<=n)],
    names(rrresult3)[which(rrresult3==3)][which(order(clust3_dist)<=n)],
    names(rrresult3)[which(rrresult3==4)][which(order(clust4_dist)<=n)]
  )
  row_num=c()
  for(i in 1:length(rrresult3)){
    if(names(rrresult3)[i] %in% ticker){
      row_num=c(row_num,i)
    }
  }
  row_num
}



for(i in 1:4){
  eff_clust=efficiency_front(cov(mm[,which(rrresult3==i)]),m_[which(rrresult3==i)]) 
  lines(sqrt(eff_clust[,2]),eff_clust[,1],col=col_clust_heavy[i])
}



