require(ggplot2)
require(flexclust)

test=read.table('Q4Test.txt',header=T)
test=as.matrix(test)

KM<-function(X,K,m){
  n=dim(X)[1]
  X=as.data.frame(X)
  X$clust=sample(1:K,n,replace=TRUE)
  X$clust_new=rep(NA,n)
  stop=0
  while(stop==0){
    centroid_mean=aggregate(X[,-which(names(X)%in%c('clust','clust_new'))],
                            by=list(X$clust),FUN=mean)
    distances=dist2(X[,-which(names(X)%in%c('clust','clust_new'))],
                    centroid_mean[,-which(names(centroid_mean)%in%c('Group.1'))],
                    method='minkowski',p=m)
    colnames(distances)=c('1','2','3')
    X$clust_new=colnames(distances)[apply(distances,1,which.min)]
    if(identical(X$clust_new,X$clust)==TRUE){stop=1}
    X$clust=X$clust_new
  }
  Cluster_Size=c(dim(X[X$clust_new=='1',])[1],
                 dim(X[X$clust_new=='2',])[1],
                 dim(X[X$clust_new=='3',])[1])
  plt={ggplot()+
      geom_point(data=as.data.frame(X),aes(x=V1,y=V2,col=clust_new))+
      geom_point(data=as.data.frame(centroid_mean),aes(V1,V2),col='black',size=5)+
      theme(legend.position='none')+
      labs(title='Clustering By Custom Function')}
  list(Cluster_Assignments=X$clust_new,
       Cluster_Means=centroid_mean,
       Cluster_Size=Cluster_Size,
       Plot=plt)
}
km=KM(test,3,2)

#builtin R function
kmR=kmeans(test,3)
test_kmR_ggplot=cbind(test,assign=kmR$cluster)
km_builtin_plt={ggplot()+
    geom_point(data=as.data.frame(test_kmR_ggplot),aes(x=V1,V2,col=assign))+
    geom_point(data=as.data.frame(kmR$centers),aes(x=V1,V2),col='black',size=5)+
    theme(legend.position='none')+
    labs(title='Clustering By Built-In Function')}

#comparing results
grid.arrange(km$Plot,km_builtin_plt,ncol=2)