remove.NA<-function(data) 
  {
    n1<-dim(data)[1]
    n2<-dim(data)[2]
### Keep the measurements
    newdata<-as.matrix(data[,3:n2])
    n2<-n2-2
    index<-rep(NA,n1)
 
    for(i in 1:n1)
      {
        if(sum(is.finite(newdata[i,]))==n2)
          index[i]<-i
      }
    index<-index[is.finite(index)==TRUE]
    data[index,]
  }
