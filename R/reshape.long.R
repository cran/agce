"reshape.long" <-
function(data,time)
  {
    d<-dim(data)
    long.data<-NULL
    for(i in 1:d[1])
      long.data<-rbind(long.data,cbind(data[i,1],data[i,2],data[i,3:d[2]],time))
    long.data<-data.frame(long.data,check.rows=T)
    names(long.data)<-c("group","subject","y","time")
    long.data
  }
