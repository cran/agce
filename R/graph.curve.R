"graph.curve" <-
function(data,time,legend=TRUE)
{
### Test if matrix and data.frame
  if(!is.matrix(data) && !is.data.frame(data))
    stop("Not a valid data matrix or data frame type.")
  
  d<-dim(data)[2]-2
  factors<-data[,1]
  n<-length(factors)
  fac<-factor(factors)
  lev<-levels(fac)
  l<-length(lev)
  med<-matrix(0,l,d)

  ###Convert to numeric
  newdata<-as.matrix(data[,3:(d+2)])
  
  if(l<2)
    stop("Should have at least two groups")
### min and max for the range
  m<-min(newdata)
  M<-max(newdata)

### plot the first group
  y<-newdata[data[,1]==lev[1],]
  for(j in 1:d)
    {
      med[1,j]<-median(y[,j],na.rm=TRUE)
    }
  plot(time,med[1,],xlab="time",ylab="tumor volume",type="b",pch=1,lty=1,ylim=c(m,M))
  
  for(i in 2:l) #number of groups
    {
      y<-newdata[data[,1]==lev[i],]
      for(j in 1:d)
        {
          med[i,j]<-median(y[,j],na.rm=TRUE)
        }
      points(time,med[i,],pch=i,lty=i,type="b")
    }
  if(legend)
    legend(time[1],M,as.character(lev),lty=1:l,pch=1:l)
}
