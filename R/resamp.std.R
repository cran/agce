"resamp.std" <-
  function(factors,Y,X,B=1000)
{
  n1<-dim(Y)[1]
  fac<-factor(factors)
  lev<-levels(fac)
  l<-length(lev)
### If the estimate is a matrix, convert to a vector
  estimate<-manova.estimate(Y,X)
  l1<-dim(as.matrix(estimate))[1]
  l2<-dim(as.matrix(estimate))[2]
  estimate<-matrix(0,l1*l2,B)
  std<-rep(0,l1*l2)
  
  for(i in 1:B)
    {
      ##Bootstrap a new dataset
      index<-NULL
      for(j in 1:l)
        {
          ind<-sample((1:n1)[fac==lev[j]],replace=TRUE)
          index<-c(index,ind)
        }
      ## Compute the new estimates
      estimate[,i]<-as.double(manova.estimate(Y[index,],X))
    }
### Reshape the standard deviation
  for(j in 1:(l1*l2))
    std[j]<-sd(estimate[j,])
  std<-matrix(std,l1,l2)
  std
}
