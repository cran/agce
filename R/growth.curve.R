"growth.curve" <-
function(data,C=NULL,U=NULL,normal=TRUE,B=1000,time=NULL)
{
### Test if matrix and data.frame
  if(!is.matrix(data) && !is.data.frame(data))
    stop("Not a valid data matrix or data frame type.")
  
### Test if enough time points
  l<-dim(data)
  if(length(time)!=(l[2]-2))
    stop("The number of time points should correspond to the number of measurements")
  
### remove the missing values
  data<-remove.NA(data)
  
### Create the design matrix
  X<-design.matrix(data[,1])
  print(dim(X))
  ## P is a within individuals design matrix
  P<-rbind(1,time)
  H<-t(P)%*%solve((P%*%t(P)))
  
  Y<-as.matrix(data[,3:(l[2])])%*%H
### Dimentional parameters
  n<-dim(Y)[1]
  p<-dim(X)[2]
  v<-n-p
  
### Default contrast matrices
  if(length(C)==0)
    C<-cbind(rep(1,p-1),-diag(p-1))
  if(length(U)==0)
    U<-as.matrix(c(0,1))

### Compute the Lawley Hotteling trace statistics
  tr<-hotelling.trace(X,Y,C,U)
  
### Calculate the degrees of freedom
  df2<-v
  df1<-qr(C)$rank
  
### Compute the exact F-test
  Ftest<-df2/df1*tr
  
### Calculate the p-value
  p<-1-pf(Ftest, df1, df2, ncp=0)

### Estimate of the mean
  beta<-manova.estimate(Y,X)

### Compute the standard deviations of the estimates
  if(normal==TRUE) # Normal assumption
    {
      ##  covarianve between the rows
      sigma.hat<-(t(Y)%*%Y-t(Y)%*%X%*%beta)/v
      ## covariance between the columns
      tau<-solve(t(X)%*%X) 
      sigma.hat.diag<-diag(sigma.hat)
      tau.diag<-diag(tau)
      std<-sqrt(tau.diag%*%t(sigma.hat.diag))
    }
  else # Bootstrap
    {
      std<-resamp.std(data[,1],Y,X,B=B)
    }

  list(Ftest=Ftest,df1=df1,df2=df2,pvalue=p,mean=beta,std.mean=std)
}
