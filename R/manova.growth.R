"manova.growth" <-
function(data,C=NULL,U=NULL,normal=TRUE,B=1000)
{
### Test if matrix and data.frame
  if(!is.matrix(data) && !is.data.frame(data))
    stop("Not a valid data matrix or data frame type.")

### remove the missing values
  data<-remove.NA(data)
  
  l<-dim(data)
  Y<-as.matrix(data[,3:(l[2])]) 
### Create the design matrix
  X<-design.matrix(data[,1])
### Dimentional parameters
  n<-dim(Y)[1]
  p<-dim(X)[2]
  v<-n-p
  
### Default contrast matrices
  if(length(C)==0)
    C<-cbind(rep(1,p-1),-diag(p-1))
  if(length(U)==0)
    U<-rbind(rep(1,p),-diag(p))
  
### Compute the Lawley Hotteling trace statistics
  tr<-hotelling.trace(X,Y,C,U)
 
### Calculate some constants
  ## Dimension of W
  u<-qr(C)$rank
  ## Number of measurement
  r<-dim(Y)[2]
  s<-min(u,r)
  w<-.5*(abs(r-u)-1)
  x<-.5*(v-r-1)
  
### Calculate the degrees of freedom
  df2<-2*(s*x+1)
  df1<-(s*(2*w+s+1))
  
### Compute the F-test
  Ftest<-df2/df1*tr/s
  
### Calculate the p-value
  p<-1-pf(Ftest, df1, df2, ncp=0)
  
### Estimate of the mean
  beta<-manova.estimate(Y,X)
  
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
    std<-resamp.std(data[,1],Y,X,B=B)
  
  list(Ftest=Ftest,df1=df1,df2=df2,pvalue=p,mean=beta,std.mean=std)
}
