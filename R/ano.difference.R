"ano.difference" <-
function(data,initial=NULL,final="last",C=NULL,U=NULL,normal=TRUE,B=100,rate=TRUE,time=NULL)
  {
    
### Test if matrix and data.frame
    if(!is.matrix(data) && !is.data.frame(data))
      stop("Not a valid data matrix or data frame type.")
    
### Test if enough time points
    l<-dim(data)
    if(length(time)!=(l[2]-2) && length(initial)!=0)
      stop("The number of time points should correspond to the number of measurements")

### remove the missing values
    data<-remove.NA(data)
    
### Create the design matrix
    X<-design.matrix(data[,1])
    d<-dim(data)
### Define the default position
    if(final=="last")
      final<-d[2]-2
    
    if(length(initial)==0)
      {
        Y<-as.double(data[,final+2])
      }
    else
      {
        Y<-as.double(data[,final+2])-as.double(data[,initial+2])
      }
    Y<-as.matrix(Y)

### Dimentional parameters
    n<-dim(Y)[1]
    p<-dim(X)[2]
    v<-n-p
    
### Default contrast matrices
    if(length(C)==0)
      C<-cbind(rep(1,p-1),-diag(p-1))
    if(length(U)==0)
      U<-as.matrix(1)
    
### Calculate some constants
    q<-qr(C)$rank

### Calculate the degree of freedom
    df1<-q
    df2<-v

### Compute the Lawley Hotteling trace statistics
    tr<-hotelling.trace(X,Y,C,U)
    
### Compute the exact F-test
  Ftest<-df2/df1*tr
  p<-1-pf(Ftest, df1, df2, ncp=0)

### Estimate of the mean (differences if initial=T)
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
### Compute the rate and the associated standard deviation
### The standard deviation are always computed by bootstrap
    if(length(time)!=0 && length(initial)!=0 && rate==TRUE)
      {
        rate.growth<-beta/(time[final]-time[initial])
        std.rate<-std/(time[final]-time[initial])^2
      }
    else
      {
        rate.growth<-NULL
        std.rate<-NULL
      }
    
  list(Ftest=Ftest,df1=df1,df2=df2,p=p,mean=beta,std.mean=std,rate=rate.growth,std.rate=std.rate)
    
  }
