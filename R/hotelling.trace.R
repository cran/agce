"hotelling.trace" <-
function(X,Y,C,U)
  {
    n<-dim(Y)[1]
### Compute the projection matrix on V|W 
    PVW<-X%*%solve(t(X)%*%X)%*%t(C)%*%solve(C%*%solve(t(X)%*%X)%*%t(C))%*%C%*%solve(t(X)%*%X)%*%t(X)
    S2<-t(Y%*%U)%*%PVW%*%(Y%*%U)
    S3<-t(Y%*%U)%*%(diag(n)-X%*%solve(t(X)%*%X)%*%t(X))%*%(Y%*%U)
    
### Compute the H-L trace statistics
    tr<-sum(diag(S2%*%solve(S3,tol=1e-50)))
  }
