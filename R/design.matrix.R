"design.matrix" <-
function(factors)
{
### Keep the initial order
  n<-length(factors)
  fac<-factor(factors)
  lev<-levels(fac)
  l<-length(lev)
  if(l<2)
    stop("Should have at least two groups")
  X<-matrix(0,n,l)
  for(i in 1:l)
    X[factors==lev[i],i]<-1
  X
}
