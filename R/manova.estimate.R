"manova.estimate" <-
function(Y,X)
  {
    solve(t(X)%*%X)%*%t(X)%*%Y
  }
