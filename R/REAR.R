require("nlme")

REAR<-function(data,time)
{
### Test if matrix and data.frame
  if(!is.matrix(data) && !is.data.frame(data))
    stop("Not a valid data matrix or data frame type.")
### remove the missing values
  data<-remove.NA(data)
### Reshape the data  
  long.data<-reshape.long(data,time)
### Convert as factor
  long.data$subject<- factor(long.data$subject)
  long.data$group<- factor(long.data$group)
  
### Fit the full model (different slopes)
  modF <- lme(y ~time:group,data=long.data, random = ~1|subject,correlation = corCAR1(form = ~ time|subject),method="ML")
  
### Fit the reduced (same slopes)
  modR <- lme(y ~time,data=long.data, random = ~1|subject,correlation =corCAR1(form = ~ time|subject),method="ML")
  a<-anova(modF,modR)
  
  list(Chi=a$L[2],p=a$p[2],df=a$df[1]-a$df[2],sum.fit.full=modF,sum.fit.reduced=modR)
}            
