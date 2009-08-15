paramPersist <-
function(x,ttr="macd4",start=0,nSteps=0,stepSize=0,burn=0,short=FALSE,silent=TRUE,TC=0.001,loud=TRUE,alpha=0.25,periods=2,file="")

## 
## Computes the conditional returns
## During non-overlapping subperiods
##
## Hypothesis Testing:
##
## H0 = for fixed parameters, returns between subperiods are uncorrelated
##
## HA = returns are correlated, meaning returns are persistent
##

{
if(periods==1) { paramStats(x=x,ttr=ttr,start=start,nSteps=nSteps,stepSize=stepSize,burn=burn,short=short,silent=silent,TC=TC,loud=loud,alpha=alpha,begin=1,percent=1,file=file) }
else
{
if(loud) cat("Preparing to Analyze",periods,"Subperiods\n")
percent <- 1/periods
if(loud) cat("Subperiod 1\n")
data <- paramStats(x=x,ttr=ttr,start=start,nSteps=nSteps,stepSize=stepSize,burn=burn,short=short,silent=silent,TC=TC,loud=loud,alpha=alpha,begin=1,percent=(1/periods),file=file)
out <- list(data)

for(i in 2:periods)
	{ begin <- 1+floor( (i-1)*percent*length(x) )
	if(loud) cat("Subperiod",i,"\n")
	data <- paramStats(x=x,ttr=ttr,start=start,nSteps=nSteps,stepSize=stepSize,burn=burn,short=short,silent=silent,TC=TC,loud=loud,alpha=alpha,begin=begin,percent=(1/periods),file=file )
	out[[i]] <- data
	}

nLags <- min(5,periods-1)
if(loud) cat("Computing Correlation Coefficients\n")
for(j in 1:nLags) { 
	ccfs <- 0
	ccfp <- 0
	for(k in 1:(periods-j)) {
		dum <- cor.test(out[[k]][[2]],out[[k+j]][[2]]) 
		ccfs[k] <- dum$estimate
		ccfp[k] <- dum$p.value
	}
	out[[periods+j]] <- ccfs
	out[[periods+j+nLags]] <- ccfp
}
if(loud) cat("Computed CC's out to lag",nLags,"\n")
if(loud) cat("Done. Output is a List of Lists\n")
if(loud) plot(out[[periods+1]],main="Lag 1 Correlation Coefficients",ylab="r-squared",xlab="pair of periods")
out
}
}

