realityCheck <-
function(x,ttr="macd4",start=0,nSteps=0,stepSize=0,burn=0,short=FALSE,silent=TRUE,TC=0.001,loud=TRUE,alpha=0.025,begin=1,percent=1,file="",benchmark="hold",bSamples=100)
{

V <- 0
par1 <- paramStats(x,ttr=ttr,start=start,nSteps=nSteps,stepSize=stepSize,burn=burn,short=short,silent=silent,TC=TC,loud=loud,alpha=alpha,begin=begin,percent=percent,file=file,benchmark=benchmark)

Vi <- max(par1[[1]])*sqrt(length(x))

then <- timeDate()
for(counter in 1:bSamples)
	{
	sam <- generateSample(x)
	par <- paramStats(sam,ttr=ttr,start=start,nSteps=nSteps,stepSize=stepSize,burn=burn,short=short,silent=TRUE,TC=TC,loud=FALSE,alpha=alpha,begin=begin,percent=percent,file=file,benchmark=benchmark)
	if(counter>5) if( floor(10*counter/bSamples) > floor(10*(counter-1)/bSamples) ) cat(10*floor(10*counter/bSamples),"% ")	
	dum <- par[[1]] - par1[[1]]

	V[counter] <- max(dum)*sqrt(length(x))
	if(counter==1) if(loud)
	{ 	now <- timeDate()
		rtime <- as.double(now-then)*bSamples
		if(rtime<3*bSamples) rtime <- rtime*60
		days <- floor(rtime/(60*60*24))
		hours <- floor(rtime/(60*60)) - 24*days
		mins <- floor(rtime/60) - 24*60*days - 60*hours
		secs <- rtime - 24*60*60*days - 60*60*hours - 60*mins
		cat("Preparing to Analyze",bSamples,"Stationary Block Bootstrap Samples\n")
		cat("Estimated Run Time:",days,"Days",hours,"Hours",mins,"Minutes",secs,"Seconds\n")
		cat("(or less than",3*bSamples,"seconds, if first call was < 3 seconds)\n")
		cat("Completed: ")
	}
	}

if(loud) cat("\nDone\n")
if(loud) cat("Mean and Var of Bootstrapped V:",mean(V),var(V),"\n")
z <- (Vi - mean(V))/sqrt(var(V))
if(loud) cat("Estimated Z and P-Value for Observed V:",z,ifelse(z>0,1-pnorm(z),pnorm(z)),"\n")
list( par1,Vi,V,z,ifelse(z>0,1-pnorm(z),pnorm(z)) )
}

