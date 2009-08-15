nullModel <-
function(x,model="stationaryBootstrap",userParams=0,nSamples=100,ttr="macd4",params=0,burn=0,short=FALSE,silent=TRUE,loud=TRUE,alpha=0.025,TC=0.001,benchmark="hold")

## For a data set whose conditional mean is significant
## When using TTR to identify a subset,
## We create 'nSamples' samples and for each
## From one of a number of models, then
## We decide if the subset identified by the TTR is significant
##
## Hypothesis Testing:
##
## H0 - model structure does not induce significant returns
##
## HA - Significant returns are due to model structure,
##      such as autocorrelation or conditional heteroskedasticity
##
{ 
	stat <- returnStats(x=x,ttr=ttr,params=params,burn=burn,short=short,silent=silent,TC=TC,benchmark=benchmark)

	
	if(! is.ts(x)) if(! is.vector(x)) {
	y <- as.ts(x[,c("Close")])
	y <- as.vector(y)
	}
	else y <- x

	m1 <- 0
	m2 <- 0
	m3 <- 0
	m4 <- 0
	m1[1] <- stat[[1]][1]
	m2[1] <- stat[[1]][2]
	m3[1] <- stat[[1]][4]
	m4[1] <- stat[[1]][5]
	Z <- 0
	P <- 0
	CR <- 0
	ER <- 0
	SR <- 0
	AR <- 0
	subSize <- 0
	mObs <- stat[[1]][1]
	srObs <- stat[[1]][3]
	
		CRA <- stat[[2]][1]
		ERA <- CRA-m1[1]
		if(loud) cat("Average Excess Return is",CRA-m1[1]," Proceding with Model:",model,"\n")
		if(loud)  cat("Creating and Analyzing",nSamples,"Samples\n")
		then <- timeDate()
		for(k in 1:nSamples) 
			{  
			if(k==5) if(loud) 
				{
				now <- timeDate()
				secs <- as.double((now-then)*nSamples/k)
				days <- floor(secs/(60*60*24))
				hours <- floor(secs/(60*60)) - 24*days
				mins <- floor(secs/60) - 24*60*days - 60*hours
				secs <- secs - 24*60*60*days - 60*60*hours - 60*mins
				cat("Estimated Run Time:",days,"Days",hours,"Hours",mins,"Minutes",secs,"Seconds\n")
				cat("Completed: ")
			}
			if(k>5) if( floor(10*k/nSamples) > floor(10*(k-1)/nSamples) ) cat(10*floor(10*k/nSamples),"% ")
			sample <- generateSample(x=x,model=model,userParams=userParams)
			stat <- returnStats(x=sample,ttr=ttr,params=params,burn=burn,short=short,silent=silent,TC=TC,benchmark=benchmark)
			CR[k] <- stat[[2]][1]
			ER[k] <- CR[k] - stat[[1]][1]
			SR[k] <- stat[[2]][3]
			AR[k] <- stat[[3]][1]
			subSize[k] <- stat[[3]][2]
			m1[k+1] <- stat[[1]][1]
			m2[k+1] <- stat[[1]][2]
			m3[k+1] <- stat[[1]][4]
			m4[k+1] <- stat[[1]][5]
		}
		srRatio <- SR/srObs
		srPercent <- sum(ifelse(srRatio>1,1,0))/length(srRatio)
		arImprove <- AR - mObs
		arPercent <- sum(ifelse(arImprove>0,1,0))/length(arImprove)
		if(loud) cat("Results:\n")
		if(loud) cat("Mean:",m1[1]," Average sample mean:",mean(m1[2:length(m1)]),"\n")
		if(loud) cat("Var:",m2[1]," Average sample var:",mean(m2[2:length(m2)]),"\n")
		if(loud) cat("Skew:",m3[1]," Average sample skew:",mean(m3[2:length(m3)]),"\n")
		if(loud) cat("Kurt:",m4[1]," Average sample kurt:",mean(m4[2:length(m4)]),"\n")
		if(loud) cat("Average excess return:",mean(ER),"\n")
		if(loud) cat("Variance of excess return:",var(ER),"\n")
		Z <- (ERA-mean(ER))/sqrt(var(ER))
		P <- ifelse(Z>0,1-pnorm(Z),pnorm(Z))
		if(loud) cat("Z-score and P-value for Observed Excess Return:",Z,P,"\n")
		list(CR,SR,AR,Z,P)
	
}

