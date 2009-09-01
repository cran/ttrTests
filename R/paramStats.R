paramStats <-
function(x,ttr="macd4",start=0,nSteps=0,stepSize=0,restrict=FALSE,burn=0,short=FALSE,silent=TRUE,TC=0.001,loud=TRUE,plot=TRUE,alpha=0.025,begin=1,percent=1,file="",benchmark="hold")

## Computes conditional return statistics
## For each parameterization of TTR in a given domain
##
## Returns a list of column vectors
##
## When assembled into rows, each row gives
## the parameters used and the return statistics (r,s,ar,s0,s1,z)
## r = conditional return
## s = 1 if this return is significant
## z = z-score amongst all parameterizations considered
##

{
if(start[1]==0) start <- defaults(ttr)[[2]]
if(stepSize[1]==0) stepSize <- defaults(ttr)[[3]]
if(nSteps[1]==0) nSteps <- defaults(ttr)[[4]]

if(defaults(ttr)[[5]]) restrict <- TRUE

k <- floor(length(x)*percent)
end <- min(begin + k - 1,length(x))
x <- x[begin:end]

if(length(start)==4)
{
	mVals <- stepSize[1]*(1:nSteps[1]-1)+start[1]
	nVals <- stepSize[2]*(1:nSteps[2]-1)+start[2]
	pVals <- stepSize[3]*(1:nSteps[3]-1)+start[3]
	qVals <- stepSize[4]*(1:nSteps[4]-1)+start[4]
	mList <- 0
	nList <- 0
	pList <- 0
	qList <- 0
	cMean <- 0
	sig <- 0
	aRet <- 0
	s0 <- 0
	s1 <- 0
	counter <- 0
	rBest <- 0
	sBest <- 0
	rMax <- 0
	sMax <- 0

	if(loud) cat("\nPreparing to Analyze",prod(nSteps),"Parameterizations of TTR\n")
	flush.console()
	rtime <- 0
	then <- timeDate()
	for(m in mVals) {
	for(n in ifelse(restrict,m,0)+nVals) {
	for(p in pVals) {
	for(q in ifelse(restrict,p,0)+qVals) {
		counter <- counter +1
		if(counter==5) 
			{
			now <- timeDate()
			secs <- as.double((now-then)*prod(nSteps)/counter)
			rtime <- secs
			if(loud){	
				days <- floor(secs/(60*60*24))
				hours <- floor(secs/(60*60)) - 24*days
				mins <- floor(secs/60) - 24*60*days - 60*hours
				secs <- secs - 24*60*60*days - 60*60*hours - 60*mins
				cat("Estimated Run Time:",days,"Days",hours,"Hours",mins,"Minutes",secs,"Seconds\n")
				cat("\n********************************************************************\n")
				cat("Completed: ")
				flush.console()
			}
		}
	if(loud) if(counter>5) if( floor(10*counter/prod(nSteps)) > floor(10*(counter-1)/prod(nSteps)) ) {
		cat(10*floor(10*counter/prod(nSteps)),"% ")
		flush.console() }
	stat <- returnStats(x=x,ttr=ttr,params=c(m,n,p,q),burn=burn,short=short,silent=TRUE,TC=TC,benchmark=benchmark)
	mList[counter] <- m
	nList[counter] <- n
	pList[counter] <- p
	qList[counter] <- q
	cMean[counter] <- stat[[2]][1]-stat[[1]][1]
	## ##	pVal <- stat[[3]][4]
	## ## sig[counter] <- as.double(pVal<alpha)
	aRet[counter] <- stat[[3]][1]
	s0[counter] <- stat[[4]][3]
	if(counter==1) {
		rMax <- cMean[counter]
		rBest <- c(m,n,p,q) }
	if(counter>1) if(cMean[counter]>rMax) {
		rMax <- cMean[counter]
		rBest <- c(m,n,p,q) }
	if(counter==1) {
		sMax <- s0[counter]
		sBest <- c(m,n,p,q) }
	if(counter>1) if(s0[counter]>sMax) {
		sMax <- s0[counter]
		sBest <- c(m,n,p,q) }
	## ## s1[counter] <- as.double(stat[[2]][3]>stat[[1]][3])
	}
	}
	}
	}
}
else if(length(start)==3)
{
	mVals <- stepSize[1]*(1:nSteps[1]-1)+start[1]
	nVals <- stepSize[2]*(1:nSteps[2]-1)+start[2]
	pVals <- stepSize[3]*(1:nSteps[3]-1)+start[3]
	mList <- 0
	nList <- 0
	pList <- 0
	cMean <- 0
	sig <- 0
	aRet <- 0
	s0 <- 0
	counter <- 0
	if(loud) cat("\nPreparing to Analyze",prod(nSteps),"Parameterizations of TTR\n")
	rtime <- 0
	then <- timeDate()
	for(m in mVals) {
	for(n in ifelse(restrict,m,0)+nVals) {
	for(p in pVals) {
		counter <- counter +1
		if(counter==5) 
			{
			now <- timeDate()
			secs <- as.double((now-then)*prod(nSteps)/counter)
			rtime <- secs
			if(loud){	
				days <- floor(secs/(60*60*24))
				hours <- floor(secs/(60*60)) - 24*days
				mins <- floor(secs/60) - 24*60*days - 60*hours
				secs <- secs - 24*60*60*days - 60*60*hours - 60*mins
				cat("Estimated Run Time:",days,"Days",hours,"Hours",mins,"Minutes",secs,"Seconds\n")
				cat("\n********************************************************************\n")
				cat("Completed: ")
				flush.console()
			}
		}
	if(loud) if(counter>5) if( floor(10*counter/prod(nSteps)) > floor(10*(counter-1)/prod(nSteps)) ) {
		cat(10*floor(10*counter/prod(nSteps)),"% ")
		flush.console() }
	stat <- returnStats(x=x,ttr=ttr,params=c(m,n,p),burn=burn,short=short,silent=TRUE,TC=TC,benchmark=benchmark)
	mList[counter] <- m
	nList[counter] <- n
	pList[counter] <- p
	cMean[counter] <- stat[[2]][1]-stat[[1]][1]
	## ##	pVal <- stat[[3]][4]
	## ## sig[counter] <- as.double(pVal<alpha)
	aRet[counter] <- stat[[3]][1]
	s0[counter] <- stat[[4]][3]
	if(counter==1) {
		rMax <- cMean[counter]
		rBest <- c(m,n,p) }
	if(counter>1) if(cMean[counter]>rMax) {
		rMax <- cMean[counter]
		rBest <- c(m,n,p) }
	if(counter==1) {
		sMax <- s0[counter]
		sBest <- c(m,n,p) }
	if(counter>1) if(s0[counter]>sMax) {
		sMax <- s0[counter]
		sBest <- c(m,n,p) }
	## ## s1[counter] <- as.double(stat[[2]][3]>stat[[1]][3])
	}
	}
	}
}
else if(length(start)==2)
{
	mVals <- stepSize[1]*(1:nSteps[1]-1)+start[1]
	nVals <- stepSize[2]*(1:nSteps[2]-1)+start[2]
	mList <- 0
	nList <- 0
	cMean <- 0
	sig <- 0
	aRet <- 0
	s0 <- 0
	counter <- 0
	if(loud) cat("\nPreparing to Analyze",prod(nSteps),"Parameterizations of TTR\n")
	rtime <- 0
	then <- timeDate()
	for(m in mVals) {
	for(n in nVals) {
		counter <- counter +1
		if(counter==5) 
		{
			now <- timeDate()
			secs <- as.double((now-then)*prod(nSteps)/counter)
			rtime <- secs
		if(loud){	
			days <- floor(secs/(60*60*24))
			hours <- floor(secs/(60*60)) - 24*days
			mins <- floor(secs/60) - 24*60*days - 60*hours
			secs <- secs - 24*60*60*days - 60*60*hours - 60*mins
			cat("Estimated Run Time:",days,"Days",hours,"Hours",mins,"Minutes",secs,"Seconds\n")
			cat("\n********************************************************************\n")
			cat("Completed: ")
			flush.console()
			}
		}
	if(loud) if(counter>5) if( floor(10*counter/prod(nSteps)) > floor(10*(counter-1)/prod(nSteps)) ) {
		cat(10*floor(10*counter/prod(nSteps)),"% ")
		flush.console() }
	stat <- returnStats(x=x,ttr=ttr,params=c(m,n),burn=burn,short=short,silent=TRUE,TC=TC,benchmark=benchmark)
	mList[counter] <- m
	nList[counter] <- n
	cMean[counter] <- stat[[2]][1]-stat[[1]][1]
	## ##	pVal <- stat[[3]][4]
	## ## sig[counter] <- as.double(pVal<alpha)
	aRet[counter] <- stat[[3]][1]
	s0[counter] <- stat[[4]][3]
	if(counter==1) {
		rMax <- cMean[counter]
		rBest <- c(m,n) }
	if(counter>1) if(cMean[counter]>rMax) {
		rMax <- cMean[counter]
		rBest <- c(m,n) }
	if(counter==1) {
		sMax <- s0[counter]
		sBest <- c(m,n) }
	if(counter>1) if(s0[counter]>sMax) {
		sMax <- s0[counter]
		sBest <- c(m,n) }
	## ## s1[counter] <- as.double(stat[[2]][3]>stat[[1]][3])
	}
	}
}
else if(length(start)==1)
{
	mVals <- stepSize[1]*(1:nSteps[1]-1)+start[1]
	mList <- 0
	cMean <- 0
	sig <- 0
	aRet <- 0
	s0 <- 0
	counter <- 0
	if(loud) cat("\nPreparing to Analyze",prod(nSteps),"Parameterizations of TTR\n")
	rtime <- 0
	then <- timeDate()
	for(m in mVals) {
		counter <- counter +1
		if(counter==5) 
		{
			now <- timeDate()
			secs <- as.double((now-then)*prod(nSteps)/counter)
			rtime <- secs
		if(loud){	
			days <- floor(secs/(60*60*24))
			hours <- floor(secs/(60*60)) - 24*days
			mins <- floor(secs/60) - 24*60*days - 60*hours
			secs <- secs - 24*60*60*days - 60*60*hours - 60*mins
			cat("Estimated Run Time:",days,"Days",hours,"Hours",mins,"Minutes",secs,"Seconds\n")
			cat("\n********************************************************************\n")
			cat("Completed: ")
			flush.console()
			}
		}
	if(loud) if(counter>5) if( floor(10*counter/prod(nSteps)) > floor(10*(counter-1)/prod(nSteps)) ) {
		cat(10*floor(10*counter/prod(nSteps)),"% ")
		flush.console() }
	stat <- returnStats(x=x,ttr=ttr,params=c(m),burn=burn,short=short,silent=TRUE,TC=TC,benchmark=benchmark)
	mList[counter] <- m
	cMean[counter] <- stat[[2]][1]-stat[[1]][1]
	## ##	pVal <- stat[[3]][4]
	## ## sig[counter] <- as.double(pVal<alpha)
	aRet[counter] <- stat[[3]][1]
	s0[counter] <- stat[[4]][3]
	if(counter==1) {
		rMax <- cMean[counter]
		rBest <- m }
	if(counter>1) if(cMean[counter]>rMax) {
		rMax <- cMean[counter]
		rBest <- m }
	if(counter==1) {
		sMax <- s0[counter]
		sBest <- m }
	if(counter>1) if(s0[counter]>sMax) {
		sMax <- s0[counter]
		sBest <- m }
	## ## s1[counter] <- as.double(stat[[2]][3]>stat[[1]][3])
	}
}
else if(length(start)>4)
	{
	cat("Error: Cannot vary more than 4 parameters\n")
	NA
}
else
	{ 
	cat("Unexpected Error\n")
	NA
}


zScore <- (cMean-mean(cMean))/sqrt(var(cMean))
if(loud) cat("\n********************************************************************\n")
if(loud) cat("\nBest Result:",max(cMean),"for Parameters:",rBest," White's V-hat-n:",max(cMean)*sqrt(length(x)),"\n")
if(loud) cat("Best Sharpe(0):",max(s0),"for Parameters:",sBest," Hansen's T-SPA:",max(s0,0)*sqrt(length(x)),"\n")
if(plot) plot(cMean,main="Excess Returns",ylab="Conditional Mean - Benchmark Mean",xlab="Parameter Choice")
if(! file=="")
	{
	table <- array(c(mList,nList,pList,qList,cMean,aRet,s0,zScore),c(length(mList),8))
	for(k in 1:length(mList)) cat(table[k,],"\n",file=file,append=TRUE)
	cat("\n Output also stored in file:",file,"\n")
}
if(length(start)==1) list(cMean,zScore,aRet,s0,rBest,sBest,mList)
else if(length(start)==2) list(cMean,zScore,aRet,s0,rBest,sBest,mList,nList)
else if(length(start)==3) list(cMean,zScore,aRet,s0,rBest,sBest,mList,nList,pList)
else if(length(start)==4) list(cMean,zScore,aRet,s0,rBest,sBest,mList,nList,pList,qList)
else NA
}

