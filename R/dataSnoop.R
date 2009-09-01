dataSnoop <-
function(x,ttr="macd4",start=0,nSteps=0,stepSize=0,burn=0,short=FALSE,silent=TRUE,TC=0.001,loud=TRUE,alpha=0.025,begin=1,percent=1,file="",benchmark="hold",bSamples=100,test="SPA",latex="")
{

V <- 0
V1 <- 0
V2 <- 0
V3 <- 0

par1 <- paramStats(x,ttr=ttr,start=start,nSteps=nSteps,stepSize=stepSize,burn=burn,short=short,silent=silent,TC=TC,loud=loud,alpha=alpha,begin=begin,percent=percent,file=file,benchmark=benchmark)
omega <- par1[[1]]/par1[[4]]
rBest <- par1[[5]]
sBest <- par1[[6]]

if(test=="RC") {
if(loud) cat("\nPerforming White's Reality Check\n\n")
Vi <- max(par1[[1]])*sqrt(length(x)) }
else if(test=="SPA") {
if(loud) cat("\nPerforming Hansen's test for Superior Predictive Ability\n\n")
Vi <- max(par1[[4]],0)*sqrt(length(x)) }
else stop("Please Choose RC or SPA for test")

then <- timeDate()
for(counter in 1:bSamples)
	{
	sam <- generateSample(x)
	par <- paramStats(sam,ttr=ttr,start=start,nSteps=nSteps,stepSize=stepSize,burn=burn,short=short,silent=TRUE,TC=TC,loud=FALSE,plot=FALSE,alpha=alpha,begin=begin,percent=percent,file=file,benchmark=benchmark)
	if(counter>5) if( floor(10*counter/bSamples) > floor(10*(counter-1)/bSamples) ) {
		if(loud) cat(10*floor(10*counter/bSamples),"% ")	
		flush.console() }
	if(test=="RC") {
	dum <- par[[1]] - par1[[1]] 
	V[counter] <- max(dum)*sqrt(length(x))
	}
	else if(test=="SPA") {
	dum1 <- par[[1]] - ifelse(par1[[1]]>0,par1[[1]],0)
	dum2 <- par[[1]] - ifelse(par1[[1]] >= -sqrt( omega^2 * log(log(length(x))) / length(x) ) , par1[[1]] , 0)
	dum3 <- par[[1]] - par1[[1]]
	V1[counter] <- max(sqrt(length(x))*dum1/omega,0)
	V2[counter] <- max(sqrt(length(x))*dum2/omega,0)
	V3[counter] <- max(sqrt(length(x))*dum3/omega,0)
	}
	else stop("this is an impossible error message")

	if(counter==1) if(loud)
	{ 	now <- timeDate()
		rtime <- as.double(difftimeDate(now,then,unit="secs"))*bSamples
		days <- floor(rtime/(60*60*24))
		hours <- floor(rtime/(60*60)) - 24*days
		mins <- floor(rtime/60) - 24*60*days - 60*hours
		secs <- rtime - 24*60*60*days - 60*60*hours - 60*mins
		cat("Preparing to Analyze",bSamples,"Stationary Bootstrap Samples\n")
		cat("Estimated Run Time:",days,"Days",hours,"Hours",mins,"Minutes",secs,"Seconds\n")
		cat("\n********************************************************************\n")
		cat("Completed: ")
		flush.console()
	}
	}

if(loud) cat("\n********************************************************************\n")

if(! latex=="") {
	dFaults <- defaults(ttr=ttr)
	if(start[1]==0) start <- dFaults[[2]]
	if(stepSize[1]==0) stepSize <- dFaults[[3]]
	if(nSteps[1]==0) nSteps <- dFaults[[4]]
}

if(test=="RC") {
	if(loud) cat("\nMean and Var of Bootstrapped V:",mean(V),var(V),"\n")
	z <- (Vi - mean(V))/sqrt(var(V))
	p <- ifelse(z>0,1-pnorm(z),pnorm(z))
	if(loud) cat("Estimated Z and P-Value for Observed V:",z,p,"\n")
	
	if(! latex=="")
		{
		cat("\n\\begin{figure}[h]\n",file=latex,append=TRUE)
		cat("\\centering\n",file=latex,append=TRUE)
		cat("\\title{Bootstrap Data Snooper for TTR:",ttr,"}\n",file=latex,append=TRUE)
		cat("\\begin{tabular}{ c c c c c c}\n",file=latex,append=TRUE)
		cat("from & by & steps & total size & best choice & p-value \\\\ \\hline \n",file=latex,append=TRUE)
		if(p<alpha) cat(start," &",stepSize," &",nSteps," &",prod(nSteps)," &",rBest," &",p,"*** \\\\ \n",file=latex,append=TRUE)
		else cat(start," &",stepSize," &",nSteps," &",prod(nSteps)," &",rBest," &",p," \\\\ \n",file=latex,append=TRUE)
		cat("\\end{tabular}\n",file=latex,append=TRUE)
		if(p<alpha) cat("\\caption{P-value from Reality Check significant for alpha =",alpha,"}\n",file=latex,append=TRUE)
		else cat("\\caption{P-value from Reality Check not significant for alpha =",alpha,"}\n",file=latex,append=TRUE)
		cat("\\end{figure}\n",file=latex,append=TRUE)
		if(loud) cat("\n Results written as latex figure to file:",latex,"\n")
	}

	ifelse(z>0,1-pnorm(z),pnorm(z)) 

}
else if(test=="SPA") {
	dum1 <- ifelse(V1>Vi,1,0)
	dum2 <- ifelse(V2>Vi,1,0)
	dum3 <- ifelse(V3>Vi,1,0)
	p1 <- (1/bSamples)*sum(dum1)
	p2 <- (1/bSamples)*sum(dum2)
	p3 <- (1/bSamples)*sum(dum3)
	if(loud) cat("\nObserved P-values for Means 'l', 'c', and 'u' respectively:",p1,p2,p3,"\n")
	
	
	if(! latex=="")
		{
		cat("\n\\begin{figure}[h]\n",file=latex,append=TRUE)
		cat("\\centering\n",file=latex,append=TRUE)
		cat("\\title{Bootstrap Data Snooper for TTR:",ttr,"}\n",file=latex,append=TRUE)
		cat("\\begin{tabular}{ c c c c c c}\n",file=latex,append=TRUE)
		cat("from & by & steps & total size & best choice & p-value \\\\ \\hline \n",file=latex,append=TRUE)
		if(max(p1,p2,p3)<alpha) cat(start," &",stepSize," &",nSteps," &",prod(nSteps)," &",sBest," &",max(p1,p2,p3),"*** \\\\ \n",file=latex,append=TRUE)
		else cat(start," &",stepSize," &",nSteps," &",prod(nSteps)," &",sBest," &",p," \\\\ \n",file=latex,append=TRUE)
		cat("\\end{tabular}\n",file=latex,append=TRUE)
		if(max(p1,p2,p3)<alpha) cat("\\caption{Max P-value from SPA significant for alpha =",alpha,"}\n",file=latex,append=TRUE)
		else cat("\\caption{Max P-value from SPA not significant for alpha =",alpha,"}\n",file=latex,append=TRUE)
		cat("\\end{figure}\n",file=latex,append=TRUE)
		if(loud) cat("\n Results written as latex figure to file:",latex,"\n")
	}

	c(p1,p2,p3)
}
else stop("this is an impossible error message")
}
