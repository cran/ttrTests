nullModel <-
function(x,model="stationaryBootstrap",userParams=0,nSamples=100,ttr="macd4",params=0,burn=0,short=FALSE,silent=TRUE,loud=TRUE,alpha=0.025,TC=0.001,benchmark="hold",latex="")

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
		## ##	pObs <- stat[,3][2]
	mObs <- stat[[1]][1]
	srObs <- stat[[1]][3]
	
		CRA <- stat[[2]][1]
		ERA <- CRA-m1[1]
		if(loud) cat("Observed Mean Excess Return is",CRA-m1[1],"\nProceding with Model:",model,"\n")
		if(loud)  cat("\nCreating and Analyzing",nSamples,"Samples\n")
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
				cat("*************************************************************\n")
				cat("Completed: ")
				flush.console()
			}
			if(k>5) if( floor(10*k/nSamples) > floor(10*(k-1)/nSamples) ) {
				if(loud) cat(10*floor(10*k/nSamples),"% ")
				flush.console() }
			sample <- generateSample(x=x,model=model,userParams=userParams)
			stat <- returnStats(sample,ttr=ttr,params=params,burn=burn,short=short,silent=silent,TC=TC,benchmark=benchmark)
	## ##			Z[k] <- stat[,3][1]
	## ##			P[k] <- stat[,3][2]
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
	## ##		pCount <- ifelse(P<=pObs,1,0)
	## ##		nOutliers <- sum(pCount)
	## ##		if(loud) cat("\nDone\n","Observed",sum(pCount),"Comparable Outliers out of",nSamples,"samples\n")
		srRatio <- SR/srObs
		srPercent <- sum(ifelse(srRatio>1,1,0))/length(srRatio)
		arImprove <- AR - mObs
		arPercent <- sum(ifelse(arImprove>0,1,0))/length(arImprove)
	## ##		pOutlier <- pObs
	## ##		pNoutlier <- 0
	## ##		if(nOutliers>=1) for(k in 1:nOutliers) pNoutlier[k] <- choose(nSamples,k-1)*pOutlier^(k-1)*(1-pOutlier)^(nSamples-k+1)
	## ##		if(loud) cat("P-value for",nOutliers,"Outliers or More:", 1-sum(pNoutlier),"\n")
	## ##		if(loud) cat(100*round(srPercent,3),"% of samples had improved Sharpe ratio\n")
	## ## 		if(loud) cat(100*round(arPercent,3),"% of samples had excess returns after adjustment for trading costs\n")
		if(loud) cat("\n*************************************************************\n")
		if(loud) cat("Mean:",m1[1]," Average sample mean:",mean(m1[2:length(m1)]),"\n")
		if(loud) cat("Var:",m2[1]," Average sample var:",mean(m2[2:length(m2)]),"\n")
		if(loud) cat("Skew:",m3[1]," Average sample skew:",mean(m3[2:length(m3)]),"\n")
		if(loud) cat("Kurt:",m4[1]," Average sample kurt:",mean(m4[2:length(m4)]),"\n")
		if(loud) cat("\nAverage excess return:",mean(ER),"\n")
		if(loud) cat("Variance of excess return:",var(ER),"\n")
	## ##		if(loud) cat("Theoretical Variance:",subsetVar(diff(log(x)),floor(mean(subSize))),"\n")
		Z <- (ERA-mean(ER))/sqrt(var(ER))
		P <- ifelse(Z>0,1-pnorm(Z),pnorm(Z))
		if(loud) cat("\nZ-score and P-value for Observed Excess Return:",Z,P,"\n")
		
	if(! latex=="")
		{
		cat("\n\\begin{figure}[h]\n",file=latex,append=TRUE)
		cat("\\centering\n",file=latex,append=TRUE)
		cat("\\title{Observed Excess Return versus Estimates from Random Samples}\n",file=latex,append=TRUE)
		cat("\\begin{tabular}{ c c c c c }\n",file=latex,append=TRUE)
		cat("mean & std dev & observed & z-score & p-value \\\\ \\hline \n",file=latex,append=TRUE)
		if(P<alpha) cat(mean(ER)," &",sqrt(var(ER))," &",ERA," &",Z," &",P,"*** \\\\ \n",file=latex,append=TRUE)
		else cat(mean(ER)," &",sqrt(var(ER))," &",ERA," &",Z," &",P," \\\\ \n",file=latex,append=TRUE)
		cat("\\end{tabular}\n",file=latex,append=TRUE)
		if(P<alpha) cat("\\caption{Random Samples from Model:",model," P-value significant for alpha =",alpha,"}\n",file=latex,append=TRUE)
		else cat("\\caption{Random Samples from Model:",model,".  P-value not significant for alpha =",alpha,"}\n",file=latex,append=TRUE)
		cat("\\end{figure}\n",file=latex,append=TRUE)
		if(loud) cat("\n Results written as latex figure to file:",latex,"\n")
	}

list(CR,SR,AR,Z,P)
	
}

