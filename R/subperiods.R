subperiods <-
function(x,ttr="macd4",start=0,nSteps=0,stepSize=0,restrict=FALSE,burn=0,short=FALSE,condition=NULL,silent=TRUE,TC=0.001,loud=TRUE,plot=TRUE,alpha=0.05,periods=0,file="",latex="",benchmark="hold")

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
if(periods==0) periods <- floor(length(x)/100)+1
if(periods==1) { paramStats(x=x,ttr=ttr,start=start,nSteps=nSteps,stepSize=stepSize,restrict=restrict,burn=burn,short=short,condition=condition,silent=silent,plot=plot,TC=TC,loud=loud,alpha=alpha,begin=1,percent=1,file=file,benchmark=benchmark) }
else
{

if(loud) cat("Preparing to Analyze",periods,"Subperiods\n")
then <- timeDate()
percent <- 1/periods
data <- paramStats(x=x,ttr=ttr,start=start,nSteps=nSteps,stepSize=stepSize,restrict=restrict,burn=burn,short=short,condition=condition,silent=TRUE,TC=TC,loud=FALSE,plot=FALSE,alpha=alpha,begin=1,percent=(1/periods),file=file,benchmark=benchmark)
out <- list(data)
xVars <- data[[1]]
yVars <- NULL

if(loud) if(periods>3)
	{ 	now <- timeDate()
		rtime <- as.double(difftimeDate(now,then,unit="secs"))*periods
		days <- floor(rtime/(60*60*24))
		hours <- floor(rtime/(60*60)) - 24*days
		mins <- floor(rtime/60) - 24*60*days - 60*hours
		secs <- rtime - 24*60*60*days - 60*60*hours - 60*mins
		cat("Estimated Run Time:",days,"Days",hours,"Hours",mins,"Minutes",secs,"Seconds\n")
		cat("\n********************************************************************\n")
		cat("Completed: ")
		flush.console()
	}

for(i in 2:periods)
	{ 
	if(i>2) if(periods>3) if( floor(10*i/periods) > floor(10*(i-1)/periods) ) {
		if(loud) cat(10*floor(10*i/periods),"% ")	
		flush.console() }
	
	begin <- 1+floor( (i-1)*percent*length(x) )
	data <- paramStats(x=x,ttr=ttr,start=start,nSteps=nSteps,stepSize=stepSize,restrict=restrict,burn=burn,short=short,condition=condition,silent=TRUE,TC=TC,loud=FALSE,plot=FALSE,alpha=alpha,begin=begin,percent=(1/periods),file=file,benchmark=benchmark)
	out[[i]] <- data
	yVars <- c(yVars,data[[1]])
	if(i<periods) xVars <- c(xVars,data[[1]])
	}

if(loud) if(periods>3) cat("\n********************************************************************\n")
cHat <- 1/length(xVars)*sum(xVars*yVars)-1/(length(xVars)^2)*sum(xVars)*sum(yVars)
rhoHat <- cHat/sqrt(var(xVars)*var(yVars))
## if(loud) cat("\nComputed test statistic C-hat =",cHat,"\n")
if(loud) cat("Observed correlation coefficient",rhoHat,"\n")
if(plot) plot(xVars,yVars,main="Ordered Pairs Data",ylab="f(k,t+1) = excess return for strategy 'k'",xlab="f(k,t)")

## ## nLags <- min(5,periods-1)
## 
## out[[periods+1]] <- regress
## if(loud) cat("Measured coefficient:",summary(regress)$coef[,"Estimate"]["xVars"],"with P-Value:",summary(regress)$coef[,"Pr(>|t|)"]["xVars"],"\n")
## if(loud) {
## if(summary(regress)$coef[,"Pr(>|t|)"]["xVars"] < alpha) cat("Significant at level alpha =",alpha,"\n")
## else cat("Not Significant at Level alpha =",alpha,"\n")
## }
## if(! latex=="")
##		{
##		cat("\n\\begin{figure}[h]\n",file=latex,append=TRUE)
##		cat("\\centering\n",file=latex,append=TRUE)
##		cat("\\begin{tabular}{ c c c }\n",file=latex,append=TRUE)
##		if(summary(regress)$coef[,"Pr(>|t|)"]["xVars"] < alpha ) cat(periods," &",summary(regress)$coef[,"Estimate"]["xVars"]," &",summary(regress)$coef[,"Pr(>|t|)"]["xVars"],"*** \\\\ \n",file=latex,append=TRUE)
##		else cat(periods," &",summary(regress)$coef[,"Estimate"]["xVars"]," &",summary(regress)$coef[,"Pr(>|t|)"]["xVars"]," \\\\ \n",file=latex,append=TRUE)
##		cat("\\end{tabular}\n",file=latex,append=TRUE)
##		if(summary(regress)$coef[,"Pr(>|t|)"]["xVars"]<alpha) cat("\\caption{P-value for Regression Coefficient significant for alpha =",alpha,"}\n",file=latex,append=TRUE)
##		else cat("\\caption{Max P-value for Regression Coefficient not significant for alpha =",alpha,"}\n",file=latex,append=TRUE)
##		cat("\\end{figure}\n",file=latex,append=TRUE)
##		if(loud) cat("\n Results written as latex figure to file:",latex,"\n")
##	}
c(cHat, rhoHat, out)
}
}

