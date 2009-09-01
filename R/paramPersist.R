paramPersist <-
function(x,ttr="macd4",start=0,nSteps=0,stepSize=0,burn=0,short=FALSE,silent=TRUE,TC=0.001,loud=TRUE,plot=TRUE,alpha=0.05,periods=0,file="",latex="")

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
if(periods==1) { paramStats(x=x,ttr=ttr,start=start,nSteps=nSteps,stepSize=stepSize,burn=burn,short=short,silent=silent,plot=plot,TC=TC,loud=loud,alpha=alpha,begin=1,percent=1,file=file) }
else
{

if(loud) cat("Preparing to Analyze",periods,"Subperiods\n")
then <- timeDate()
percent <- 1/periods
data <- paramStats(x=x,ttr=ttr,start=start,nSteps=nSteps,stepSize=stepSize,burn=burn,short=short,silent=TRUE,TC=TC,loud=FALSE,plot=FALSE,alpha=alpha,begin=1,percent=(1/periods),file=file)
out <- list(data)
xVars <- data[[1]]
yVars <- NULL

for(i in 2:periods)
	{ 
	if(i>6) if( floor(10*i/periods) > floor(10*(i-1)/periods) ) {
		if(loud) cat(10*floor(10*i/periods),"% ")	
		flush.console() }
	if(i==6) if(loud)
	{ 	now <- timeDate()
		rtime <- as.double(difftimeDate(now,then,unit="secs"))*periods/5
		days <- floor(rtime/(60*60*24))
		hours <- floor(rtime/(60*60)) - 24*days
		mins <- floor(rtime/60) - 24*60*days - 60*hours
		secs <- rtime - 24*60*60*days - 60*60*hours - 60*mins
		cat("Estimated Run Time:",days,"Days",hours,"Hours",mins,"Minutes",secs,"Seconds\n")
		cat("\n********************************************************************\n")
		cat("Completed: ")
		flush.console()
	}
	begin <- 1+floor( (i-1)*percent*length(x) )
	data <- paramStats(x=x,ttr=ttr,start=start,nSteps=nSteps,stepSize=stepSize,burn=burn,short=short,silent=TRUE,TC=TC,loud=FALSE,plot=FALSE,alpha=alpha,begin=begin,percent=(1/periods),file=file )
	out[[i]] <- data
	yVars <- c(yVars,data[[1]])
	if(i<periods) xVars <- c(xVars,data[[1]])
	}

## ## nLags <- min(5,periods-1)
if(loud) cat("\n********************************************************************\n")
if(loud) cat("\nTesting Model: f(k,t) ~ int + coef*f(k,t-1) + eps\n")
if(plot) plot(xVars,yVars,main="Regression Analysis",ylab="f(k,t+1) = excess return for strategy 'k'",xlab="f(k,t)")
regress <- glm(yVars ~ xVars)
out[[periods+1]] <- regress
if(loud) cat("Measured coefficient:",summary(regress)$coef[,"Estimate"]["xVars"],"with P-Value:",summary(regress)$coef[,"Pr(>|t|)"]["xVars"],"\n")
if(loud) {
if(summary(regress)$coef[,"Pr(>|t|)"]["xVars"] < alpha) cat("Significant at level alpha =",alpha,"\n")
else cat("Not Significant at Level alpha =",alpha,"\n")
}
if(! latex=="")
		{
		cat("\n\\begin{figure}[h]\n",file=latex,append=TRUE)
		cat("\\centering\n",file=latex,append=TRUE)
		cat("\\title{Persistence of Good Parameter Choices for TTR:",ttr,"}\n",file=latex,append=TRUE)
		cat("\\begin{tabular}{ c c c }\n",file=latex,append=TRUE)
		cat("subperiods & coefficient & p-value \\\\ \\hline \n",file=latex,append=TRUE)
		if(summary(regress)$coef[,"Pr(>|t|)"]["xVars"] < alpha ) cat(periods," &",summary(regress)$coef[,"Estimate"]["xVars"]," &",summary(regress)$coef[,"Pr(>|t|)"]["xVars"],"*** \\\\ \n",file=latex,append=TRUE)
		else cat(periods," &",summary(regress)$coef[,"Estimate"]["xVars"]," &",summary(regress)$coef[,"Pr(>|t|)"]["xVars"]," \\\\ \n",file=latex,append=TRUE)
		cat("\\end{tabular}\n",file=latex,append=TRUE)
		if(summary(regress)$coef[,"Pr(>|t|)"]["xVars"]<alpha) cat("\\caption{P-value for Regression Coefficient significant for alpha =",alpha,"}\n",file=latex,append=TRUE)
		else cat("\\caption{Max P-value for Regression Coefficient not significant for alpha =",alpha,"}\n",file=latex,append=TRUE)
		cat("\\end{figure}\n",file=latex,append=TRUE)
		if(loud) cat("\n Results written as latex figure to file:",latex,"\n")
	}
out
}
}

