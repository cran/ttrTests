returnStats <-
function(x,ttr="macd4",params=0,burn=0,short=FALSE,condition=NULL,silent=FALSE,TC=0.001,benchmark="hold",latex="")

## Computes summary statistics for the return series

{ 


if(! is.ts(x)) if(! is.vector(x)) {
y <- as.ts(x[,c("Close")])
y <- as.vector(y)
}
else y <- x

uRet1 <- cReturns(x=x,ttr=benchmark,params=params,burn=burn,short=short,condition=condition,TC=TC)
uRet <- uRet1[[1]]
cRet1 <- cReturns(x=x,ttr=ttr,params=params,burn=burn,short=short,condition=condition,TC=TC)
cRet <- cRet1[[1]]
aMean <- cRet[[2]]
uResults <- 0
cResults <- 0
pResults <- 0
lResults <- 0
sResults <- 0
nResults <- 0
eResults <- 0

uResults[1] <- mean(uRet)
uResults[2] <- sqrt(var(uRet))
uResults[3] <- uResults[1]/uResults[2]
uResults[4] <- skewness(uRet)
uResults[5] <- kurtosis(uRet)
a <- acf(uRet,5,plot=FALSE)
uResults[6] <- a$acf[2]
uResults[7] <- a$acf[3]
uResults[8] <- a$acf[4]
uResults[9] <- a$acf[5]
uResults[10] <- a$acf[6]

## Computes summary statistics for the conditional
## Returns series, as returned by 'cReturns'

cResults[1] <- mean(cRet)
cResults[2] <- sqrt(var(cRet))
cResults[3] <- cResults[1]/cResults[2]
cResults[4] <- skewness(cRet)
cResults[5] <- kurtosis(cRet)
b <- acf(cRet,5,plot=FALSE)
cResults[6] <- b$acf[2]
cResults[7] <- b$acf[3]
cResults[8] <- b$acf[4]
cResults[9] <- b$acf[5]
cResults[10] <- b$acf[6]

pResults[1] <- cRet1[[2]]
pResults[2] <- length(cRet)

eResults[1] <- mean(cRet-uRet)
eResults[2] <- sqrt(var(cRet-uRet))
eResults[3] <- eResults[1]/eResults[2]
eResults[4] <- skewness(cRet-uRet)
eResults[5] <- kurtosis(cRet-uRet)

lResults[1] <- mean(cRet1[[3]])
lResults[2] <- sqrt(var(cRet1[[3]]))
lResults[3] <- lResults[1]/lResults[2]
lResults[4] <- skewness(cRet1[[3]])
lResults[5] <- kurtosis(cRet1[[3]])

sResults[1] <- mean(cRet1[[4]])
sResults[2] <- sqrt(var(cRet1[[4]]))
sResults[3] <- sResults[1]/sResults[2]
sResults[4] <- skewness(cRet1[[4]])
sResults[5] <- kurtosis(cRet1[[4]])

nResults[1] <- mean(cRet1[[5]])
nResults[2] <- sqrt(var(cRet1[[5]]))
nResults[3] <- nResults[1]/nResults[2]
nResults[4] <- skewness(cRet1[[5]])
nResults[5] <- kurtosis(cRet1[[5]])

if(!silent)
{ 
cat("Benchmark is:",ifelse(is.character(benchmark),benchmark,"from function"),"\n")
cat("TTR is:",ifelse(is.character(ttr),ttr,"from function"),"\n")
cat("Summary Statistics: n, mean, stddev, Sharpe(0), skew, kurt, acf(1-5)\n")
cat("\n**************************************\n\n")
cat("Benchmark return statistics:",length(uRet1[[1]]),uResults,"\n")
cat("Conditional return statistics:",length(cRet1[[1]]),cResults,"\n")
cat("\n**************************************\n\n")
cat("Long return statistics:",length(cRet1[[3]]),lResults,"\n")
cat("Short return statistics:",length(cRet1[[4]]),sResults,"\n")
cat("Neutral return statistics:",length(cRet1[[5]]),nResults,"\n")
cat("\n**************************************\n\n")
cat("Excess return statistics:",length(uRet1[[1]]),eResults,"\n")
cat("Excess return adjusted for trading costs:",pResults[1]-uResults[1],"\n")
if(! latex=="") cat("\n Results written to file:",latex,"as latex figure\n")
}

if(! latex=="")
{
cat("\n\\begin{table}[htp]\n",file=latex,append=TRUE)
cat("\\centering\n",file=latex,append=TRUE)
cat("\\begin{tabular}{ r | c c c c c }\n",file=latex,append=TRUE)
cat(" & \\# observations & mean & std dev & skew & kurtosis \\\\ \\hline \n",file=latex,append=TRUE)
cat("Benchmark &",length(uRet1[[1]])," &",uResults[1]," &",uResults[2]," &",uResults[4]," &",uResults[5]," \\\\ \n",file=latex,append=TRUE)
cat("TTR &",length(cRet1[[1]])," &",cResults[1]," &",cResults[2]," &",cResults[4]," &",cResults[5]," \\\\ \n",file=latex,append=TRUE)
cat("Long &",length(cRet1[[3]])," &",lResults[1]," &",lResults[2]," &",lResults[4]," &",lResults[5]," \\\\ \n",file=latex,append=TRUE)
if(short) cat("Short &",length(cRet1[[4]])," &",sResults[1]," &",sResults[2]," &",sResults[4]," &",sResults[5]," \\\\ \n",file=latex,append=TRUE)
else cat("Neutral &",length(cRet1[[5]])," &",nResults[1]," &",nResults[2]," &",nResults[4]," &",nResults[5]," \\\\ \n",file=latex,append=TRUE)
cat(" & & & & & \\\\ \n",file=latex,append=TRUE)
cat("Excess &",length(cRet1[[5]])," &",eResults[1]," &",eResults[2]," &",eResults[4]," &",eResults[5]," \\\\ \n",file=latex,append=TRUE)
cat("\\end{tabular}\n",file=latex,append=TRUE)
cat("\\caption{Summary Statistics for TTR:",ttr,"versus Benchmark:",benchmark,"}\n",file=latex,append=TRUE)
cat("\\end{table}\n",file=latex,append=TRUE)
}

## Output is a list

list(uResults,cResults,pResults,eResults,lResults,sResults,nResults)
}

