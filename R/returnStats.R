returnStats <-
function(x,ttr="macd4",params=0,burn=0,short=FALSE,silent=FALSE,TC=0.001,benchmark="hold")

## Computes summary statistics for the return series

{ 


if(! is.ts(x)) if(! is.vector(x)) {
y <- as.ts(x[,c("Close")])
y <- as.vector(y)
}
else y <- x

uRet1 <- cReturns(x=x,ttr=benchmark,params=params,burn=burn,short=short,TC=TC)
uRet <- uRet1[[1]]
cRet1 <- cReturns(x=x,ttr=ttr,params=params,burn=burn,short=short,TC=TC)
cRet <- cRet1[[1]]
aMean <- cRet[[2]]
uResults <- 0
cResults <- 0
pResults <- 0
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
if(!silent)
{ 
cat("Benchmark is:",benchmark,"\n")
cat("TTR is:",ttr,"\n")
cat("Summary Statistics: mean, stddev, Sharpe(0), skew, kurt, acf(1-5)\n")
cat("Benchmark return statistics:",uResults,"\n")
cat("Conditional return statistics:",cResults,"\n")
cat("Excess return:",cResults[1]-uResults[1],"\n")
cat("Excess return adjusted for trading costs:",pResults[1]-uResults[1],"\n")
}
## Output is a list

list(uResults,cResults,pResults)
}

