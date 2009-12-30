cReturns <-
function(x,ttr="macd4",params=0,burn=0,short=FALSE,condition=NULL,TC=0.001)

## Given data 'x' and a technical trading rule 'ttr'
## With parameters 'params'
## This function:

## 1) computes the position indicated by the ttr, i.e. 
## '1' for long, '-1' for short, and '0' for neutral
## position is forced to '0' during the 'burn' period
## position is always '1' or '0' if short is FALSE

## ## { if(ttr=="macd4")
## ## {
{

result <- indicator(x=x,ttr=ttr,params=params,burn=burn,short=short,condition=condition)

if(! is.ts(x)) if(! is.vector(x)) {
x <- as.ts(x[,c("Close")])
x <- as.vector(x) }

pos <- result[[1]]
ind <- result[[2]]
nTrades <- sum(abs(ind))

## 2) computes the 'return series'
## As the first difference of the data 'x'

returns <- diff(log(x))
returns[length(x)] <- 0

lReturns <- ifelse(pos>0,returns,NA)
sReturns <- ifelse(pos<0,returns,NA)
nReturns <- ifelse(pos==0,returns,NA)

lReturns <- deleteNA(lReturns)
sReturns <- deleteNA(sReturns)
nReturns <- deleteNA(nReturns)

cReturns <- returns*pos

adjust <- nTrades*TC
aaReturn <- (1/length(cReturns))*(sum(cReturns)-adjust)
list(cReturns,aaReturn,lReturns,sReturns,nReturns)

## ## }
}

