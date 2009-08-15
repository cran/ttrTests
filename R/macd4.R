macd4 <-
function(x,params=c(12,26,1,9))

## Computes the difference between the
## MACD line and the MACD Signal Line
## Where the MACD line could be a faster EMA
## of the traditional MACD line, hence the 4th parameter

{ 
d <- emaTA(x,params[1])-emaTA(x,params[2])
emaTA(d,params[3])-emaTA(d,params[4])
}

