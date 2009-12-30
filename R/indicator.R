indicator <-
function(x,ttr="macd4",params=0,burn=0,short=FALSE,condition=NULL)

## Returns a series 'y' for which:
##
## y(t) = 1 if TTR indicates initiating a long position
## y(t) = -1 if TTR indicates closing a long position
## y(t) = 2 if TTR indicates reversing a short position to a long one
## y(t) = -2 if TTR indicates reversing a long position to a short one
## y(t) = 0 otherwise
##
## computed as the first difference of the 'position' function

{ pos <- position(x=x,ttr=ttr,params=params,burn=burn,short=short,condition=condition)
t <- length(pos)
ind <- 0
for(k in 1:(t-1)) ind[t-k] <- pos[t-k+1]-pos[t-k]
list(pos,ind)
}

