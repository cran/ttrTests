position <-
function(x,ttr="macd4",params=0,burn=0,short=FALSE,condition=NULL)

## p(t) = 1 if TTR indicates a long position
## p(t) = 0 if TTR indicates no position
## p(t) = -1 if TTR indicates a short position and short=TRUE
##
## if short=FALSE, then p(t) = 0 when TTR indicates a short position
##
{
if(is.null(condition)) condition <- rep(1,length(x))

if(length(condition)!=length(x)) stop("Mismatch: length of CONDITION must be the same as length of data")

if(is.character(ttr))

{ 

if(params[1]==0) params <- defaults(ttr)[[1]]

if(ttr=="macd4")
	{ s <- macd4(x,params[1:4])
	pos <- (ifelse(s>0,1,0)+ifelse(s<0,1,0)*ifelse(short,-1,0))
	pos[1:(burn+1)] <- 0
	pos*condition
	}
else if(ttr=="none")
	{
	pos <- 0
	pos[1:length(x)] <- 0
	pos*condition
	}
else if(ttr=="hold")
	{
	pos <- 0
	pos[1:length(x)] <- 1
	pos*condition
	}
else if(ttr=="aroon")
	{ 
	s <- aroon(x,n=params[1])
	s <- ifelse(s[,3]>params[2],1,0)*ifelse(s[,1]>params[3],1,0) + ifelse(s[,3] < -params[2],1,0)*ifelse(s[,2]>params[3],1,0)*ifelse(short,-1,0)
	ifelse(is.na(s),0,s)*condition
	}
else if(ttr=="cci")
	{
	s <- CCI(x,n=params[1],c=1/params[2])
	s <- ifelse(s>params[3],1,0) + ifelse(s < -params[3],1,0)*ifelse(short,-1,0)
	ifelse(is.na(s),0,s)*condition
	}
else if(ttr=="cmo")
	{
	s <- as.ts(CMO(x,n=params[1]))
	sig <- emaTA(s,params[2])
	s <- ifelse(s>sig,1,0) + ifelse(s<sig,1,0)*ifelse(short,-1,0)
	ifelse(is.na(s),0,s)*condition
	}
else if(ttr=="kst")
	{
	ind <- KST(x,n=c(params[1],params[2],params[3],params[4]),nROC=c(params[5],params[6],params[7],params[8]),nSig=params[9])
	s <- 0
	for(k in 1:length(x)) s[k] <- ind[k] - ind[length(x)+k]
	sig <- ifelse(s>0,1,0) + ifelse(s<0,1,0)*ifelse(short,-1,0)
	ifelse(is.na(sig),0,sig)*condition
	}
else if(ttr=="macd")
	{
	s <- as.ts(MACD(x,nFast=params[1],nSlow=params[2],nSig=params[3]))
	s <- ifelse(s[,1]>s[,2],1,0) + ifelse(s[,1]<s[,2],1,0)*ifelse(short,-1,0)
	ifelse(is.na(s),0,s)*condition
	}
else if(ttr=="tdi")
	{
	ind <- TDI(x,n=params[1],multiple=params[2])
	tdi <- ind[,1]
	di <- ind[,2]
	s <- ifelse(tdi>0,1,0)*(ifelse(di>0,1,0) + ifelse(di<0,1,0)*ifelse(short,-1,0))
	ifelse(is.na(s),0,s)*condition
	}
else if(ttr=="trix")
	{
	ind <- TRIX(x,n=params[1],nSig=params[2])
	lin <- ind[,1]
	sig <- ind[,2]
	s <- ifelse(lin>sig,1,0)+ifelse(lin<sig,1,0)*ifelse(short,-1,0)
	ifelse(is.na(s),0,s)*condition
	}
}
else if(is.function(ttr))
	{
	s <- ttr(x=x,params=params,burn=burn,short=short)
	check <- ifelse(abs(s)>1,1,0)
	s2 <- ifelse(s==floor(s),0,1)
	ch <- sum(check) + sum(s2)
	if(ch>0) {
		cat("Error: given function does not have ternary output\n")
		NA
		}
	else s*condition
	}
else 	{ 
	cat("TTR not found\n")
	NA
	}
}

