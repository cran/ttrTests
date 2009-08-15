defaults <-
function(ttr)

## Default parameters for given TTR
##
## params, start, nSteps, stepSize, 

{ 
if(ttr=="macd4")
	{ 
	params <- c(12,26,1,9)
	start <- c(8,5,1,4)
	stepSize <- c(3,2,1,1)
	nSteps <- c(4,3,1,2)
	list(params,start,stepSize,nSteps)
	}
else if(ttr=="none")
	{
	0
	}
else if(ttr=="hold")
	{
	0
	}
else if(ttr=="aroon")
	{ 
	params <- c(20,40,30)
	start <- c(10,20,20)
	stepSize <- c(4,10,10)
	nSteps <- c(3,3,3)
	list(params,start,stepSize,nSteps)
	}
else if(ttr=="cci")
	{
	params <- c(20,65,75)
	start <- c(15,40,30)
	stepSize <- c(3,10,15)
	nSteps <- c(3,3,3)
	list(params,start,stepSize,nSteps)
	}
else if(ttr=="cmo")
	{
	params <- c(14,9)
	start <- c(8,5)
	stepSize <- c(3,3)
	nSteps <- c(4,4)
	list(params,start,stepSize,nSteps)
	}
else if(ttr=="kst")
	{
	params <- c(10,10,15,15,10,15,25,40,9)
	start <- c(5,5,5,5,10,10,10,10,5)
	stepSize <- c(5,5,10,10,5,5,10,10,3)
	nSteps <- c(2,2,1,1,2,2,1,1,2)
	list(params,start,stepSize,nSteps)
	}
else if(ttr=="macd")
	{
	params <- c(12,26,9)
	start <- c(8,5,4)
	stepSize <- c(3,2,2)
	nSteps <- c(4,3,2)
	list(params,start,stepSize,nSteps)
	}
else if(ttr=="tdi")
	{
	params <- c(20,2)
	start <- c(8,1)
	stepSize <- c(4,1)
	nSteps <- c(5,3)
	list(params,start,stepSize,nSteps)
	}
else if(ttr=="trix")
	{
	params <- c(20,9)
	start <- c(8,5)
	stepSize <- c(6,2)
	nSteps <- c(4,4)
	list(params,start,stepSize,nSteps)
	}
else 0
}

