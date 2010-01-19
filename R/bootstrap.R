bootstrap <-
function(x,model="bootstrap",userParams=4)

## Creates sample data of the same length as 'x'
## From a given model, where parameters
## Are chosen to fit observed 'x'

## Model must be given as a function
## or, if no function is given, 'bootstrap'
## or 'stationary bootstrap' can be used

{
if(is.function(model))
{
	model(x,userParams)
}
else if(model=="bootstrap")

## Bootstrap Model:
## r(t) = r* with probability 1/n
## for every r* observed from 'x'
## selected with replacement

	{ 
	n <- length(x)
	sample <- 0
	sam <- floor(runif(n,max=n))
	for(k in 1:n) 
		{ sample[k] <- x[sam[k]+1]
	}
	sample
  }
else if(model=="stationaryBootstrap")

## Stationary Block Bootstrap Model:
## r(k),...,r(k+l) block selected
## l geometric {0,1,...} with default mean 'userparams' 
## k selected with probability 1/(n-l)
## selected with replacement

	{
	n <- length(x)
	sample <- 0
	gParam <- as.double(1/userParams)
	sam <- rgeom(floor(n/2),gParam)
	while(sum(sam)<n) sam <- rgeom(floor(n/2),gParam)
	sample[1] <- x[1+runif(1,max=n)]
	count <- 1
	done <- 1
	while(done<n) 
		{
		blockStart <- floor(runif(1,max=(n-sam[count])))  	
		blockSize <- min(sam[count],(n-done)-1)		
		for(k in 1:(blockSize+1)) 
			{
			sample[done+1] <- x[blockStart+k]
			done <- done+1
		}
	count <- count+1
	}
	sample
  }
else 
	{ cat("Model not found, using default: bootstrap\n")
	generateSample(x)
  }

}

