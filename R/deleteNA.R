deleteNA <- function(x)
{
## deletes NA entries of vector x

count <- 1
out <- 0
for(k in 1:length(x)) 
	{
	if(! is.na(x[k])) 
		{
		out[count] <- x[k]
		count <- count + 1
		}
	}
out
}
