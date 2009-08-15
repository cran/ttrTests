\name{cReturns}
\Rdversion{1.1}
\alias{cReturns}
\title{
Return series conditioned on a TTR
}
\description{
For a series x(t) and a given TTR, computes
 - the return series r(t) = ln( x(t+1) / x(t) )
 - the conditional return series, r(t)*s(t), where
   s(t) is the position indicated by the TTR
   i.e. s(t) = 1 for long, -1 for short, 0 for neutral
}
\usage{
cReturns(x, ttr = "macd4", params = 0, burn = 0, short = FALSE, TC = 0.001)
}
\arguments{
  \item{x}{
The data set
}
  \item{ttr}{
The TTR to be used.  Can be a character string for built-in TTRs,
 or a user defined function whose output is a position series s(t).
 See 'defaults' for a list of built-in TTRs.
}
  \item{params}{
Used to compute the TTR.  Will be passed to a user defined function.
 Hence a user defined function should have at least 2 inputs,
 the data set and a vector or list of parameters
}
  \item{burn}{
When computing the position function s(t), values for t < burn
 will be forced to 0, i.e. no position held during the 'burn' period
}
  \item{short}{
Logical.  If false the position function s(t) will be forced to
 0 when it would otherwise be -1, i.e. no short selling
}

  \item{TC}{
Trading cost, as a percentage.  Used to compute an adjusted average
 return.
}
}
\value{
\item{cReturns}{The conditional returns series}
\item{aReturns}{The mean one-period return, adjusted for trading costs}
}
\references{
William Brock, Josef Lakonishok, and Blake LeBaron. Simple technical
 trading rules and the stochastic properties of stock returns. The Journal
 of Finance, 47(5):1731-1764, 1992.
}
\author{
David St John
}
\note{
EXTREMELY IMPORTANT NOTE: The functions in this package evaluate past 
performance only.  No warranty is made that the results of these tests should, 
or even can, be used to inform business decisions or make predictions of 
future events.  

The author does not make any claim that any results will predict future 
performance.  No such prediction is made, directly or implied, by the outputs of 
these function, and any attempt to use these function for such prediction is done 
solely at the risk of the end user. 
}
\examples{

## Is the mean conditional return higher than the mean unconditional return?

spData <- as.vector(getYahooData("SPY",start="20060101",end="20081231")[,"Close"])
mean(diff(log(spData)))

cr <- cReturns(spData)
mean(cr)

}

\keyword{ univar }
