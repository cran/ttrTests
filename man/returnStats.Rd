\name{returnStats}
\Rdversion{1.2}
\alias{returnStats}
\title{
Conditional statistics for given data and TTR
}
\description{
The first of four main backtests for a technical analysis strategy.
Gives returns conditioned on the TTR above and beyond some benchmark.
}
\usage{
returnStats(x, ttr = "macd4", params = 0, burn = 0, short = FALSE, 
silent = FALSE, TC = 0.001, benchmark = "hold", latex="")
}
\arguments{
  \item{x}{
A univariate series
}
  \item{ttr}{
The TTR to be used.  Can be a character string for built-in TTRs,
 or a user defined function whose output is a position series s(t).
 See 'defaults' for a list of built-in TTRs.
}
  \item{params}{
A list of parameters to use for the TTR
}
  \item{burn}{
When computing the position function s(t), values for t < burn
 will be forced to 0, i.e. no position held during the 'burn' period
}
  \item{short}{
Logical.  If false the position function s(t) will be forced to
 0 when it would otherwise be -1, i.e. no short selling
}
  \item{silent}{
Logical.  If TRUE, supresses output from subroutines
}
  \item{TC}{
Percentage used to compute returns adjusted for trading costs.
}
  \item{benchmark}{
When computing 'excess' returns, all functions in this package
 subtract the conditional returns based on a given "ttr" from
 the "benchmark" returns.  Two different TTRs can be compared
 this way if desired.
}
\item{latex}{
Full path name for a writable file.  The laTeX code that generates a
figure with a summary of the output will be appended to file.
}
}
\details{
"excess return" means the conditional return minus the benchmark return 
}
\value{
uResults and cResults are each of lenght 10 and include:
mean, variance, sharp ratio, skew, kurtosis, and first 5 autocorrelation coefficients

The third item of output is the excess return adjusted for trading costs.
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

spData <- as.vector(getYahooData("SPY",start="20060101",end="20081231")[,"Close"])
stat <- returnStats(spData)

}

\keyword{ univar }
