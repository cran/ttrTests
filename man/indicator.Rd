\name{indicator}
\Rdversion{1.5}
\alias{indicator}
\title{
The canonical position / indicator functions
}
\description{
Computes the "position" function corresponding to a given
 univariate series and TTR.  Position is 1 for long, -1 for
 short, and 0 for neutral.  The indicator function is just
 the difference of the position from one period to the next,
 i.e. 1 for buy one unit, -2 for sell 2 units, etc.
}
\usage{
indicator(x, ttr = "macd4", params = 0, burn = 0, short = FALSE,
condition = NULL)
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
  \item{condition}{
An extra opportunity to restrict the TTR so that position is forced
to 0 under some condition.  Must be a binary string of the same
length as the data 'x'.  See 'position' for more details.
}
}
\value{
Output is a list contatining the position and indicator (resp.)
}
\references{
William Brock, Josef Lakonishok, and Blake LeBaron. Simple technical
 trading rules and the stochastic properties of stock returns. The Journal
 of Finance, 47(5):1731-1764, 1992.
}
\author{
David St John
}

\examples{
## How many days does the TTR 'macd4' indicate a long position?
## How many trades are indicated?

spData <- as.vector(getYahooData("SPY",start="20060101",end="20081231")[,"Close"])
position <- indicator(spData,short=FALSE)

length(spData)
sum(position[[1]])
sum(abs(position[[2]]))

}
\keyword{ ts }
