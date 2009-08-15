\name{macd4}
\Rdversion{1.1}
\alias{macd4}
\title{
MACD oscillator
}
\description{
A new way of computing the MACD oscillator where a fourth
 parameter is allowed to vary.  Typically this parameter
 is fixed at 1 arbitrarily.
}
\usage{
macd4(x, params = c(12, 26, 1, 9))
}
\arguments{
  \item{x}{
A univariate series
}
  \item{params}{
First parameter - the 'fast' average parameter
Second parameter - the 'slow' average parameter
Third parameter - the new parameter, indicating a 'fast'
 averaging of the MACD line instead of the typical choice
 of the MACD line itself
Fourth parameter - the 'slow' averaging for the MACD
 signal line
}
}
\details{
The standard interpretation of the MACD is a 'crossover'
 rule, i.e. when the MACD oscillator crosses from positive
 to negative, that's a sell signal, and vice versa.
}
\value{
A univariate series computed as the difference between the 
 MACD line and the MACD signal line.  "macd4" replaces the
 MACD line with a fast average of the MACD line.
}
\references{
http://cran.r-project.org/web/packages/TTR/
http://en.wikipedia.org/wiki/MACD
}
\author{
David St John
}

\examples{

spData <- as.vector(getYahooData("SPY",start="20060101",end="20081231")[,"Close"])
oscillator <- macd4(spData)
plot(oscillator)

}
\keyword{ ts }
