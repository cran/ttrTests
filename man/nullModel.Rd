\name{nullModel}
\Rdversion{1.3}
\alias{nullModel}
\title{
Hypothesis test for efficacy of TTR
}
\description{
One of the four main functions in the package.  Creates a confidence interval
 for the observed excess return via bootstrap resampling.  Can write summary
of output to a file as a latex figure.
}
\usage{
nullModel(x, model = "stationaryBootstrap", userParams = 4, nSamples = 100, 
ttr = "macd4", params = 0, burn = 0, short = FALSE, condition = NULL, silent = TRUE, loud = TRUE, 
alpha = 0.025, TC = 0.001, benchmark = "hold", latex = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A univariate series
}
  \item{model}{
Passed to the function 'generateSample'
}
  \item{userParams}{
Passed to the function 'generateSample'
}
  \item{nSamples}{
How many bootstrapped samples to generate
}
  \item{ttr}{
Could be a character string for a built in TTR, or a user
 defined function.  User defined functions must take
 a univarate series and a list/vector of inputs and must
 output a series with values 1,0,-1 only
}
  \item{params}{
Used to calculate the position based on the given TTR
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
  \item{silent}{
Logical.  If TRUE, output from subroutines will be supressed.
}
  \item{loud}{
Logical.  If FALSE, output from the main function will be supressed.
}
  \item{alpha}{
Confidence interval for 2-sided hypothesis test
}
  \item{TC}{
Percentage trading costs.  Used to adjust return statistics.
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
\value{
\item{ CR }{A vector of conditional returns of length 'nSamples'}
\item{ AR }{CR, adjusted for trading costs}
\item{ SR }{Sharp ratio for these returns using r_f = 0}
\item{ Z }{Z-score for observed excess return, using mean
 and standard deviation of CR for a confidence interval}
\item{ P }{P-value associated with observed Z-score}
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
A significant P-value is enough to reject the null hypothesis that the TTR
had results due solely to randomness in the data.  However, there are
several other null hypotheses to explain good results, chiefly the data
snooping hypothesis, addressed using the function 'realityCheck'.

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
null <- nullModel(spData,nSamples=5)

}

\keyword{ design }
