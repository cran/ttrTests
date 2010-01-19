\name{paramStats}
\Rdversion{1.4}
\alias{paramStats}
\title{
Analyzes a domain of parameter choices to pick the best one
}
\description{
This function calls the 'returnStats' function with every choice
of parameters in a given domain and records the result.  This allows
'good' choices for parameters to be identified.
}
\usage{
paramStats(x, ttr = "macd4", start = 0, nSteps = 0, stepSize = 0, 
restrict = FALSE, burn = 0, short = FALSE, condition = NULL, silent = TRUE, TC = 0.001, loud = TRUE, 
plot = TRUE, alpha = 0.025, begin = 1, percent = 1, file = "", benchmark = "hold")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A univariate series
}
  \item{ttr}{
The TTR to be used.  Can be a character string for built-in TTRs,
 or a user defined function whose output is a position series s(t).
 See 'defaults' for a list of built-in TTRs.
}
  \item{start}{
Initial values for parameters
}
  \item{nSteps}{
How many parameter choices to use for each parameter
}
  \item{stepSize}{
The difference between successive choices of a parameter.
}
  \item{restrict}{
If restricted = TRUE, this will force the second parameter
(and 4th, if applicable) to be strictly greater than the first (3rd, resp.)
This is sensible if the pairs are moving average parameters.
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
Logical.  If TRUE, supresses output from subroutines
}
  \item{TC}{
Percentage used to compute returns adjusted for trading costs.
}
  \item{loud}{
Logical.  If FALSE, supresses output from the main function(s).
}
  \item{plot}{
Logical.  If FALSE, supresses plot of results by parameter choice.
}
  \item{alpha}{
Confidence level for 2-sided hypothesis testing
}
  \item{begin}{
The starting index of the data.  The function assumes that the
 user wants a subset of the data, where the default subset
 is the entire data
}
  \item{percent}{
How much of the original data to use (default 100%)
}
  \item{file}{
The full writable path string for a file to which output
 will be appended.  Ideal for reviewing results.
}
  \item{benchmark}{
When computing 'excess' returns, all functions in this package
 subtract the conditional returns based on a given "ttr" from
 the "benchmark" returns.  Two different TTRs can be compared
 this way if desired.
}
}
\details{
This function will only allow a list of parameters with length
at most 4.  If a TTR requires more than 4 parameters, it is not
supported here (yet).  

If a TTR uses 3 or 4 parameters, it may be 'restricted'.  In this case
it is assumed that the first 2 parameters are related, and forces the 
second parameter to be strictly greater than the first.  The 3rd and 4th 
parameters are treated similarly.  Built in TTRs 'macd' and 'macd4' are
restricted.  Users may wish to 'restrict' user defined TTR, if appropriate.

Example: (4 parameters)
start = c(2,4,3,6)
nSteps = c(3,5,1,2)
stepSize = c(4,5,2,3)

The values of the first parameter would be (2,6,10)
The values of the second parameter would be (4,9,14,19,24) PLUS THE FIRST!!!
The values of the third parameter would be (3)
The values of the fourth parameter would be (6,9) PLUS THE THIRD!!!

So there would be 30 parameterizations in this domain.  They would be:
(2,6,3,6) , (2,11,3,6) , ... (notice the second parameter is NOT 4,9,...)
(6,10,3,6) , (6,15,3,6) , ... (it is forced to be strictly greater)
(10,14,3,6) , (10,19,3,6) , ... (by adding the first parameter)

If restrict = FALSE, no such adjustment will be made to the choice of
domain for parameters.
}
\value{
Output is a list of vectors.  When read as a matrix, each row containts:
 The conditional mean return
 The z-socre of this amongst all parameter choices
 The adjusted return (after trading costs)
 The Sharpe ratio
 (The best choice of parameters, as a vector.  Not part of the overall matrix)
 The parameters
 
For a unique choice of parameters
}
\references{
Ryan Sullivan, Allan Timmermann, and Halbert White. Data snooping,
technical trading rule performance, and the bootstrap. The Journal of
Finance, 54(5):1647-1691, 1999.
}
\author{
David St John
}
\note{
Of all parameter choices, the 'best' is used for the function 'dataSnoop'

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
ps <- paramStats(spData)

max(ps[[1]])
max(ps[[2]])

}

\keyword{ design }
