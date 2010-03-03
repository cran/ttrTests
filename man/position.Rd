\name{position}
\Rdversion{1.5}
\alias{position}
\title{
Position Function
}
\description{
Obsoleted by 'indicator' function, but called from there so
must be included in the package.
}
\usage{
position(x, ttr = "macd4", params = 0, burn = 0, short = FALSE,
condition = NULL)
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
length as the data 'x'.  See notes for more details.
}
}
\value{
Value returned to function 'indicator' is returned as 'pos'
}
\references{
}
\author{
David St John
}
\note{
The user-supplied 'Condition' series should be zero during any time 
period when there should not be any position, and one otherwise.  
If none is given, a string of all ones is default.  Such a restriction 
could be no positions held during certain times or any other external
condition.
}
\examples{

}

\keyword{ ts }
