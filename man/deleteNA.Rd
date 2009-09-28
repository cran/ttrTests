\name{deleteNA}
\Rdversion{1.2}
\alias{deleteNA}
\title{
Cleans data that contains NA entries
}
\description{
Used in this package to create data for long, short, or neutral
days only.  Could have many other applications.  Probably exists
in other packages with more support.
}
\usage{
deleteNA(x)
}
\arguments{
  \item{x}{
A univariate series or vector
}
}
\details{
As long as x can be indexed, it should work fine.
}
\value{
Outputs is a vector of length <= length(x) with NA entries deleted
}

\author{
David St John
}

\examples{

x <- c(1,NA,2,3,NA,4)
deleteNA(x)

t <- c(1,2,3)
x <- c(t[1],t[2],t[3],t[4],t[5])
deleteNA(x)

}
