\name{ttrTests-package}
\Rdversion{1.7}
\alias{ttrTests-package}
\alias{ttrTests}
\docType{package}
\title{
A series of tests for the efficacy of a technical trading rule (TTR)
}
\description{
Contains five major tests supported by other functions: Did the TTR strategy outperform a benchmark in the past data?  Is the excess return significant, using bootstrapping to construct a confidence interval?  Is the excess return explained by data snooping? Is the 'good' choice of parameters robust across sub-samples?  Is this robustness significant, using bootstrapping to construct a confidence interval?
}
\details{
\tabular{ll}{
Package: \tab ttrTests\cr
Type: \tab Package\cr
Version: \tab 1.7\cr
Date: \tab 2011-08-15\cr
License: \tab GPL\cr
LazyLoad: \tab yes\cr
}
First, data should be a univariate time series 'x'.  Attempts have been
made to accomodate 'High-Low-Close' data by coercing it into a univariate 'close'
series, but stability of this behavior is not gauranteed.

All computations basically take place on the return series,
 diff(log(x))
So it is assumed that the data x is such that this return series
is stationary, a common assumption for financial data.

Second, a TTR should be any function whose input is the data 'x'and a list of parameters,
and whose output is a ternary trading rule, i.e. 1 for 'long', -1 for
'short', and 0 for neither.  Built in TTR functions have this behavior,
and any user defined function with this behavior is accepted.

USER PROVIDED TTR FUNCTIONS MUST HAVE THE FOLLOWING FORM:

ttr <- function ( x , params , burn , short)

Where x is the data, params is a numeric vector of parameters, burn is an integer
describing the 'burn' period, and short is logical.  The function need not
use these, but they must accept them.

PLEASE SEE THE EXAMPLES IF YOU ARE GETTING ERRORS!

Now, the major functions of the package:

1) returnStats(x,ttr) compares the performance of the TTR with some
benchmark.  A ttr that had good past performance might merit further study.

2) nullModel(x,ttr) constructs a confidence interval for this performance
and gives a p-value for the excess return observed in (1).  If the TTR fails 
this test (i.e. p-value too high), it means that the good performance was 
just as likely due to luck in how the data was constructed.

3) dataSnoop(x,ttr) constructs a p-value for the 'best'
choice of parameters within a given domain (preferably large). If a TTR
fails this test, it means that good performance was just as likely due
to luck in picking a choice of parameters.

4) subperiods(x,ttr) asks whether or not good choices of parameters
were robust across different time periods.  This is a new type of 'out-of-
sample' confirmation introuduced by the author in 2010.

5) paramPersist(x,ttr) tests if the persistence measure from subperiods()
is statistically significant.  It constructs an asymptotically valid
p-value for the observed persistence, assuming that the true correlation is zero.
If this test fails (i.e. p-value too high), it means that regardless of
how significant the performance of a TTR was, no choice of parameters 
performed consistently well over time.

Version >= 1.1 allows the results of these tests to be written
to a file in LaTeX code as a figure.  It also includes two tests for data
snooping, White's Reality Check and Hansen's test for Superior Predictive
Ability, the later not included in the first version.  

Version >= 1.2 fixes some minor glitches in 1.1.

Version >= 1.3 uses a different algorithm to compute the statistical
significance of the observed correlation from test (4).  Some new functions
have been added to accomplish this.  An option for the position/indicator
functions was added to increase flexibility.  Other minor changes here and there.

Version >= 1.4 fixes some minor glitches in 1.3

Version >= 1.5 fixes some major glitches in 1.4, now
producing correct results for some tests that were
behaving questionably, and with much more accurate
p-values based on bootstrapping.

Version >= 1.6 allows the user to pass the 'restrict' option
to the dataSnoop test.  Absence of this feature made it impossible for
the user to use the 'restrict' option for a user defined ttr.  Also
a small glitch in cating the results of dataSnoop was fixed, and a
relatively serious error in paramStats was corrected to allow
it to run with rules using fewer than 4 parameters.

Version >= 1.7 has new examples, a new data set, and minor 
changes to the documentation.

}
\author{ 
David St John

Maintainer: David St John <dstjohn@math.uic.edu>
}
\note{
EXTREMELY IMPORTANT NOTE: The functions in this package evaluate past 
performance only.  No warranty is made that the results of these tests should, 
or even can, be used to inform business decisions or make predictions of 
future events.  

The author does not make any claim that any results will predict future 
performance.  No such prediction is made directly or implied by the outputs of 
these function, and any attempt to use these function for such prediction is done 
solely at the risk of the end user. 

Please note that Dr. Halbert White owns U.S. patents 6,088,676 and 
5,893,069, pertaining to the RC option for the function dataSnoop().  
These routines are licensed to the author for use solely
for the purposes of non-commercial academic research and study.
License rights as granted in the GPL are therefore limited to
uses that are legal under national and international patent law.
Future contributers, as defined in the GPL, assume full responsibility
for their modifications and implementations. 

If there is any doubt, please remove code that implements the reality check
and enjoy the rest of the package.  There should be very minimal loss
of functionality, if any, since a second data snooping test is included.
}
\references{
[1]Sidney S. Alexander. Price movements in speculative markets: Trends
or random walks. Industrial Managment Review, 2(2):7-26, 1961.

[2] David R. Aronson. Evindence Based Technical Analysis. John Wiley
and Sons, 2007.

[3] William Brock, Josef Lakonishok, and Blake LeBaron. Simple technical
trading rules and the stochastic properties of stock returns. The Journal
of Finance, 47(5):1731-1764, 1992.

[4] David P. Brown and Robert H. Jennings. On technical analysis. The
Review of Financial Studies, 2(4):527-551, 1989.

[5] B. Efron. Bootstrap methods: Another look at the jackknife. The Annals
of Statistics, 7(1):1-26, 1979.

[6] Eugene F. Fama and Marshall E. Blume. Filter rules and stock market
trading. The Journal of Business, 39(1):226-241, 1966.

[7] John Hull. Futures, Options, and Other Derivatives. Prentice Hall, 2006.

[8] Kenneth A. Kavajecz and Elizabeth R. Odders-White. Technical analysis
and liquidity provision. The Review of Financial Studies, 17(4):1043-
1071, 2004.

[9] Richard M. Levich and Lee R. Thomas. The significance of technical
trading rule profits in the foreign exchance markets: A bootstrap approach.
1991.

[10] Christopher Neely, Paul Weller, and Rob Dittmar. Is technical analysis
in the foreign exchange market profitable? a genetic programming approach.
The Journal of Financial and Quantitative Analysis, 32(4):405-
426, 1997.

[11] Gary Norden. Technical Analysis and the Active Trader. McGraw - Hill,
2006.

[12] Min Qi and Yangru Wu. Technical trading rule profitability, data snooping,
and reality check: Evidence from the foreign exchange market. Journal
of Money, Credit, and Banking, 38(8):2135-2158, 2006.

[13] Mark J. Ready. Profits from technical trading rules. Financial Managment,
31(3):43-61, 2002.

[14] Halbert White.  A Reality Check for Data Snooping.  Econometrica,
68, 2000, pp1097-1126.

[15] Ryan Sullivan, Allan Timmermann, and Halbert White. Data snooping,
technical trading rule performance, and the bootstrap. The Journal of
Finance, 54(5):1647-1691, 1999.

[16] Jack L. Treynor and Robert Ferguson. In defense of technical analysis.
The Journal of Finance, 40(3):757-773, 1985.

[17] David St John.  Technical Analysis based on Moving Average Convergence
and Divergence.  Phd thesis, University of Illinois, Chicago.  2010.
http://math.uic.edu/~dstjohn/thesis.pdf
}
\keyword{ package }
\examples{

data(spData)

## First and foremost your ttr is a function.
## Call your ttr anything you like and use any defaults you like
## (they're ultimately irrelevant) but you must use
## 'x', 'params', 'burn', and 'short' exactly!

myTTR <- function(x, params=c(3,7,5), burn=0, short=TRUE)
{

crystalBall <- rexp(length(x),rate=1/params[3])

## note that there are no 'fixed' values used
## the output must depend explicitly on the input

position <- ifelse(crystalBall<=params[1],-1,0) + 
ifelse(crystalBall>=params[2],1,0)

## the output will be a vector with values {1,0,-1}
## interpreted as long, neutral, short, respectively
## it should have the same length as the data

return(position)
}

## I know it's quirky, but when using your ttr, you have to include 
## values for params (or start, nSteps, stepSize where needed), 
## burn, and short for all of the following calls.  No defaults 
## will be used, and you will get an error if you exclude them

stat <- returnStats(spData, ttr=myTTR, params=c(3,7,5),burn=0,short=TRUE)

## Witness the amazing predictive power of my crystalBall!
## Go ahead and run returnStats over and over!
## We see a positive excess return every time!
## Man I am such a good fund manager

null <- nullModel(spData,ttr=myTTR,params=c(3,7,5),burn=0,short=TRUE,bSamples=5)
spa <- dataSnoop(spData,ttr=myTTR,start=c(3,7,5),nSteps=c(3,3,3), 
stepSize=c(1,1,1),burn=0,short=TRUE,restrict=TRUE,bSamples=3)
pp <- paramPersist(spData,ttr=myTTR,start=c(3,7,5),nSteps=c(3,3,3), 
stepSize=c(1,1,1),burn=0,short=TRUE,restrict=TRUE,bSamples=3)

## Sorry, for those who missed the joke, myTTR decides to be long, short, or
## neutral completely at random according to a disribution specified by
## the input parameters.  Since the benchmark returns were negative, and
## the 'strategy' is short or neutral some of the time, it outperforms
## as expected, but with no real predictive power

###########################################################

## Here's an example of another user defined ttr that will work:

twoSMA<-function(x,params=c(20,30),burn = 0, short = FALSE)
{
mac2<-SMA(x,params[2])-SMA(x,params[1]) 
mac2[is.na(mac2)]<-0
sig<-ifelse(mac2>0,1,0)
return(sig)
}

## This one is based on some actual technical analysis...
## not that that guarantees it will do much better than my crystalBall
}
