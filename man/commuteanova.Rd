\name{commuteanova}
\alias{commuteanova}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Find out if times differ significantly
}
\description{Takes a data frame of times in minutes, as integers, which have a 
variable, Day, defining their factor group, and returns the conclusion of an ANOVA.
}
\usage{
commuteanova(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Data frame which must contain a column, Day, which has the character string of
  the day the time is entered corresponding to a required column Total. Total contains an
  integer value in minutes for a total commute time.
}
}
\details{
Perhaps this can be generalized to other time collection purposes.
}
\value{
The returned message advised whether the times are significantly different by Day, and
if not it will still advise which day the shortest average time, or commute.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
David Burton
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x) 
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
