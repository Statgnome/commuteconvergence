\name{read_sheet}
\alias{read_sheet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Read a Google Sheet using googlesheets
}
\description{
read_sheet takes a Google Sheets sharing url, and it returns, via
the googlesheets package, a data frame of the Google Sheet. It defaults
to producing a data frame of Sheet1 of any Google Sheet, but the name
of the sheet that will be returned may be specified.
}
\usage{
read_sheet(url,sheetname)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{url}{A Google Sheets sharing url the user must supply as a character string with 
  quotation marks.}
  \item{sheetname}{A character name defaults to "Sheet1", but the user may specify a
  specific sheet name of their own if their sheet is named in Google Sheets.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A data frame of the requested Google Sheet.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
David Burton
}
\note{
Wubba lubba dub dub
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
