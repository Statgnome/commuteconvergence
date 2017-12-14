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

\value{
A data frame of the requested Google Sheet.
}

\author{
David Burton, URMC Dept. of Biostatistics
}
\note{
Wubba lubba dub dub
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ commute }% use one of  RShowDoc("KEYWORDS")
\keyword{ sheets }% __ONLY ONE__ keyword per line
