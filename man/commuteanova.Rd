\name{commuteanova}
\alias{commuteanova}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Find out if times differ significantly
}
\description{Takes a data frame of times in minutes, as integers, which have a
variable, Day, defining their factor group, and returns the conclusion of an ANOVA.
}
\usage{
commuteanova(commutedf,Leave="Left",Arrive="ArriveDesk",
Time="Total",Group="Day",usertimebins=6)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{commutedf}{Data frame which must contain either two columns
  Arrive and Leave, or one column, Total. Total contains an integer
  value in minutes for a total commute time. Arrive and Leave must
  contain time of arrival at destination and time of departure from
  residence, respectively. All other arguments are optional, and
  all arguments, accept for commutedf itself, have defaults. Leave, Arrive,
  Time, and Group all have default character strings for column names in
  commutedf, and timebins has a default of 6. Timebins is how many
  different groups into which the commuting window will be split for
  analysis.}
  \item{Leave}{Time of departure column name. Default: "Left"}
  \item{Arrive}{Time of arrival column name. Default: "ArriveDesk"}
  \item{Time}{Total commute time column name. Default: "Total"}
  \item{Group}{Group name for analysis by factor. Default: "Day"}
  \item{usertimebins}{Integer for number of groups to split
  times into for k-means clustering. Default: 6.}
}


\details{
Perhaps this can be generalized to other time collection purposes.
}

\value{
The returned message advises whether the times are significantly
different by Day, and if not it will still advise which day has
the shortest average time, or commute.
}

\author{
David Burton, URMC Dept. of Biostatistics
}

\note{wubba lubba dub dub}

%% ~Make other sections like Warning with \section{Warning }{....} ~



% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ commute }% use one of  RShowDoc("KEYWORDS")
\keyword{ anova }% __ONLY ONE__ keyword per line
