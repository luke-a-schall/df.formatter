\name{missing.values}
\alias{missing.values}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Missing Values
}
\description{
Finds the columns with missing values and provides its statistics
}
\usage{
missing.values(df, type = "perc", decimal = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
A data frame to be checked
}
  \item{type}{
A character string specifying the "perc" or "sum" of missing values
}
  \item{decimal}{
An integer specifying the amount of decimal places for the percent
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
body <- data.frame(HairEyeColor)
body <- missing.values(body, type = "sum", decimal = NA)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
