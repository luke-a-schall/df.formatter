\name{swap.row}
\alias{swap.row}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Swapping Rows
}
\description{
Swaps a row with another row in an existing data frame
}
\usage{
swap.row(df, row.num1, row.num2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
A data frame to be fortmatted
}
  \item{row.num1}{
An integer indicating the location of a row to be swapped
}
  \item{row.num2}{
An integer indicating the location of another column to swap
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
body <- swap.row(body, 1, 2)
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
