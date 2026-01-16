\name{delete.row}
\alias{delete.row}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Deleting a Row
}
\description{
Deletes a row in an existing data frame
}
\usage{
delete.row(df, row.num, rows.numbered = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
A data frame to be formatted
}
  \item{row.num}{
An integer indicating the location of the row to be deleted
}
  \item{rows.numbered}{
A logical indicating whether to name the rows in ascending numerical order
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
iris_data <- iris
iris_data <- delete.row(iris_data, 1)
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
