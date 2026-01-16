\name{delete.rows}
\alias{delete.rows}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Deleting Rows
}
\description{
Deletes rows in an existing data frame
}
\usage{
delete.rows(df, row.nums, rows.numbered = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
A data frame to be formatted
}
  \item{row.nums}{
A vector indicating the locations of the rows to be deleted
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
iris_data <- delete.rows(iris_data, c(1,2))
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
