\name{insert.row}
\alias{insert.row}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Inserting a Row
}
\description{
Inserts a row into an existing data frame with provided data
}
\usage{
insert.row(df, row.num, row.data, rows.numbered = TRUE, row.name = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
A data frame to be formatted
}
  \item{row.num}{
An integer indicating the location where the new row is to be inserted
}
  \item{row.data}{
A vector of the data to be inserted into the data frame
}
  \item{rows.numbered}{
A logical indicating whether to name the rows in ascending numerical order
}
  \item{row.name}{
A character string specifying the name of the row being added
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
body <- insert.row(body, 2, c("Brown", "Brown", "Male", "1"))
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
