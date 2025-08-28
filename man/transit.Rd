\name{transit}
\alias{transit}
\docType{data}
\title{
US Metro Area Transportation Data}
\description{
A collection of the top 100 United States Metropolitan Areas based on population with the proportions of the population who use each mode of transit
}
\usage{data("transit")}
\format{
  A data frame with 100 observations on the following 15 variables.
  \describe{
    \item{\code{City}}{city names}
    \item{\code{State}}{state names}
    \item{\code{Region}}{region names}
    \item{\code{Percentile}}{a factor with levels 25, 75, and 100}
    \item{\code{Population}}{metro area populations}
    \item{\code{Car}}{proportion of residents who drive a car to work}
    \item{\code{Alone}}{proportion of residents who drive alone to work}
    \item{\code{Carpool}}{proportion of residents who carpool to work}
    \item{\code{Carpool2}}{proportion of residents who carpool to work with two riders}
    \item{\code{Carpool3}}{proportion of residents who carpool to work with three riders}
    \item{\code{Carpool4}}{proportion of residents who carpool to work with four riders}
    \item{\code{Public}}{proportion of residents who take public transit to work}
    \item{\code{Walk}}{proportion of residents who walk to work}
    \item{\code{Other}}{proportion of residents who take other modes of transit to work}
    \item{\code{Home}}{proportion of residents who work from home}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
https://censusreporter.org/tables/C08301/
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(transit)
}
\keyword{datasets}
