\name{us.cities}
\alias{us.cities}
\docType{data}
\title{
All Cities in the United States
}
\description{
A collection of all of the cities within the United States
}
\usage{data("us.cities")}
\format{
  A data frame with 31120 observations on the following 9 variables.
  \describe{
    \item{\code{city}}{Name of the US City}
    \item{\code{state_id}}{State Abbreviation of the US City}
    \item{\code{state_name}}{Name of the State for the US City}
    \item{\code{county_name}}{Name of the County for the US City}
    \item{\code{lat}}{The Latitude of the US City}
    \item{\code{lng}}{The Longitude of the US City}
    \item{\code{population}}{The Population of the US City}
    \item{\code{density}}{Density of the US City}
    \item{\code{zips}}{The Zip Codes within the US City}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
https://simplemaps.com/data/us-cities
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(us.cities)
}
\keyword{datasets}
