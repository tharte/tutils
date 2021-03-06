% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ts-utils.R
\name{is_yearmon}
\alias{is_yearmon}
\title{Check if the object is of class \code{\link{yearmon}}}
\usage{
is_yearmon(x)
}
\arguments{
\item{x}{object}
}
\value{
\code{\link{logical}}
}
\description{
Check if the object is of class \code{\link{yearmon}}
}
\examples{
  is_yearmon(as.yearmon("2017-07-04"))
  is_yearmon("2017-07-04")

  x<- as.yearmon(seq(as.Date("2017-07-01"), len=4, by=1))
  is_yearmon(x)
  is_yearmon(as.character(x))


}
\seealso{
\code{\link{yearmon}}
}
\author{
Thomas P. Harte
}
\keyword{\code{\link{yearmon}}}
