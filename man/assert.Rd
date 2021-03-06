% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc-utils.R
\name{assert}
\alias{assert}
\title{Asserts that the condition(s) is true}
\usage{
assert(..., exprs, local = TRUE)
}
\description{
Asserts that the condition(s) is true
}
\examples{
  assert(is.data.frame(data.frame()))

}
\seealso{
\code{\link{stopifnot}}
}
\author{
Thomas P. Harte (inspired by the Rev Miguel Weylandto)
}
\keyword{\code{\link{stopifnot}}}
