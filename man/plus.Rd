% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/object-utils.R
\name{+}
\alias{+}
\title{Open up a \code{\link{tibble}} (or similar) \code{\link{data.frame}}}
\usage{
"+"(e1, e2)
}
\arguments{
\item{e1}{any object, or a \code{\link{data.frame}}}

\item{e2}{any other object}
}
\value{
\code{\link{data.frame}}
}
\description{
Open up a \code{\link{tibble}} (or similar) \code{\link{data.frame}}.
Printing a \code{\link{tibble}} typically only displays a few rows
of the table. \code{+} is used to open up the table fully, as if
it were a \code{\link{data.frame}}.
}
\examples{
# mtcars is a regular data.frame
  mtcars
  mtcars \%>\% tbl_df
  mtcars \%>\% tbl_df \%>\% `+`

# with typical use as follows:
  x<- mtcars \%>\% tbl_df
  x
  +x

}
\seealso{
\code{\link{data.frame}}, \code{\link{tibble}}, \code{\link{tbl_df}}
}
\author{
Thomas P. Harte
}
\keyword{\code{\link{data.frame}},}
\keyword{\code{\link{tbl_df}}}
\keyword{\code{\link{tibble}},}
