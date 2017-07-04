#' Check if the object is of class \code{\link{Date}}
#'
#' Check if the object is of class \code{\link{Date}}
#'
#' @param  x  object
#'
#' @return \code{\link{logical}}
#'
#' @author Thomas P. Harte
#'
#' @keywords \code{\link{Date}}
#'
#' @seealso \code{\link{Date}}
#'
#' @examples
#'   require(zoo)
#'   is_Date(as.Date("2017-07-04"))
#'   is_Date("2017-07-04")
#'
#'   x<- seq(as.Date("2017-07-01"), len=4, by=1)
#'   is_Date(x)
#'   is_Date(as.character(x))
#'
#' @export
`is_Date`<- function(x) {
	# paste(class(x), collapse=", ")=="Date"
	inherits(x, "Date")
}


#' Check if the object is of class \code{\link{yearmon}}
#'
#' Check if the object is of class \code{\link{yearmon}}
#'
#' @param  x  object
#'
#' @return \code{\link{logical}}
#'
#' @author Thomas P. Harte
#'
#' @keywords \code{\link{yearmon}}
#'
#' @seealso \code{\link{yearmon}}
#'
#' @examples
#'   require(zoo)
#'   is_yearmon(as.yearmon("2017-07-04"))
#'   is_yearmon("2017-07-04")
#'
#'   x<- as.yearmon(seq(as.Date("2017-07-01"), len=4, by=1))
#'   is_yearmon(x)
#'   is_yearmon(as.character(x))
#'
#'
#' @export
`is_yearmon`<- function(x) {
    inherits(x, "yearmon")
}
