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


#' Compute (simple net) return of values in vector
#'
#' Compute (simple net) return of values in vector
#'
#' @param  x \code{\link{numeric}} \code{\link{vector}}
#' @param  nan.replace \code{\link{logical}} if TRUE, replace \code{\link{NaN}} (not-a-number) symbols with \code{NA}
#'
#' @return \code{\link{numeric}} \code{\link{vector}}
#'
#' @author Thomas P. Harte
#'
#' @keywords \code{\link{numeric}}, \code{\link{NaN}}, \code{\link{vector}}
#'
#' @seealso \code{\link{numeric}}, \code{\link{NaN}}, \code{\link{vector}}
#'
#' @examples
#'   get_return(c(NA))
#'   get_return(c(NA, NA))
#'   get_return(c(NaN, NA))
#'   get_return(c(NaN, 1, 1.1), nan.replace=TRUE)
#'   get_return(c(1, +Inf, 1.1), nan.replace=TRUE)
#'
#' @export
`get_return`<- function(
    x,
	nan.replace=FALSE
) {
	if (is.zoo(x))
        x<- as.numeric(x)
	x<- na.omit(x)
	n<- length(x)

	# we can only take a return when we have values for x[1] & x[2]
	# regardless of whether or not they are one of:
	#	0, NA, NaN, +Inf, -Inf

	if (n<2) return(NA)

	# if the values of x[1] and x[n] are one of:
	#	0, NA, NaN, +Inf, -Inf
	# then a non-a-number symbol will result

	r<- x[n]/x[1]-1

	if (nan.replace) {
		if (is.nan(r) | is.infinite(r))
			return (NA)
	}

	return(r)
}


#' Compute the difference of values in vector
#'
#' Compute the difference of values in vector
#'
#' @param  x \code{\link{numeric}} \code{\link{vector}}
#'
#' @return \code{\link{numeric}} \code{\link{vector}}
#'
#' @author Thomas P. Harte
#'
#' @keywords \code{\link{numeric}}, \code{\link{diff}}
#'
#' @seealso \code{\link{numeric}}, \code{\link{diff}}
#'
#' @examples
#'   get_diff(c(NA))
#'   get_diff(c(NA, NA))
#'   get_diff(c(NaN, NA))
#'   get_diff(c(NaN, 1, 1.1))
#'   get_diff(c(1, +Inf, 1.1))
#'
#' @export
`get_diff`<- function(x) {
	if (is.zoo(x)) x<- as.numeric(x)
	x<- na.omit(x)
	n<- length(x)

	# we can only take a diff when we have values for x[1] & x[2]
	if (n<2) return(NA)

	# if the values of x[1] and x[n] are one of 0 or the not-a-number symbols:
	#	0, NA, NaN, +Inf, -Inf
	# then a NaN (non-a-number) symbol will result

	r<- x[n]-x[1]

	return(r)
}
