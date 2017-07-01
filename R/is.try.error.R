#' check for try-error
#'
#' checks to see if a try-error occurred
#'
#'
#' @param  x \code{\link{try}}
#'
#' @return \code{\link{logical}}
#'
#' @author Thomas P. Harte
#'
#' @keywords \code{\link{environment}}
#'
#' @seealso \code{\link{try}}
#'
#' @examples
#'	e<- new.env()
#'	local({
#'		a<- "a"
#'		b<- "this is 'b'"
#'		x<- pi
#'	}, env=e)
#'	f<- new.env()
#'	is.try.error(try(
#'		put.var("someVar", from=e, to=f)
#'	))
#'	whos(sort="Name", env=f)
#'	# Error in get(var, envir = fromEnvir) : object 'someVar' not found
#'	# [1] TRUE
#'	#     Class Dimensions Bytes
#'	#NULL    NA         NA    NA
#'
#' @export
`is.try.error`<- function (x) {
	return("try-error" %in% class(x))
}
