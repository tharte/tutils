#' Reload a single object written with the function save
#'
#' load_as reloads the contents of a single object that was saved in .Rdata format into the workspace
#'
#' @param  file \code{\link{character}} file name of the stored object to reload
#'
#' @return \code{\link{character}}
#'
#' @author Thomas P. Harte
#'
#' @keywords \code{\link{sort}}
#'
#' @seealso \code{\link{load}}, \code{\link{save}}
#'
#' @examples
#'	file<- paste(tempfile(), ".Rdata", sep="")
#'	objectWithAVeryStrangeName<- rnorm(10)
#'	whos()
#'	save(objectWithAVeryStrangeName, file=file)
#'	## not run:
#'	## load(file)
#'	## loads objWithAVeryStrangeName into the workspace which may be undesirable
#'	x<- load_as(file)
#'	whos()
#'	unlink(file)
#'
#' @export
`load_as`<- function(file) {
	stopifnot(is.character(file), file.exists(file))
	# make a temporary environment in which to inspect the
	# contents of file
	e<- new.env()
	# var is a character string containing the variables loaded
	var<- load(file, envir=e)
	if (length(var)!=1) {
		stop(paste(file, "contains more than one variable"))
	}
	return(get(var, envir=e))
}
