#' Get file extension
#'
#' Returns the file extension of a file name
#'
#' @param x \code{\link{character}} a file name
#' @param parse \code{\link{logical}} if TRUE, the filename
#'   is parsed and returned as a \code{\link{list}}
#'	 with two elements: basename and extension
#'
#' @return \code{\link{character}} file extension
#'
#' @author Thomas P. Harte
#'
#' @keywords \code{\link{sort}}
#'
#' @seealso \code{\link[limma]{removeExt}}
#'
#' @examples
#'   # "csv" :
#'   getExtension("foo.bar.csv")
#'   # "csv" :
#'   getExtension("foo.bar.csv", parse=TRUE)
#'   # $basename
#'   # [1] "foo.bar"
#'   # $extension
#'   # [1] "csv"
#'
#' @export
`getExtension`<- function (
    x,
	parse=FALSE	# parse the string into basename & file extension
) {
	stopifnot(is.character(x))
	if (!length(grep("\\.", x)))
		if (parse)
			return(list(basename=x, extension=character(0)))
		else 	return("")
	# cf. limma::removeExt
	extension<- sub("(.*)\\.(.*)$", "\\2", x)
	basename<- sub("(.*)\\.(.*)$", "\\1", x)
	if (parse)
		return(list(basename=basename, extension=extension))
	else return(extension)
}
