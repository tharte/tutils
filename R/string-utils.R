#' Capitalize first character words in a string
#'
#' Capitalize the first character of each word in a character string
#'
#' @param x \code{\link{character}} string to capitalize
#'
#' @return \code{\link{character}} string with start of words capitalized
#'
#' @author Thomas P. Harte
#'
#' @seealso \code{\link{gsub}}
#'
#' @examples
#' capitalize("hello, world!")
#'
#' @export
`capitalize`<- function (x) {
	if (!is.character(x))
		stop("input must be a character string")

	return( gsub("\\b(\\w)", "\\U\\1", x, perl=TRUE) )
}

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
#'   get_file_ext("foo.bar.csv")
#'   # "csv" :
#'   get_file_ext("foo.bar.csv", parse=TRUE)
#'   # $basename
#'   # [1] "foo.bar"
#'   # $extension
#'   # [1] "csv"
#'
#' @export
`get_file_ext`<- function (
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

#' Trim whitespace
#'
#' Trim whitespace from ends of \code{\link{character}} string
#'
#' @aliases triml, trimr
#'
#' @param str \code{\link{character}} string
#'
#' @return \code{\link{character}}
#'
#' @author Thomas P Harte
#'
#' @seealso \code{\link{gsub}}
#'
#' @examples
#'	# left trim:
#'	cat(sprintf("'\%s'\n", triml(" asdf qwerty \t")))
#'	cat(sprintf("'\%s'\n", triml("\tasdf qwerty \t")))
#'	# right trim:
#'	cat(sprintf("'\%s'\n", trimr(" asdf qwerty \t")))
#'	cat(sprintf("'\%s'\n", trimr("\tasdf qwerty \t ")))
#'	# left & right trim:
#'	cat(sprintf("'\%s'\n", trim(" asdf qwerty \t")))
#'	cat(sprintf("'\%s'\n", trim("\tasdf qwerty \t")))
#'	cat(sprintf("'\%s'\n", trim(" asdf qwerty \t")))
#'	cat(sprintf("'\%s'\n", trim("\tasdf qwerty \t ")))
#'
#' @keywords \code{\link{sort}}
#'
#' @export
#' @name trim
`trim`<-
function(str) {
	if (is.null(str)) return(NULL)
	if (!is.character(str))
		stop("input must be of mode 'character'")
	triml(trimr(str))
}
#' @export
#' @rdname trim
`triml`<- function(str) {
	if (is.null(str)) return(NULL)
	if (!is.character(str))
		stop("input must be of mode 'character'")
	gsub('^[[:space:]]+', '', str)
}
#' @export
#' @rdname trim
`trimr`<-
function(str) {
	if (is.null(str)) return(NULL)
	if (!is.character(str))
		stop("input must be of mode 'character'")
	gsub('[[:space:]]+$', '', str)
}
