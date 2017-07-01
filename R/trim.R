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
