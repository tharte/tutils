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
