#' Extract first / last object
#'
#' @aliases last
#'
#'	first and last return the first and last (non-\code{NA})
#'	objects respectively using \code{\link{[}}
#'
#' @param x \code{object} indexable by \code{\link{[}}
#'
#' @return \code{object} of same \code{mode} as \code{x}
#'
#' @author Thomas P. Harte
#'
#' @keywords \code{\link{sort}}
#'
#' @seealso \code{\link{Extract}}, \code{\link{na.omit}}
#'
#' @examples
#'   # from the following phrases
#'   str<- c("hallo there", "how are you?")
#'   cat(paste(str), sep="\n")
#'   # extract the first words:
#'   sapply(strsplit(str, " "), first)
#'   # extract the last words:
#'   sapply(strsplit(str, " "), last)
#'
#'   # in fact this is a very useful idiom, e.g. with stock tickers:
#'   tickers<- c("INDU Index", "USD Curncy")
#'   cat(paste(tickers), sep="\n")
#'   short.tickers<- sapply(strsplit(tickers, " "), first)
#'   short.tickers
#'   category<- sapply(strsplit(tickers, " "), last)
#'   category
#'
#' @export
#' @name first
`first`<- function(x) {
	x<- na.omit(x)
	n<- length(x)
	if (!n) return(NA)
	else    return(x[1])
}

#' @export
#' @rdname first
`last`<- function(x) {
	x<- na.omit(x)
	n<- length(x)
	if (!n) return(NA)
	else    return(x[n])
}
