#' Returns the head and tail part of an object
#'
#' Returns the head and tail part of an object
#'
#' @param x an object.
#' @param n \code{\link{integer}}
#'       If positive, size for the resulting object:
#'          number of elements for a vector (including lists), rows for a
#'          matrix or data frame or lines for a function. If negative,
#'          all but the 'n' last/first number of elements of 'x'
#' @param ... arguments to \code{\link{head}} and \code{\link{tail}}
#'
#' @return \code{\link{character}}
#'
#' @author Thomas P. Harte
#'
#' @keywords \code{\link{sort}}
#'
#' @seealso \code{\link{head}}, \code{\link{tail}}
#'
#' @examples
#'	 N<- 20
#'	 df<- data.frame(Name=letters[1:N], Value=1:N)
#'	 ht(df)
#'	 N<- 3
#'	 df<- data.frame(Name=letters[1:N], Value=1:N)
#'	 ht(df)
#'
#' @export
`ht`<- function (x, n = 6, ...) {
	if (any("list" %in% class(x))) {
		if (is.null(names(x)))
			    nm <- paste("[[", 1:length(x), "]]", sep = "")
		else
			    nm <- paste("$", names(x), sep = "")
		for (i in 1:length(x)) {
			    cat(sprintf("\n%s", nm[i]))
			    ht(x[[i]], n, ...)
		}
	}
	else {
		d <- dim(x)
		if (is.null(d))
			    d <- length(x)
		else
			    d <- d[1]
		if (d > 2 * n) {
			    cat("\n***>> head <<***\n")
			    print(head(x, n, ...))
			    cat("\n***>> tail <<***\n")
			    print(tail(x, n, ...))
		}
		else {
			    cat("\n***>> all <<***\n")
			    print(x)
		}
	}
}
