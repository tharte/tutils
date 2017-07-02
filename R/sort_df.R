#' Sort a data.frame by column
#'
#' Sorts the columns of a \code{\link{data.frame}}
#'
#' @param  x \code{\link{data.frame}} on which to perform the sort
#' @param  cn \code{\link{character}} the name of the column(s) to sort.
#' @param  decreasing \code{\link{logical}} should the sort be increasing or decreasing?
#' @param  na.last \code{\link{logical}} see \code{\link{logical}};
#'           for controlling the treatment of \code{NA}s.
#' 	         If \code{TRUE}, missing values in the data are put last; if \code{FALSE},
#' 	         they are put first; if \code{NA}, they are removed.
#'
#' @return \code{\link{character}}
#'
#' @author Thomas P. Harte (originated by John Laing)
#'
#' @keywords \code{\link{sort}}
#'
#' @seealso \code{\link{sort}},
#'          \code{\link{data.frame}}
#' @examples
#' 	 df<- data.frame(Name=c("one","two","three"), Value=1:3)
#' 	 sort_df(df, cn="Value", decreasing=TRUE)
#' 	 # sort first on column "Value", then on column "Name"
#' 	 # note that both columns are sorted with decreasing=TRUE
#' 	 sort_df(df, cn=c("Value","Name"), decreasing=TRUE)
#'
#' @export
`sort_df`<-
function(x, cn, decreasing=FALSE, na.last=NA) {
	stopifnot(any("data.frame" %in% class(x)))

	errMsg<- paste("no matching columns of data frame:", paste(cn, collapse=" "))
	if (!is.character(cn) & mode(cn)!="numeric")
		stop(errMsg)

	# convert column names to their numerical equivalent:
	if (is.character(cn))
		cols<- pmatch(cn, colnames(x))
	else
		cols<- which(1:ncol(x) %in% cn)
	# error if is.null(colnames(x)) or non-matching column numbers:
	if (any(is.na(cols)) | !length(cols) | length(cols)!=length(cn)) stop(errMsg)

	# now sort using e.g. x[order(x[,3], x[,1]), ] :
	sequence<- do.call(function(...)
		order(..., decreasing=decreasing, na.last=na.last), as.list(x)[cols]
	)

	return(x[sequence, ])
}
