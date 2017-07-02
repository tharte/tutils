#' appends an object to the back of a list
#'
#' appends an object to the back of a list
#' @param  x \code{\link{list}} list of objects
#' @param  y \code{\link{list}} list of objects to append to \code{x}
#'
#' @return	\code{\link{character}}
#'
#' @author Thomas P. Harte
#'
#' @keywords \code{\link{gsub}}
#'
#' @seealso \code{\link{list}}
#'
#' @examples
#'	# zero-length list x: append zero-length object y:
#'	push_back(list(), list())
#'
#'	# zero-length list x: append non-list object:
#'	x<- list()
#'	x<- push_back(x, "one")
#'	x
#'
#'	# unnamed list x: append non-list object:
#'	x<- list()
#'	x[[1]]<- "one"
#'	x<- push_back(x, 2)
#'	x
#'
#'	# named list x: append non-list object:
#'	x<- list()
#'	x[["one"]]<- 1
#'	x<- push_back(x, "two")
#'	x
#'
#'	# unnamed list x: append unnamed list object y:
#'	x<- list()
#'	x[[1]]<- "one"
#'	y<- list(); y[[1]]<- 2;
#'	x<- push_back(x, y)
#'	x
#'
#'	# unnamed list x: append named list object y:
#'	x<- list()
#'	x[[1]]<- "one"
#'	y<- list(); y[[1]]<- 2; names(y)<- "two"
#'	x<- push_back(x, y)
#'	x
#'
#'	# unnamed list x: append named list object y:
#'	x<- list()
#'	x[[1]]<- 1; names(x)<- "one"
#'	y<- list(); y[[1]]<- 2; names(y)<- "two"
#'	x<- push_back(x, y)
#'	x
#'
#'	# note that a particularly useful idiom is to use
#'	# push_back to extract the named elements of a list:
#'	push_back(list(), x)
#'	# thus, if you wish to concatenate the *elements* of
#'	# a list with another list without simply pushing
#'	# back the entire list 'x':
#'	push_back(y, x)
#'	# then use this idiom thusly:
#'	push_back(y, push_back(list(), x))
#'
#' @export
`push_back`<- function(x, y) {
	# y can be a list or any other object to push back to x,
	# but x *must* be a list:
	stopifnot(is.list(x))

	# strategy: copy everything to out, then fix names;
	# for large size lists i found that this is the quickest way
	# to append list, or object, y to list x

	# 1. copy everything to out
	n.x<- length(x)
	# if y isn't a list we'll just push it back as is:
	n.y<- ifelse(is.list(y), length(y), 1)
	out<- vector("list", n.x + n.y)
	for (i in seq_len(n.x)) {
		out[[i]]<- x[[i]]
	}
	for (i in seq_len(n.y)) {
		# out[[i+n.x]]<- ifelse(is.list(y), y[[i]], y)
		if(is.list(y)) out[[i+n.x]]<- y[[i]]
		else           out[[i+n.x]]<- y
	}
	# 2. fix up names
	if (!is.null(names(x)) | !is.null(names(y))) {
		# 	x and/or y may have no names
		nms.x<- if(is.null(names(x))) vector("character", n.x) else names(x)
		nms.y<- if(is.null(names(y))) vector("character", n.y) else names(y)
		names(out)<- c(nms.x, nms.y)
		# NOTE: it is legal to assign non-unique names to a list (although
		#       i have no idea why)
		# stopifnot(length(c(nms.x, nms.y)) != length(unique(c(nms.x, nms.y))))
			# stop("something's wrong! non-unique names in x or y")
	}
	return(out)
}

#
# OLDER VERSIONS:
#
# note the use of 'as.character(substitute(y))' below ...
# need to alter 'insert' to reflect this change
if (0) {
`push_back`<-
function(x, y) {
	# append object 'y' to 'list' 'x'
	if (!is.list(x))
		stop("cannot push back object to non-list 'x'")
	n<- length(x)
	if (!is.null(names(x))) {
		name<- as.character(substitute(y))
		x[[name]]<- y
	}
	else
		x[[n+1]]<- y
	return (x)
}
}
