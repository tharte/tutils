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
#'   str<- c("hello there", "how are you?")
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

#' Copy elements of a list
#'
#' Copies elements of a list, either indexed or named
#'
#'
#' @param x \code{\link{list}}, unnamed or named
#' @param index \code{\link{integer}} or \code{\link{character}}
#'   index of the elements in \code{x} to copy, either indexed
#'   \code{\link{integer}} or named \code{\link{character}}
#'
#' @return	\code{\link{list}}
#'
#' @author Thomas P. Harte
#'
#' @keywords \code{\link{list}}
#'
#' @seealso \code{\link{list}}
#'
#' @examples
#'  # NULL list:
#' 	copy(list(), integer(0))
#'
#' 	# unnamed list:
#'  x<- list(1, 2, 3)
#'  x
#'
#'  copy(x, integer(0))
#'  copy(x, 1:2)
#'
#'  # named list:
#'  x<- list(a=1, b=2, c=3)
#'  x
#'  # integer index still works:
#'  copy(x, 1:2)
#'  # named index works too:
#'  copy(x, c("a","b"))
#'
#'  ## FIXME: index mismatch
#'  ## copy(x, 1:4)
#'  ## copy(x, c("a","b"))
#'  ## copy(x, c("a","b","d"))
#'
#' @export
`copy`<- function(x,
	index=1:length(x)	# preference is for index of class 'integer'
				# as opposed to 'character' names
) {

if (0) {
# [2009-07-20]: John Laing pointed out that using **single square brackets**
#               (appears to) accomplishes everything that copy does
# TODO:
#	replace with
	x[index]
# HOWEVER:
# 	note from
	?"["
#       that x[[index, exact=TRUE]] only works for "[[" accessor not "["
#       so that partial matches are performed whereas 'copy' does exact matching
}
	# TODO: make generic?
	stopifnot(is.list(x))
	if (is.character(index)) {
		if (is.null(names(x)))
			stop("index uses names but input is not a named list")
		ix<- which(names(x) %in% index)
		if (!length(ix))
			stop("could not find a match for index in list names")
		if (length(ix)!=length(index))
			stop("index only partial matches names")
		# revert to preference for 'integer' index
		return(copy(x, index=ix))
	}
	stopifnot(is.vector(index), is.numeric(index))
	if (any(!is.element(index, 1:length(x))))
		stop("index does not match")
	if (identical(index, 1:length(x)))
		return(x)

	if (!length(index))
		return(list())
	else
		# this fills out with NULL
		out<- vector("list", length(index))

	# note that 'out' has its own index set:
	out.index<- 1:length(index)
	for (i in out.index) out[[i]]<-
		if (!is.null(x[[index[i]]])) x[[index[i]]] else {next}
	if (!is.null(names(x)))
		names(out)<- names(x)[index]
	return(out)
}

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

#' Copy variables from one environment to another environment
#'
#' @param from.env \code{\link{environment}} where variables are to be copied from
#' @param to.env \code{\link{environment}} where variables are to be copied to
#' @param vars \code{\link{character}} \code{\link{vector}} of named
#'   variables to copy from \code{\link{from.env}}
#'
#' @return \code{\link{invisible}}
#'
#' @author Thomas P. Harte
#'
#' @keywords \code{\link{environment}}
#'
#' @seealso  \code{\link{put.var}}
#'
#' @examples
#' e<- new.env()
#' local({
#'   a<- "a"
#'   b<- "this is 'b'"
#'   x<- pi
#' }, env=e)
#'
#' f<- new.env()
#' copy_vars(from=e, to=f)
#' whos(sort="Name", env=f)
#'
#' g<- new.env()
#' copy_vars(from=e, to=g, vars=(c("b","x")))
#' whos(sort="Name", env=g)
#'
#' @export
`copy_vars`<- function(from.env, to.env=.GlobalEnv, vars=ls(envir=from.env)) {
	sapply(vars, function(var) put_var(var, from.env=from.env, to.env=to.env))

	return(invisible())
}

#' copy object
#'
#' copy a single variable from one environment to another environment
#'
#' @param var \code{\link{character}} name of variable in \code{from.envir} to copy
#' @param from.envir \code{\link{environment}} where variable is to be copied from
#' @param to.envir \code{\link{environment}} where variable is to be copied to
#'
#' @return \code{\link{invisible}}
#'
#' @author Thomas P. Harte
#' @keywords \code{\link{environment}}
#' @seealso  \code{\link{copy.vars}}
#' @examples
#' 	 e<- new.env()
#' 	 local({
#' 		a<- "a"
#' 		b<- "this is 'b'"
#' 		x<- pi
#' 	 }, env=e)
#'   f<- new.env()
#' 	 put_var("x", from=e, to=f)
#' 	 whos(sort="Name", env=f)
#' @export
`put_var`<- function(var, from.envir=parent.frame(), to.envir=.GlobalEnv) {
	assign(var, get(var, envir=from.envir), envir=to.envir)

	return(invisible())
}

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
#'	is_try_error(try(
#'		put_var("someVar", from=e, to=f)
#'	))
#'	whos(sort="Name", env=f)
#'	# Error in get(var, envir = fromEnvir) : object 'someVar' not found
#'	# [1] TRUE
#'	#     Class Dimensions Bytes
#'	#NULL    NA         NA    NA
#'
#' @export
`is_try_error`<- function (x) {
	return("try-error" %in% class(x))
}

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

#' List objects in an environment
#'
#' Lists objects in a specified environment in a way that is more
#' useful than \code{\link{ls}}
#'
#' @param all \code{\link{logical}} list all objects rather than the first 10
#' @param envir \code{\link{environment}} the environment in which to list objects
#' @param sort.by \code{\link{character}} sort on one of \code{Class},
#'   \code{Dimensions}, \code{Bytes} or by \code{Name}
#' @param decreasing \code{\link{logical}} should the sort.by be increasing or decreasing?
#' @param omit.classes \code{\link{character}} type of objects to omit from list
#'
#' @return A \code{\link{data.frame}}, with \code{\link{row.names}} being the
#'   objects in the \code{\link{environment}}
#'
#' @author Thomas P. Harte
#'
#' @keywords \code{\link{ls}}
#'
#' @seealso \code{\link{ls}}, \code{\link{sort_df}}, \code{\link{parent.frame}},
#'   \code{\link{environment}}
#'
#' @examples
#'	 # list objects in the current environment
#'	 whos()
#'
#'	 # list objects in alphabetical order
#'	 whos(sort="Name")
#'
#'	 # show all objects (specifically, include functions)
#'	 whos(sort="Name", omit=NULL)
#'
#' @export
`whos`<- function(
    all=TRUE,
    envir=parent.frame(),
    sort.by="Bytes",
    decreasing=FALSE,
    omit.classes="function"
) {
	# OBJECT SIZE (BYTES)
	sz<- sapply(ls(all=TRUE, envir=envir),
			function(x) object.size(get(x, envir=envir)))
	if (length(sz)) {
		# OBJECT CLASS
		cl<- lapply(ls(all=TRUE, envir=envir),
				function(x) class(get(x, envir=envir)))
		# Collapse class the same way we do for dimension
		# e.g. POSIX dates have class:
		# 	c("POSIXt",  "POSIXlt")
		cl<- unlist(lapply(cl, paste, collapse = " "))

		# OBJECT DIMENSIONS
		ll<- lapply(ls(all=TRUE, envir=envir),
				function(x) {
					d<- dim(get(x, envir=envir))
					return(	if (!is.null(d)) d
						else length(get(x, envir=envir))
					)
				})
		ll<- unlist(lapply(ll, paste, collapse=" x "))
		objs<- data.frame(Class=cl, Dimensions=ll, Bytes=format(sz,big.mark=","))
		# sort by name?
		by.name<- which(substr(tolower(sort.by), 1, 1) %in% "n")
		if (length(by.name)) {
			# crude hack: add a "Name" column, sort the data.frame
			# (could be sort on multiple columns), then remove the
			# "Name" column (because we want to list by rownames)
			objs[,"Name"]<- rownames(objs)
			objs<- sort_df(objs, sort.by, decreasing=decreasing)
			objs<- objs[,-which(colnames(objs) %in% "Name")]
		}
		else {
			objs<- sort_df(objs, sort.by, decreasing=decreasing)
		}
		# add a blank line:
		objs<- rbind(objs, data.frame(Class="", Dimensions="", Bytes=""))
		rownames(objs)[nrow(objs)]<- "------"
		# add the total-bytes line:
		objs<- rbind(objs, data.frame(Class="", Dimensions="", Bytes=format(sum(sz),big.mark=",")))
		rownames(objs)[nrow(objs)]<- "TOTAL:"
	}
	else {
		# purely cosmetic:
		objs<- data.frame(Class=NA, Dimensions=NA, Bytes=NA)
		rownames(objs)<- "NULL"
		return(objs)
	}
	# Kill things we don't necessarily want to see (like "function") :
	if (!is.null(omit.classes)) {
		if (!is.character(omit.classes))
			warning("omit.classes must be a character vector")
		else {
			# omit<- which(objs$Class %in% omit.classes)
			# allow partial matching & regular-expression class matching, e.g
			# omit=c("^fun*", "^li*")
			omit <- unique(unlist(sapply(omit.classes, function(x) grep(x, objs$Class), USE.NAMES=FALSE)))
			if (length(omit))
				objs<- objs[-omit, ]
		}
	}
	if (all) return(objs)
	else  {
		if (nrow(objs)>10)
			return(objs[1:10,,drop=FALSE])
		else    return(objs)
	}
}

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

#' Save zoo object to named extension file
#'
#' Saves a \code{\link{zoo}} object to a file with a named extension
#'
#'  @param z \code{\link{zoo}}
#' 	  a zoo object (\code{\link{coredata}} can be multi-column)
#'  @param file \code{\link{character}}
#' 	  filename in which to save \code{z} with the file extension being one of:
#'         \itemize{
#'                 \item{"psv"}{pipe-separated values}
#'                 \item{"csv"}{comma-separated values}
#'                 \item{"txt"}{tab-separated values}
#'                 \item{"xls"}{Excel spreadsheet}
#'                 \item{"Rdata"}{R binary file}
#'         }
#'
#' @author Thomas P. Harte
#'
#' @keywords \code{\link{zoo}}
#'
#' @seealso \code{\link{save}},
#'          \code{\link{write.table}},
#' 	        \code{\link{write.csv}},
#'          \code{\link{write.xls}},
#'          \code{\link{get_file_ext}},
#'          \code{\link{zoo}}
#'
#' @examples
#'  require(zoo)
#' 	n<- 10
#' 	dates<- seq(as.Date("2008-10-31"), by=1, len=n)
#' 	z<- zoo(matrix(c(rnorm(n, sd=.1), rnorm(n, sd=.5), rnorm(n, sd=1)), nc=3),
#' 		order.by=dates)
#' 	basename<- tempfile("z")
#'
#' 	# save as a PSV file:
#' 	filename<- paste(basename, ".psv", sep="")
#' 	save_zoo(z, file=filename)
#' 	# note: column names added by default:
#' 	file.show(filename)
#'
#' 	# add column names:
#' 	colnames(z)<- c("one", "two", "three")
#' 	# this time save as a CSV file:
#' 	filename<- paste(basename, ".csv", sep="")
#' 	save_zoo(z, file=filename)
#' 	file.show(filename)
#'
#' 	# this time save as a TXT file:
#' 	filename<- paste(basename, ".txt", sep="")
#' 	save_zoo(z, file=filename)
#' 	file.show(filename)
#'
#' 	filename<- paste(basename, ".Rdata", sep="")
#' 	save_zoo(z, file=filename)
#' 	# get rid of 'z':
#' 	rm(z)
#' 	# restore 'z':
#' 	print(load(filename))
#' 	# restore 'z' as 'foo':
#' 	foo<- load_as(filename)
#' 	stopifnot(identical(z, foo))
#'
#' @export
`save_zoo`<- function (z,
	file=character(0), 	# filename (with extension)
	append=FALSE, 		# append data to existing file?
    ...
) {
	`make.xsv.data.frame`<- function(z) {
		if (!is.null(colnames(z)))
			cn<- colnames(z)
		else
			cn<- paste(1:NCOL(z), sep="")
		out<- as.data.frame(z)
		rows<- rownames(out)
		rownames(out)<- NULL
		out<- data.frame(rows, out, stringsAsFactors=FALSE)
		colnames(out)<- c("Date", cn)
		return(out)
	}
	stopifnot(is.zoo(z))
	if (length(file)) {
		ext<- get_file_ext(file)
		if (length(ext)) {
			base<- get_file_ext(file, parse=TRUE)$base
			switch(tolower(ext),
# TODO: `save_zoo.as.csv`<- function(z, file, digits=NULL, nsmall=0, ...) {
				"psv" = {
					write.table(make.xsv.data.frame(z),
						file=file, quote=FALSE, row.names=FALSE, sep="|", append=append, ...)
				},
# TODO: `save_zoo.as.csv`<- function(z, file, digits=NULL, nsmall=0, ...) {
				"csv" = {
					write.csv(make.xsv.data.frame(z),
						file=file, quote=FALSE, row.names=FALSE, append=append, ...)
				},
				"txt" = {
					write.table(make.xsv.data.frame(z),
						file=file, quote=FALSE, sep="\t", ...)
				},
				"xls" = {
					switch(Sys.info()["sysname"],
						Windows = {
							require(xlsReadWrite, quietly=FALSE)
							write.xls(as.data.frame(z), file=file, ...)
						}
					)
				},
				"rdata" = {
					# z<- as.character(substitute(z))
					# save(get(z, envir=environment()), file=file, ...)
					# TODO: this saves the object as 'z' ...
					save(z, file=file, ...)
				}
			)
		}
		else error(sprintf("'%s' has no file extension ... cannot save data", file))
	}
	else error("'file' is empty ... cannot save data")
}


#' Get column classes in \code{\link{data.frame}}
#'
#' Get column classes in \code{\link{data.frame}}
#'
#' @param  DF \code{\link{data.frame}} or \code{\link{matrix}}
#' @param  flip \code{\link{logical}} flips results sideways, if \code{TRUE}
#'
#' @return named \code{\link{vector}} of \code{\link{character}} strings
#'
#' @author Thomas P. Harte
#'
#' @keywords \code{\link{class}}
#'
#' @seealso \code{\link{class}}
#'
#' @examples
#'	 DF<- data.frame(
#'        names=c("one","two","three"),
#'        numbers=1:3,
#'        stringsAsFactors=FALSE
#'   )
#'
#'	 out<- col_classes(DF)
#'
#'	 all(colnames(out)==c("names","numbers"))
#'	 out["names"] == "character"
#'	 out["numbers"] == "integer"
#'
#' @export
`col_classes`<- function(DF, flip=FALSE) {
	assert(inherits(DF, "data.frame") | is.matrix(DF))

    if (all(dim(DF)==c(0,0)))
        return(matrix(, nr=0, nc=0))

	out<-        character(ncol(DF))
	names(out)<- colnames(DF)

	for (col in 1:ncol(DF))
		out[col]<- paste(class(DF[[col]]), collapse=", ")

    if (flip)
        return(as.matrix(out))

	return(out)
}


#' Find row-col location of a search string in \code{\link{data.frame}}
#'
#' Description: What the function does in more detail
#'
#' @param  term \code{\link{character}} string to search for
#' @param  DF \code{\link{data.frame}} or \code{\link{matrix}} to search
#' @param  ROW.FUN \code{\link{function}} for row search
#' @param  COL.FUN \code{\link{function}} for column search
#'
#' @return named \code{\link{vector}} with row & col location of search
#'
#' @author Thomas P. Harte
#'
#' @keywords \code{\link{match}}, \code{\link{grep}}
#'
#' @seealso \code{\link{match}}, \code{\link{grep}}
#'
#' @examples
#'	tab<- read.csv(con<- textConnection(
#'	"Name,    Age, Salary
#'	 Derek  ,  NA,    32k		# <- NOTE: 'Derek  '
#'	 Tom,      26,    21k
#'	 NA,       NA,     NA
#'	 Harry,    31,    50k"
#'	), header=TRUE, colClasses=c("character","integer","character"), comment.char="#"); close(con)
#'
#'	match_col("NON-MATCHING-STRING", tab, ROW.FUN="first", COL.FUN="first")
#'	match_col("NON-MATCHING-STRING", tab, ROW.FUN="first", COL.FUN="last")
#'	match_col("NON-MATCHING-STRING", tab, ROW.FUN="last", COL.FUN="first")
#'	match_col("NON-MATCHING-STRING", tab, ROW.FUN="last", COL.FUN="last")
#'
#'	result<-        rep(NA,2)
#'	names(result)<- c("row","col")
#'
#'	result["row"]<- 1; result["col"]<- 1
#'	all.equal(match_col("k", tab, ROW.FUN=tutils::first, COL.FUN=tutils::first), result)
#'
#'	result["row"]<- 1; result["col"]<- 3
#'	all.equal(match_col("k", tab, ROW.FUN=tutils::first, COL.FUN=tutils::last), result)
#'
#'	result["row"]<- 4; result["col"]<- 1
#'	all.equal(match_col("r", tab, ROW.FUN=tutils::last, COL.FUN=tutils::first), result)	# <- NOTE: testing for "r"
#'
#'	result["row"]<- 4; result["col"]<- 3
#'	all.equal(match_col("k", tab, ROW.FUN=tutils::last, COL.FUN=tutils::last), result)	# <- NOTE: testing for "k"
#'
#' @export
`match_col`<- function(
	term,
	DF,
	ROW.FUN=c("first", "last"),
	COL.FUN=c("first", "last")
) {
    `make_row_col`<- function() {
        out<-        rep(NA, 2)
        names(out)<- c("row", "col")

        return(out)
    }

	assert(
        is.character(term),
        inherits(DF, "data.frame") | is.matrix(DF)
    )

	ROW.FUN<- match.fun(ROW.FUN)
	COL.FUN<- match.fun(COL.FUN)
	out<-     make_row_col()

	if (any(cols.character<- col_classes(DF) %in% "character")) {
		rows<- apply(DF, 1, FUN=function(x) COL.FUN(grep(term, x)))
		ix<-   which(!is.na(rows))
		if (length(ix)) {
			out["row"]<- ROW.FUN(ix)
			out["col"]<- rows[out["row"]]
		}
	}

	return(out)
}


#' Remove all columns or rows from \code{\link{data.frame}} matching criterion
#'
#' Remove all columns or rows from \code{\link{data.frame}} matching criterion
#'
#' @param  DF \code{\link{data.frame}} or  \code{\link{matrix}}
#' @param  fun \code{\link{function}} with criterion for removal
#'
#' @return \code{\link{data.frame}} or \code{\link{matrix}} with matching removed
#'
#' @author Thomas P. Harte
#'
#' @keywords \code{\link{grep}}, \code{\link{match}}
#'
#' @seealso \code{\link{grep}}, \code{\link{match}}
#'
#' @examples
#'	FUN<- function(x) is_blank(x) | is.na(x)
#'
#'	tab<- read.table(con<- textConnection(
#'	"Name  Age Salary
#'	 Dick   NA    32k
#'	 Tom    NA    21k
#'	 NA     NA    NA"
#'	), header=TRUE, colClasses=c("character","integer","character")); close(con)
#'
#'
#'  remove_from_rows(tab, fun=FUN)
#'  remove_from(tab, fun=FUN, dim="row")
#'
#'  remove_from_cols(tab, fun=FUN)
#'  remove_from(tab, fun=FUN, dim="col")
#'
#'  remove_from(tab, fun=FUN, dim="both")
#'
#' @export
#' @name remove_from
`remove_from`<- function(DF, fun, dim=c("both","rows","cols")) {
	assert(
        inherits(DF, "data.frame") | is.matrix(DF),
        is.function(fun)
    )
	dim<- match.arg(dim)

	switch(dim,
		both    = {
			DF<- remove_from_rows(remove_from_cols(DF, fun), fun)
		},
		rows    = {
			DF<- remove_from_rows(DF, fun)
		},
		cols = {
			DF<- remove_from_cols(DF, fun)
		}
	)

    DF
}


#' @export
#' @rdname remove_from
`remove_from_rows`<- function(DF, fun) {
	assert(
        inherits(DF, "data.frame") | is.matrix(DF),
        is.function(fun)
    )

    rm.ix<- which(apply(fun(DF), 1, sum)==ncol(DF))
    if (length(rm.ix))
        DF<- DF[-rm.ix, ]

    DF
}


#' @export
#' @rdname remove_from
`remove_from_cols`<- function(DF, fun) {
	assert(
        inherits(DF, "data.frame") | is.matrix(DF),
        is.function(fun)
    )

    rm.ix<- which(apply(fun(DF), 2, sum)==nrow(DF))
    if (length(rm.ix))
        DF<- DF[, -rm.ix]

    DF
}


#' Re-order named columns to front or back of \code{\link{data.frame}}
#'
#' Re-order named columns to front or back of \code{\link{data.frame}}
#'
#' @param  x \code{\link{data.frame}}
#' @param  cn \code{\link{character}} \code{\link{vector}} of column names
#'
#' @return \code{\link{data.frame}} with re-ordered columns
#'
#' @author Thomas P. Harte
#'
#' @keywords \code{\link{data.frame}}, \code{\link{dplyr::everything}}
#'
#' @seealso \code{\link{data.frame}}, \code{\link{dplyr::everything}},
#'   \url{http://stackoverflow.com/questions/27865865/in-dplyr-how-to-delete-and-rename-columns-that-dont-exist-manipulate-all-name}
#'
#' @examples
#'	tab<- read.table(
#'        con<- textConnection(
#'            "Name    Age Salary ID
#'             Dick     38    32k  1
#'             Tom      21    21k  2
#'             Harry    56     NA  3"
#'        ),
#'        header=TRUE,
#'        colClasses=c("character","integer","character","integer")
#'    )
#'    close(con)
#'  tab
#'
#'  tab %>% order_cn_front(c("Salary","Age"))
#'  tab %>% order_cn_back(c("Salary","Age"))
#'
#' @export
#' @name order_cn
`order_cn_front`<- function(x, cn) {
    assert(
        inherits(x, "data.frame"),
        cn %in% (orig.cn<- colnames(x))
    )

    x %>% select(one_of(c(cn, setdiff(orig.cn, cn))))
}


#' @export
#' @rdname order_cn
`order_cn_back`<- function(x, cn) {
    assert(
        inherits(x, "data.frame"),
        cn %in% (orig.cn<- colnames(x))
    )

    x %>% select(one_of(c(setdiff(orig.cn, cn), cn)))
}
