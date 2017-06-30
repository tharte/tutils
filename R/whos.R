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
			objs<- sort.df(objs, sort.by, decreasing=decreasing)
			objs<- objs[,-which(colnames(objs) %in% "Name")]
		}
		else {
			objs<- sort.df(objs, sort.by, decreasing=decreasing)
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
