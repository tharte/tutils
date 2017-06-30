	# whos()
	# whos(all=TRUE)
	# whos(all=TRUE)[1:20,,drop=FALSE]
	# function(all=FALSE, envir=.GlobalEnv) {
# if (0) {
#			# first or
#			objs<- objs[order(rownames(objs), decreasing=decreasing),]
# }
# These functions provide access to 'environment's (frames in S
# terminology) associated with functions further up the calling
# stack:
# ?sys.nframe
#	foo<- function() { qux<- function() {print(sys.nframe())}; qux() }; foo()
#	foo<- function() { qux<- function() {print(sys.calls())}; qux() }; foo()
#	foo<- function() { qux<- function() {print(sys.parents())}; qux() }; foo()
# ?sys.parent
# ?sys.frame
#     The parent frame of a function *evaluation* is the environment in
#     which the function was *called*.  It is not necessarily numbered one
#     less than the frame number of the current evaluation, nor is it
#     the environment within which the function was defined. 
# hence, here sys.frame(-1) : gets the environment of the *calling* frame NOT the parent frame (which is where the function 'whos' is *defined*
	# of note (and this is actually a very useful idiom for 'whos'):	
	#		w<- whos()
	# 		aggregate(w$Bytes, by=list(w$Class), sum)
	# must assign 'w<- whos()', otherwise
	# 		aggregate(whos()$Bytes, by=list(whos()$Class), sum)
	# would, by default, look in 'aggregate's environment
	# also of note:
	#		w<- whos()
	#		split(w, w$Class)
	#		split(w, w$Class)$function
# other arguments that I tried:
# function(all=TRUE, envir=sys.frame(-1), sort.by.size=TRUE, omit.classes=NULL) {	
# function(all=TRUE, envir=environment(), sort.by.size=TRUE, omit.classes=NULL) {	
# function(all=FALSE, envir=as.environment(-1), sort.by.size=TRUE) {	


`whos`<- 	
function(all=TRUE, envir=parent.frame(), sort.by="Bytes", decreasing=FALSE, omit.classes="function") {	
	# OBJECT SIZE (BYTES)
	sz<- sapply(ls(all=TRUE, envir=envir), 
			function(x) object.size(mget(x, envir=envir)[[1]]))
	if (length(sz)) {
		# OBJECT CLASS
		cl<- sapply(ls(all=TRUE, envir=envir), 
				function(x) class(mget(x, envir=envir)[[1]]))
		# Collapse class the same way we do for dimension
		# e.g. POSIX dates have class:
		# 	c("POSIXt",  "POSIXlt")
		cl<- unlist(lapply(cl, paste, collapse = " "))

		# OBJECT DIMENSIONS
		ll<- lapply(ls(all=TRUE, envir=envir), 
				function(x) {
					d<- dim(mget(x, envir=envir)[[1]])
					return(	if (!is.null(d)) d 
						else length(mget(x, envir=envir)[[1]])
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

