`sort.df`<-
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

