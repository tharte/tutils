`capitalize`<- 
function (x) {
	if (!is.character(x))
		stop("input must be a character string")

	return( gsub("\\b(\\w)", "\\U\\1", x, perl=TRUE) )
}

