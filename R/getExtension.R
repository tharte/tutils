`getExtension`<- 
function (x, 
	parse=FALSE	# parse the string into basename & file extension
) {
	stopifnot(is.character(x))
	if (!length(grep("\\.", x)))
		if (parse)
			return(list(basename=x, extension=character(0)))
		else 	return(character(0))
	# cf. limma::removeExt 
	extension<- sub("(.*)\\.(.*)$", "\\2", x)
	basename<- sub("(.*)\\.(.*)$", "\\1", x)
	if (parse)
		return(list(basename=basename, extension=extension))
	else return(extension)
}

