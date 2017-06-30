`loadAs`<- function(file) {
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
