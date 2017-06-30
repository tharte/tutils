`copy`<- 
function(x, 
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
