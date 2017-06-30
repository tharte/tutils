`first`<- function(x) {
	x<- na.omit(x)
	n<- length(x)
	if (!n) return(NA)
	else    return(x[1])
}
