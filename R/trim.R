`triml`<- function(str) {
	if (is.null(str)) return(NULL)
	if (!is.character(str))
		stop("input must be of mode 'character'")
	gsub('^[[:space:]]+', '', str)
}
`trimr`<- 
function(str) {
	if (is.null(str)) return(NULL)
	if (!is.character(str))
		stop("input must be of mode 'character'")
	gsub('[[:space:]]+$', '', str)
}
`trim`<-  
function(str) {
	if (is.null(str)) return(NULL)
	if (!is.character(str))
		stop("input must be of mode 'character'")
	triml(trimr(str))
}
