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
