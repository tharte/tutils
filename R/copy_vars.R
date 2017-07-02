#' Copy variables from one environment to another environment
#'
#' @param from.env \code{\link{environment}} where variables are to be copied from
#' @param to.env \code{\link{environment}} where variables are to be copied to
#' @param vars \code{\link{character}} \code{\link{vector}} of named
#'   variables to copy from \code{\link{from.env}}
#'
#' @return \code{\link{invisible}}
#'
#' @author Thomas P. Harte
#'
#' @keywords \code{\link{environment}}
#'
#' @seealso  \code{\link{put.var}}
#'
#' @examples
#' e<- new.env()
#' local({
#'   a<- "a"
#'   b<- "this is 'b'"
#'   x<- pi
#' }, env=e)
#'
#' f<- new.env()
#' copy_vars(from=e, to=f)
#' whos(sort="Name", env=f)
#'
#' g<- new.env()
#' copy_vars(from=e, to=g, vars=(c("b","x")))
#' whos(sort="Name", env=g)
#'
#' @export
`copy_vars`<- function(from.env, to.env=.GlobalEnv, vars=ls(envir=from.env)) {
	sapply(vars, function(var) put_var(var, from.env=from.env, to.env=to.env))

	return(invisible())
}
