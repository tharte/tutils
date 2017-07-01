#' Set the top-level directory for a system
#'
#' You can explicitly set the directory by providing the argument
#' \code{.top.dir} with a path, or you can call the function
#' without an argument and \code{top.dir} will make an operating-system
#' specific guess as to the appropriate path.
#'
#' This is necessary for the \code{sweave} script's operation
#' because it needs to know the top level and set it before
#' it calls R.
#'
#' \code{.top.dir} is usually \code{~/tc} on Linux and \code{c:/tc} on Windoze.
#'
#' Regardless, once \code{top.dir} is called it assigns
#' the chosen path to the variable \code{.top.dir} in \code{.GlobalEnv}
#'
#' Making a call to \code{top.dir} with no arguments will return
#' the value of the variable \code{.top.dir} that has been assigned
#' in \code{.GlobalEnv}, i.e. it acts an enquiry only.
#'
#'   @param .top.dir \code{\link{character}} directory name other than default
#'
#' 	\code{\link{.top.dir}}
#'
#'	Why does \code{top.dir} assign the variable .top.dir to
#'	\code{.GlobalEnv}?
#'	This is simply because 	\code{top.dir} takes an argument
#'	\code{.top.dir} which can be different from the default.
#'	Typically in a project the top-level directory is defined once,
#'	and this may be different from the default, say "c:/" on Windows
#'	instead of "c:/tc". Instead of calling \code{top.dir} as
#'	\code{top.dir(.top.dir="c:")}  this call can be made once at
#'	the start of the code block and subsequent references to it
#'	can be made via the variable \code{.top.dir} instead of
#'	calling the function explicitly each time as
#'	\code{top.dir(.top.dir="c:")}. This isolates the change in
#'	the top-level directory to the variable \code{.top.dir}.
#'
#'	Thus the calling convention is to establish the top-level
#'	directory with a call to \code{top.dir} either explicitly
#'	naming the top-level directory, e.g.
#'	\code{top.dir(.top.dir="c:")} or relying on the default
#'	for the system using \code{top.dir()}. After this all
#'	references to the top-level directory can be made through
#'	the variable \code{.top.dir}.
#'
#' @author Thomas P. Harte
#' @seealso \code{\link{Sys.info}}
#' @examples
#' 	# set the top-level directory to the system default
#' 	# (every project should start this way, the value .top.dir
#' 	# is assigned in .GlobalEnv
#' 	`check.top.dir`<- function() {
#' 		print( whos(sort="Name", env=.GlobalEnv) )
#' 		if (exists(".top.dir", env=.GlobalEnv)) {
#'			get(".top.dir", env=.GlobalEnv)
#'		}
#'		else {
#'			# guess the top-level directory depending on the OS:
#'			top.dir()
#'			get(".top.dir", env=.GlobalEnv)
#'		}
#'	}
#'	check.top.dir()
#'
#' @keywords \code{\link{Sys.info}}
#'
#' @export
`top.dir`<- function(.top.dir=NULL) {
	# unless an explicit .top.dir is specified in the argument list,
	# take the one in .GlobalEnv if it exists:
	if (is.null(.top.dir) & exists(".top.dir", env=.GlobalEnv)) {
		return( get(".top.dir", env=.GlobalEnv) )
	}
	# nothing specified anywhere, so make some reasonable guesses:
	else if (is.null(.top.dir) & !exists(".top.dir", env=.GlobalEnv)) {
		switch(Sys.info()["sysname"],
			Windows = { .top.dir<- "c:/tc" },
			Linux   = { .top.dir<- "~/tc" }
		)
	}
	assign(".top.dir", .top.dir, .GlobalEnv)

	return(.top.dir)
}
