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
