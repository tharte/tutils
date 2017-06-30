`put.var`<- function(var, fromEnvir=parent.frame(), toEnvir=.GlobalEnv) { 
	assign(var, get(var, envir=fromEnvir), envir=toEnvir)

	return(invisible())
}
