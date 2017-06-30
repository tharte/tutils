`copy.vars`<- function(fromEnvir, toEnvir=.GlobalEnv, vars=ls(envir=fromEnvir)) { 
	sapply(vars, function(var) put.var(var, fromEnvir=fromEnvir, toEnvir=toEnvir))

	return(invisible())
}

