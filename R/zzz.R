.First.lib<- function(libname, pkgname) {
# TODO: This mechanism for loading dependent libraries has been 
#       almost entirely superseded by using the 'Depends:' field in the 
#       'DESCRIPTION' file of a package.
	require(zoo, quietly=FALSE)
	

	switch(Sys.info()["sysname"],
		Windows = { 
			require(xlsReadWrite, quietly=FALSE) 
		},
		Linux = { 
			`windows`<- function(width=7, height=7) x11("", width, height)
			assign("windows", windows, .GlobalEnv)
		}
	)

	# library.dynam("tutils", pkgname, libname)

	# unfurl the start-up banner
	str<- read.dcf(file.path(libname, pkgname, "DESCRIPTION"), 
			fields=c("Package", "Version", "Description"))
	package.name<-        str[,"Package"]
	package.version<-     str[,"Version"]
	package.description<- str[,"Description"]
	cat(sprintf("Package '%s'", package.name), "version", package.version, "loaded\n")
	cat(package.description, "\n")
}

.Last.lib<- function(libpath) {
	# library.dynam.unload("Commodities", libpath)
	# print out some random message:
	# `windows`<- function(width, height) x11("", width, height)
	# 	cat("\ngoodbye, cruel world...\n")
}

