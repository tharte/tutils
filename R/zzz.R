.onLoad<- function(libname, pkgname) {
    # FIXME: This mechanism for loading dependent libraries has been
    #       almost entirely superseded by using the 'Depends:' field in the
    #       'DESCRIPTION' file of a package.
    #	require(zoo, quietly=FALSE)
    switch(Sys.info()["sysname"],
        Windows = {
        # FIXME: remove all xlsReadWrite : upgrade to openxlsx only
            if (0) {
                require(xlsReadWrite, quietly=FALSE)
            }
        },
        Linux = {
            # the (default) type="cairo" for x11 is an antialiasing
            # graphics engine that is woefully slow on Ubuntu 9.04; type="Xlib"
            # produces horridly retro bitmapped graphics; type="nbcairo" is good:
            `windows`<- function(width=7, height=7) x11("", width, height, type="nbcairo")
            assign("windows", windows, .GlobalEnv)
        }
    )

    # unfurl the start-up banner
    str<- read.dcf(
        file.path(libname, pkgname, "DESCRIPTION"),
        fields=c("Package", "Version", "Description")
    )
    package.name<-        str[,"Package"]
    package.version<-     str[,"Version"]
    package.description<- str[,"Description"]
    cat(sprintf("Package '%s'", package.name), "version", package.version, "loaded\n")
    cat(package.description, "\n")
}

.onUnload<- function(libpath) {
    # library.dynam.unload("Commodities", libpath)
    # print out some random message:
    # `windows`<- function(width, height) x11("", width, height)
    # 	cat("\ngoodbye, cruel world...\n")
}
