`save.zoo`<- 
function (z, 
	file=character(0), 	# filename (with extension)
	append=FALSE, 		# append data to existing file?
		...
) {
	`make.xsv.data.frame`<- function(z) {
		if (!is.null(colnames(z))) 
			cn<- colnames(z)
		else
			cn<- paste(1:NCOL(z), sep="")
		out<- as.data.frame(z)
		rows<- rownames(out)
		rownames(out)<- NULL
		out<- data.frame(rows, out, stringsAsFactors=FALSE)
		colnames(out)<- c("Date", cn)
		return(out)
	}
	stopifnot(is.zoo(z))
	if (length(file)) {
		ext<- getExtension(file)
		if (length(ext)) {
			base<- getExtension(file, parse=TRUE)$base
			switch(tolower(ext),
# TODO: `save.zoo.as.csv`<- function(z, file, digits=NULL, nsmall=0, ...) {
				"psv" = { 	
					write.table(make.xsv.data.frame(z), 
						file=file, quote=FALSE, row.names=FALSE, sep="|", append=append, ...)
				},
# TODO: `save.zoo.as.csv`<- function(z, file, digits=NULL, nsmall=0, ...) {
				"csv" = { 	
					write.csv(make.xsv.data.frame(z), 
						file=file, quote=FALSE, row.names=FALSE, append=append, ...)
				},
				"txt" = { 	
					write.table(make.xsv.data.frame(z), 
						file=file, quote=FALSE, sep="\t", ...)
				},
				"xls" = { 	
					switch(Sys.info()["sysname"],
						Windows = { 
							require(xlsReadWrite, quietly=FALSE) 
							write.xls(as.data.frame(z), file=file, ...)
						}
					)
				},
				"rdata" = { 	
					# z<- as.character(substitute(z))
					# save(get(z, envir=environment()), file=file, ...)
					# TODO: this saves the object as 'z' ...
					save(z, file=file, ...)
				}
			)
		}
		else error(sprintf("'%s' has no file extension ... cannot save data", file))
	}
	else error("'file' is empty ... cannot save data")
}

