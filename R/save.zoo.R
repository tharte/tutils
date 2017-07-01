#' Save zoo object to named extension file
#'
#' Saves a \code{\link{zoo}} object to a file with a named extension
#'
#'  @param z \code{\link{zoo}}
#' 	  a zoo object (\code{\link{coredata}} can be multi-column)
#'  @param file \code{\link{character}}
#' 	  filename in which to save \code{z} with the file extension being one of:
#'         \itemize{
#'                 \item{"psv"}{pipe-separated values}
#'                 \item{"csv"}{comma-separated values}
#'                 \item{"txt"}{tab-separated values}
#'                 \item{"xls"}{Excel spreadsheet}
#'                 \item{"Rdata"}{R binary file}
#'         }
#'
#' @author Thomas P. Harte
#'
#' @keywords \code{\link{zoo}}
#'
#' @seealso \code{\link{save}},
#'          \code{\link{write.table}},
#' 	        \code{\link{write.csv}},
#'          \code{\link{write.xls}},
#'          \code{\link{getExtension}},
#'          \code{\link{zoo}}
#'
#' @examples
#' 	n<- 10
#' 	dates<- seq(as.Date("2008-10-31"), by=1, len=n)
#' 	z<- zoo(matrix(c(rnorm(n, sd=.1), rnorm(n, sd=.5), rnorm(n, sd=1)), nc=3),
#' 		order.by=dates)
#' 	basename<- tempfile("z")
#'
#' 	# save as a PSV file:
#' 	filename<- paste(basename, ".psv", sep="")
#' 	save.zoo(z, file=filename)
#' 	# note: column names added by default:
#' 	file.show(filename)
#'
#' 	# add column names:
#' 	colnames(z)<- c("one", "two", "three")
#' 	# this time save as a CSV file:
#' 	filename<- paste(basename, ".csv", sep="")
#' 	save.zoo(z, file=filename)
#' 	file.show(filename)
#'
#' 	# this time save as a TXT file:
#' 	filename<- paste(basename, ".txt", sep="")
#' 	save.zoo(z, file=filename)
#' 	file.show(filename)
#'
#' 	filename<- paste(basename, ".Rdata", sep="")
#' 	save.zoo(z, file=filename)
#' 	# get rid of 'z':
#' 	rm(z)
#' 	# restore 'z':
#' 	print(load(filename))
#' 	# restore 'z' as 'foo':
#' 	foo<- loadAs(filename)
#' 	stopifnot(identical(z, foo))
#'
#' @export
`save.zoo`<- function (z,
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
