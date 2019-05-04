#' Set the top-level directory for a system
#'
#' You can explicitly set the directory by providing the argument
#' \code{.top.dir} with a path, or you can call the function
#' without an argument and \code{top_dir} will make an operating-system
#' specific guess as to the appropriate path.
#'
#' This is necessary for the \code{sweave} script's operation
#' because it needs to know the top level and set it before
#' it calls R.
#'
#' \code{.top.dir} is usually \code{~/tc} on Linux and \code{c:/tc} on Windoze.
#'
#' Regardless, once \code{top_dir} is called it assigns
#' the chosen path to the variable \code{.top.dir} in \code{.GlobalEnv}
#'
#' Making a call to \code{top_dir} with no arguments will return
#' the value of the variable \code{.top.dir} that has been assigned
#' in \code{.GlobalEnv}, i.e. it acts an enquiry only.
#'
#'   @param .top.dir \code{\link{character}} directory name other than default
#'
#' 	\code{\link{.top.dir}}
#'
#'	Why does \code{top_dir} assign the variable .top.dir to
#'	\code{.GlobalEnv}?
#'	This is simply because 	\code{top_dir} takes an argument
#'	\code{.top.dir} which can be different from the default.
#'	Typically in a project the top-level directory is defined once,
#'	and this may be different from the default, say "c:/" on Windows
#'	instead of "c:/tc". Instead of calling \code{top_dir} as
#'	\code{top_dir(.top.dir="c:")}  this call can be made once at
#'	the start of the code block and subsequent references to it
#'	can be made via the variable \code{.top.dir} instead of
#'	calling the function explicitly each time as
#'	\code{top_dir(.top.dir="c:")}. This isolates the change in
#'	the top-level directory to the variable \code{.top.dir}.
#'
#'	Thus the calling convention is to establish the top-level
#'	directory with a call to \code{top_dir} either explicitly
#'	naming the top-level directory, e.g.
#'	\code{top_dir(.top.dir="c:")} or relying on the default
#'	for the system using \code{top_dir()}. After this all
#'	references to the top-level directory can be made through
#'	the variable \code{.top.dir}.
#'
#' @author Thomas P. Harte
#' @seealso \code{\link{Sys.info}}
#' @examples
#' 	# set the top-level directory to the system default
#' 	# (every project should start this way, the value .top.dir
#' 	# is assigned in .GlobalEnv
#' 	`check_top_dir`<- function() {
#' 		print( whos(sort="Name", env=.GlobalEnv) )
#' 		if (exists(".top.dir", env=.GlobalEnv)) {
#'			get(".top.dir", env=.GlobalEnv)
#'		}
#'		else {
#'			# guess the top-level directory depending on the OS:
#'			top_dir()
#'			get(".top.dir", env=.GlobalEnv)
#'		}
#'	}
#'	check_top_dir()
#'
#' @keywords \code{\link{Sys.info}}
#'
#' @export
`top_dir`<- function(.top.dir=NULL) {
	# unless an explicit .top.dir is specified in the argument list,
	# take the one in .GlobalEnv if it exists:
	if (is.null(.top.dir) & exists(".top.dir", env=.GlobalEnv)) {
		return( get(".top.dir", env=.GlobalEnv) )
	}
	# nothing specified anywhere, so make some reasonable guesses:
	else if (is.null(.top.dir) & !exists(".top.dir", env=.GlobalEnv)) {
		switch(Sys.info()["sysname"],
			Windows = { .top.dir<- "c:" },
			Linux   = { .top.dir<- "~/" }
		)
	}
	assign(".top.dir", .top.dir, .GlobalEnv)

	return(.top.dir)
}

#' Get ticker from Bloomberg.
#'
#' Returns the selected tickers from Bloomberg
#'
#' @param  tickers \code{\link{character}} Bloomberg tickers to fetch
#' @param  field \code{\link{character}} Bloomberg data field
#' @param  block \code{\link{integer}}
#'		number of calendar years of data to fetch at a time
#'		(if both \code{start} and \code{end} are \code{NULL}
#'		then data are fetched with this block size from as
#'		far back as possible to the latest date possible in
#'		blocks of size \code{block})
#' @param  start \code{\link{character}} or \code{\link{Date}}.
#'		starting date of complete data block: if \code{NULL}
#'		signals to fetch data as far back as possible
#' @param  end \code{\link{character}} or \code{\link{Date}}
#'		end date of complete data block: if \code{NULL}
#'		signals to fetch data to the latest date possible
#' @param  file \code{\link{character}} filename (with extension) to save data
#'
#' @return \code{\link{zoo}}
#'
#' @author Thomas P. Harte
#'
#' @keywords \code{\link{sort}}
#'
#' @seealso \code{\link{blpGetData}}, \code{\link{blpConnect}}, \code{\link{RBloomberg}}
#'
#' @examples
#'   \dontrun{
#'	 ## Not run:
#'	 ## get_ticker_from.Bloomberg("INDU INDEX", field="PX_LAST")
#'	 ## get_ticker_from.Bloomberg("INDU INDEX", start="2008-01-01")
#'	 ## get_ticker_from.Bloomberg("INDU INDEX", start="2008-01-01", end="2008-10-31")
#'   }
#'
#' @export
`get_ticker_from_Bloomberg`<-
function(tickers, 		# Bloomberg ticker symbols
	field="PX_LAST", 	# Bloomberg data field
	block=5,		# block size (in years)
	start=NULL,		# start of data block (over-rides 'block' if supplied)
	end=Sys.Date(),		# end of data block
	file=character(0)	# filename (with extension) to save data
) {
	require(RBloomberg)
	## by default establish connection via Desktop COM API
	conn <- blpConnect()
	#
	if (missing(end)) end<- Sys.Date()-1	# <- read from today - 1 (won't have today's last yet)
	`get.valid.date`<- function(date, default.date, name="") {
		if (class(date)=="Date")
			return(date)
		if (is.null(date))
			return(default.date)
		errMsg<- sprintf("%s must be of class 'Date' or a
			character string representing a 'Date'", ifelse(name="", "Date", name))
		if (is.character(date)) {
			try(date<- as.Date(date), silent=TRUE)
			if (class(.Last.value)=="try-error")
				stop(errMsg)
			return(date)
		}
		stop(errMsg)
	}
	end<- get.valid.date(end, default.date=Sys.Date(), "'end'")

	remove.date<- end+1
	ncols<- length(tickers)
	z<- zoo(matrix(NA, nr=1, nc=ncols), order.by=remove.date)
	colnames(z)<- tickers
	cat("starting to read the following tickers:\n")
	cat(paste(tickers,sep=","), "\n")

	# do we have a pre-defined start & end point (end point is already defined
	# by default, so this applies if a start date is defined)?
	if (!is.null(start)) {
		cat(sprintf("reading tickers between dates: %s and %s\n",
			format(start,"%Y-%m-%d"), format(end,"%Y-%m-%d")))
		tmp<- blpGetData(conn, tickers, field,
			start=as.chron(as.POSIXct(sprintf("%s 00:00:00",as.character(start)))),
			end=as.chron(as.POSIXct(sprintf("%s 00:00:00",as.character(end)))))
		tmp<- zoo(coredata(tmp), order.by=as.Date(index(tmp)))
		colnames(tmp)<- colnames(z)
		z<- rbind(z, tmp)
	}
	# otherwise, keep reading until the end
	else {
		# start<- get.valid.date(start, default.date=end-block*365, "'start'")
		while (1) {
			s<- sprintf("%d-%s", as.integer(format(end, "%Y"))-block, format(end, "%m-%d"))
			try(start<- as.Date(s), silent=TRUE)
			if (class(.Last.value)=="try-error") {
				while (class(.Last.value)=="try-error") {
					end<- end-1
					s<- sprintf("%d-%s", as.integer(format(end, "%Y"))-block, format(end, "%m-%d"))
					try(start<- as.Date(s), silent=TRUE)
				}
			}
			cat(sprintf("reading tickers between dates: %s and %s\n",
				format(start,"%Y-%m-%d"), format(end,"%Y-%m-%d")))
			tmp<- blpGetData(conn, tickers, field,
				start=as.chron(as.POSIXct(sprintf("%s 00:00:00",as.character(start)))),
				end=as.chron(as.POSIXct(sprintf("%s 00:00:00",as.character(end)))))
			# terminate loop if everything in this data block is NA:
			all.na<- sum(is.na(tmp))==prod(dim(tmp))
			if (all.na) break
			tmp<- zoo(coredata(tmp), order.by=as.Date(index(tmp)))
			colnames(tmp)<- colnames(z)
			z<- rbind(z, tmp)
			# DANGER: assumes daily data
			end<- start-1
		}
		# 'rbind' removes 'colnames' from Nx1 column vector; force it:
		if (ncols==1) {
			z<- zoo(matrix(coredata(z), nr=length(z), nc=1), order.by=index(z))
			colnames(z)<- tickers
		}
	}

	## disconnect!!!
	blpDisconnect(conn)
	if (length(file)) save_zoo(z, file=file)
	return(z[-which(index(z)==remove.date),])
}

#' Asserts that the condition(s) is true
#'
#' Asserts that the condition(s) is true
#'
#' @author Thomas P. Harte (inspired by the Rev Miguel Weylandto)
#'
#' @keywords \code{\link{stopifnot}}
#'
#' @seealso \code{\link{stopifnot}}
#'
#' @examples
#'   assert(is.data.frame(data.frame()))
#'
#' @export
`assert`<- base::stopifnot


#' Negates \code{\link{\%in\%}}
#'
#' Negates \code{\link{\%in\%}}
#'
#' @author Thomas P. Harte
#'
#' @keywords \code{\link{\%in\%}}, \code{\link{Negate}}
#'
#' @seealso \code{\link{\%in\%}}, \code{\link{Negate}}
#'
#' @examples
#'   assert("a" %not.in% letters[2:length(letters)])
#'   "a" %not.in% letters[2:length(letters)]
#'   "a" %not.in% letters
#'
#' @export
`%not.in%`<- Negate(`%in%`)


#' Checks for even / odd number
#'
#' Checks for even / odd number
#'
#' @author Thomas P. Harte
#'
#' @keywords \code{\link{\%\%}}
#'
#' @seealso \code{\link{\%\%}}
#'
#' @examples
#'   is_even(1:10)
#'   is_odd(1:10)
#'
#' @export
#' @name is_even
`is_even`<- function(x) {
	assert(is.numeric(x) | is.integer(x))

	!x %% 2
}


#' @export
#' @rdname is_even
`is_odd`<- function(x) {
	!is_even(x)
}


#' Get the absolute path of a file name
#'
#' Get the absolute path of a file name
#'
#' @param  filename \code{\link{character}} string - file name
#'
#' @return \code{\link{character}} string - absolute path of file name
#'
#' @author Thomas P. Harte
#'
#' @keywords \code{\link{path}}, \code{\link{dirname}}, \code{\link{basename}}
#'
#' @seealso \code{\link{path}}, \code{\link{dirname}}, \code{\link{basename}}
#'
#' @examples
#'   make_filename<- function() {
#'       if (.Platform$OS.type=="windows") {
#'           filename<- "u:/foo/bar.csv"
#'       }
#'       else if (.Platform$OS.type=="unix") {
#'           filename<- "~/foo/bar.csv"
#'       }
#'       else {
#'           stop(sprintf("%s not recognized", .Platform$OS.type))
#'       }
#'
#'       filename
#'   }
#'   (filename<- make_filename())
#'   absolute_path(filename)
#'
#' @export
`absolute_path`<- function(filename) {
	### works on .Platform$OS.type=="unix" & .Platform$OS.type=="windows" too

	file.path(dirname(filename), basename(filename))
}


#' Use system \code{cut} (*NIX only)
#'
#' Use system \code{cut} (*NIX only). For large files this is much faster
#' than reading the file into R and cutting the columns.
#'
#' @param  cols \code{\link{integer}} \code{\link{vector}} index of columns to cut
#' @param  filename \code{\link{character}} file name
#' @param  sep \code{\link{character}} delimiter (passed to \code{cut})
#'
#' @return \code{\link{character}} \code{\link{vector}} output from \code{cut}
#'
#' @author Thomas P. Harte
#'
#' @keywords \code{\link{system}}, \code{\link{cut}}
#'
#' @seealso \code{\link{system}}, \code{\link{cut}}
#'
#' @examples
#'	tab<- read.csv(text='Name,Age,Salary,ID
#'             Dick,38,32k,1
#'             Tom,21,21k,2
#'             Harry,56,NA,3',
#'             header=TRUE,
#'             stringsAsFactors=FALSE
#'    )
#'    for (col in which(col_classes(tab)=="character"))
#'        tab[, col]<- tutils::trim(tab[, col])
#'    tab
#'
#'    filename<- paste(tempfile(), ".csv", sep="")
#'    write.csv(tab, file=filename, row.names=FALSE, quote=FALSE)
#'    cut_system(1, filename, sep=",")
#'    cut_system(4, filename, sep=",")
#'    cut_system(c(1,4), filename, sep=",")
#'    unlink(filename)
#'
#' @export
`cut_system`<- function(cols, filename, sep="|") {
    assert(
        .Platform$OS.type=="unix",
        is.integer(cols) | is.numeric(cols),
        file.exists(filename)
    )

    cols<- ifelse(length(cols)>1,
        paste(as.character(cols),collapse=","),
        as.character(cols)
    )

    if (get_file_ext(filename)=="gz") {
        command<- sprintf("gunzip -c %s | cut -f%s -d'%s'", filename, cols, sep)
    }
    else {
        command<- sprintf("cut -f%s -d'%s' %s", cols, sep, filename)
    }

    system(command, intern=TRUE)
}


#' Use system \code{grep} (*NIX only)
#'
#' Use system \code{grep} (*NIX only). For large files this is much faster
#' than reading the file into R and grepping the columns.
#'
#' @param  pattern \code{\link{character}} to search
#' @param  filename \code{\link{character}} file name
#' @param  options \code{\link{character}} passed to \code{grep}
#'
#' @return \code{\link{character}} \code{\link{vector}} output from \code{cut}
#'
#' @author Thomas P. Harte
#'
#' @keywords \code{\link{system}}, \code{\link{cut}}
#'
#' @seealso \code{\link{system}}, \code{\link{cut}}
#'
#' @examples
#'	tab<- read.csv(text='Name,Age,Salary,ID
#'             Dick,38,32k,1
#'             Tom,21,21k,2
#'             Harry,56,NA,3',
#'             header=TRUE,
#'             stringsAsFactors=FALSE
#'    )
#'    for (col in which(col_classes(tab)=="character"))
#'        tab[, col]<- tutils::trim(tab[, col])
#'    tab
#'
#'    filename<- paste(tempfile(), ".csv", sep="")
#'    write.csv(tab, file=filename, row.names=FALSE, quote=FALSE)
#'    grep_system("Tom", filename)
#'    grep_system("Dick", filename)
#'    grep_system("harry", filename, options="-i")
#'
#'    unlink(filename)
#'
#' @export
`grep_system`<- function(pattern, filename, options="") {
    assert(
        .Platform$OS.type=="unix",
        file.exists(filename)
    )

    pattern<- as.character(pattern)
    .grep<-   ifelse(get_file_ext(filename)=="gz", "zgrep", "grep")
    command<- sprintf("%s %s '%s' %s", .grep, options, pattern, filename)

    system(command, intern=TRUE)
}


#' Use system \code{pdflatex} (*NIX only) to compile tikz output
#'
#' Use system \code{pdflatex} (*NIX only) to compile tikz output. 
#' \code{\link{pdftikz}} will save the current directory, change
#' to the directory of the filename and execute pdflatex on the 
#' tikz output to produce a pdf file
#'
#' @param  filename \code{\link{character}} file name
#'
#' @return output of \code{tryCatch} block
#'
#' @author Thomas P. Harte
#'
#' @keywords \code{\link{system}}, \code{pdflatex}
#'
#' @seealso \code{\link{system}}
#'
#' @examples
#' # reproduce cover PDF from:
#' #     https://cran.r-project.org/web/packages/tikzDevice/vignettes/tikzDevice.pdf
#' d<- tempdir()
#' filename<- file.path(d, 'cover-pdf.tex')
#' require(tikzDevice)
#' tikz(file=filename, width=4, height=4, standAlone=TRUE)
#' par(las=1)
#' 
#' x <- seq(-4.5,4.5,length.out=100)
#' y <- dnorm(x)
#' 
#' xi <- seq(-2,2,length.out=30)
#' yi <- dnorm(xi)
#' 
#' plot(x,y,type='l',col='blue',ylab='$p(x)$',xlab='$x$')
#' lines(xi,yi,type='s')
#' lines(range(xi),c(0,0))
#' lines(xi,yi,type='h')
#' title(main="$p(x)=\\frac{1}{\\sqrt{2\\pi}}e^{-\\frac{x^2}{2}}$")
#' int <- integrate(dnorm,min(xi),max(xi),subdivisions=length(xi))
#' text(2.8, 0.35,
#'     paste(
#' 	"\\small$\\displaystyle\\int_{",min(xi),"}^{",max(xi),"}p(x)dx",
#' 	"\\approx", round(int[['value']], 3), '$', 
#'         sep=''
#'     )
#' )
#' 
#' dev.off()
#' pdftikz(filename)
#'
#' @export
`pdftikz`<- function(filename) {
    assert(file.exists(filename))
    od<- getwd()

    o<- tryCatch({
            nd<- dirname(filename)
	    cmd<- sprintf("cd %s && pdflatex %s", nd, basename(filename))
	    system(cmd)
	},
	error = function(e) {
	    print(paste('pdftikz: ', e))
	    stop(sprintf('pdftikz: could not compile %s', filename))
	},
	finally = {
	    setwd(od)
	}
    )

    o
}
