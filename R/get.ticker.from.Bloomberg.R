`get.ticker.from.Bloomberg`<- 
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
	if (length(file)) save.zoo(z, file=file)
	return(z[-which(index(z)==remove.date),])
}

