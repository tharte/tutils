`ht`<- 
function (x, n = 6, ...) {
	if (any("list" %in% class(x))) {
		if (is.null(names(x)))
			    nm <- paste("[[", 1:length(x), "]]", sep = "")
		else
			    nm <- paste("$", names(x), sep = "")
		for (i in 1:length(x)) {
			    cat(sprintf("\n%s", nm[i]))
			    ht(x[[i]], n, ...)
		}
	}
	else {
		d <- dim(x)
		if (is.null(d))
			    d <- length(x)
		else
			    d <- d[1]
		if (d > 2 * n) {
			    cat("\n***>> head <<***\n")
			    print(head(x, n, ...))
			    cat("\n***>> tail <<***\n")
			    print(tail(x, n, ...))
		}
		else {
			    cat("\n***>> all <<***\n")
			    print(x)
		}
	}
}

