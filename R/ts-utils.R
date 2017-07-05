#' Check if the object is of class \code{\link{Date}}
#'
#' Check if the object is of class \code{\link{Date}}
#'
#' @param  x  object
#'
#' @return \code{\link{logical}}
#'
#' @author Thomas P. Harte
#'
#' @keywords \code{\link{Date}}
#'
#' @seealso \code{\link{Date}}
#'
#' @examples
#'   require(zoo)
#'   is_Date(as.Date("2017-07-04"))
#'   is_Date("2017-07-04")
#'
#'   x<- seq(as.Date("2017-07-01"), len=4, by=1)
#'   is_Date(x)
#'   is_Date(as.character(x))
#'
#' @export
`is_Date`<- function(x) {
	# paste(class(x), collapse=", ")=="Date"
	inherits(x, "Date")
}


#' Check if the object is of class \code{\link{yearmon}}
#'
#' Check if the object is of class \code{\link{yearmon}}
#'
#' @param  x  object
#'
#' @return \code{\link{logical}}
#'
#' @author Thomas P. Harte
#'
#' @keywords \code{\link{yearmon}}
#'
#' @seealso \code{\link{yearmon}}
#'
#' @examples
#'   require(zoo)
#'   is_yearmon(as.yearmon("2017-07-04"))
#'   is_yearmon("2017-07-04")
#'
#'   x<- as.yearmon(seq(as.Date("2017-07-01"), len=4, by=1))
#'   is_yearmon(x)
#'   is_yearmon(as.character(x))
#'
#'
#' @export
`is_yearmon`<- function(x) {
    inherits(x, "yearmon")
}


#' Compute (simple net) return of values in vector
#'
#' Compute (simple net) return of values in vector
#'
#' @param  x \code{\link{numeric}} \code{\link{vector}}
#' @param  nan.replace \code{\link{logical}} if TRUE, replace \code{\link{NaN}} (not-a-number) symbols with \code{NA}
#'
#' @return \code{\link{numeric}} \code{\link{vector}}
#'
#' @author Thomas P. Harte
#'
#' @keywords \code{\link{numeric}}, \code{\link{NaN}}, \code{\link{vector}}
#'
#' @seealso \code{\link{numeric}}, \code{\link{NaN}}, \code{\link{vector}}
#'
#' @examples
#'   require(zoo)
#'   get_return(c(NA))
#'   get_return(c(NA, NA))
#'   get_return(c(NaN, NA))
#'   get_return(c(NaN, 1, 1.1), nan.replace=TRUE)
#'   get_return(c(1, +Inf, 1.1), nan.replace=TRUE)
#'
#'   dates<- seq(as.Date("2011-01-01"), by=1, len=6)
#'	 z<- zoo(c(NA, 1, 2, 3, 0, 0), order.by=dates)
#'
#'   zoo::rollapply(z, 2, get_return, align="right", nan.replace=TRUE)
#'   # 2011-01-02 2011-01-03 2011-01-04 2011-01-05 2011-01-06
#'   #	  NA        1.0        0.5       -1.0         NA
#'
#'   zoo::rollapply(z, 2, get.return, align="right", nan.replace=FALSE)
#'   # 2011-01-02 2011-01-03 2011-01-04 2011-01-05 2011-01-06
#'   #	  NA        1.0        0.5       -1.0         NaN
#'
#' @export
`get_return`<- function(
    x,
	nan.replace=FALSE
) {
	if (is.zoo(x))
        x<- as.numeric(x)
	x<- na.omit(x)
	n<- length(x)

	# we can only take a return when we have values for x[1] & x[2]
	# regardless of whether or not they are one of:
	#	0, NA, NaN, +Inf, -Inf

	if (n<2) return(NA)

	# if the values of x[1] and x[n] are one of:
	#	0, NA, NaN, +Inf, -Inf
	# then a non-a-number symbol will result

	r<- x[n]/x[1]-1

	if (nan.replace) {
		if (is.nan(r) | is.infinite(r))
			return (NA)
	}

	return(r)
}


#' Compute the difference of values in vector
#'
#' Compute the difference of values in vector
#'
#' @param  x \code{\link{numeric}} \code{\link{vector}}
#'
#' @return \code{\link{numeric}} \code{\link{vector}}
#'
#' @author Thomas P. Harte
#'
#' @keywords \code{\link{numeric}}, \code{\link{diff}}
#'
#' @seealso \code{\link{numeric}}, \code{\link{diff}}
#'
#' @examples
#'   require(zoo)
#'   get_diff(c(NA))
#'   get_diff(c(NA, NA))
#'   get_diff(c(NaN, NA))
#'   get_diff(c(NaN, 1, 1.1))
#'   get_diff(c(1, +Inf, 1.1))
#'
#'   dates<- seq(as.Date("2011-01-01"), by=1, len=6)
#'   z<- zoo(c(NA, 1, 2, 3, 0, 0), order.by=dates)
#'
#'   zoo::rollapply(z, 2, get_diff, align="right")
#'   # 2011-01-02 2011-01-03 2011-01-04 2011-01-05 2011-01-06
#'   #	  NA        1.0        1.0       -3.0         0
#'
#' @export
`get_diff`<- function(x) {
	if (is.zoo(x)) x<- as.numeric(x)
	x<- na.omit(x)
	n<- length(x)

	# we can only take a diff when we have values for x[1] & x[2]
	if (n<2) return(NA)

	# if the values of x[1] and x[n] are one of 0 or the not-a-number symbols:
	#	0, NA, NaN, +Inf, -Inf
	# then a NaN (non-a-number) symbol will result

	r<- x[n]-x[1]

	return(r)
}

#' Coerce character strings to the Date class
#'
#' Coerce character strings to Date class by judiciously guessing the strings' format
#'
#' @param  strs \code{\link{character}} string to convert to ISO 8601
#' @param  year.left \code{\link{logical}} give \code{to_ISO_8601} a hint: does
#'   the year occur on the left \code{TRUE} or on the right \code{FALSE}
#' @param  american \code{\link{logical}} does the string follow the bizarre American
#'   convention for writing dates, viz. MM-DD-YYYY
#' @return \code{\link{Date}} \code{\link{vector}}
#'
#' @author Thomas P. Harte
#'
#' @keywords \code{\link{Date}}
#'
#' @seealso \code{\link{Date}}, \url{https://xkcd.com/1179}
#'
#' @examples
#'    require(zoo)
#'    require(magrittr)
#'    require(dplyr)
#'    tab<-  tutils:::make_ISO_8601_test_table()
#'    tab %<>% mutate(`Output`=as.character(NA))
#'
#'    for (row in 1:nrow(tab)) {
#'        tab[row, "Output"]<- to_ISO_8601(
#'            str=tab[row, "Test"],
#'            year.left=tab[row, "year.left"],
#'            american=tab[row, "american"]
#'        ) %>% as.character
#'    }
#'
#'    (tab %<>% mutate(`success`=`Expected`==`Output`))
#'
#' @export
`to_ISO_8601`<- function(strs, year.left=TRUE, american=FALSE) {
    `.has.letters`<- function(x, only=FALSE) {
        return(.has(x, "[a-z]", only=only))
    }
    `.has.numbers`<- function(x, only=FALSE) {
        return(.has(x, "[0-9]", only=only))
    }
    `.has`<- function(x, srch, only=FALSE) {
        ix<- grep(srch, x)
        if (only) {
            if (length(ix)==length(x))
                return(TRUE)
        }
        else if (length(ix)) {
            return(TRUE)
        }

        return(FALSE)
    }
    `.standardize.strs`<- function(strs) {
        stopifnot(is.character(strs),
                  mode(strs)=="character"
        )
        # remove extraneous characters
        # FIXME: could "," be used as a separator, e.g. other than 'Mar 30, 2012'?
        strs<- tutils::trim(strs)
        rx<-  ","
        strs<- gsub(rx, "", strs)
        strs<- replace(strs, is_blank(strs) | is.na(strs), NA)

        .obj[["ix"]]<<- which(!is.na(strs))

        if (!length(.obj$ix)) {
            standardized.strs<- rep(as.character(NA), length(.obj$data.raw))
        }
        else {
            strs<- strs[.obj$ix]
            # transform the date sep to a standard sep
            # TODO: this doesn't account for date formats such as '20131212'
            #       of '2013Dec'
            rx<-  "[:[:space:]\\/-]"
            stopifnot(length(!is.na(grep(rx, strs)))==length(strs))
            standardized.strs<- tolower(gsub(rx, .obj$sep, strs))
        }

        return(standardized.strs)
    }
    `.data.elements`<- function() {
        elements<- strsplit(.obj$data.std, .obj$sep)
        elements<- lapply(elements, function(x) {
            sapply(x, function(x) {
            if (.has.numbers(x) & nchar(x)==1)
                return(paste("0",x,sep=""))
            return(x)
            })
        })
        if (length(unique(sapply(elements, length)))!=1)
            stopifnot("data.elements : not a string containing date elements")

        return(elements)
    }
    `.n.data.elements`<- function() {
        n.elements<- unique(sapply(.obj$data.elements, length))
        stopifnot(n.elements==2 | n.elements==3)

        return(n.elements)
    }
    `.nchar.data.elements`<- function() {
        nchar.elements<- sapply(.obj$data.elements, nchar)

        if (!any(apply(nchar.elements, 1, function(x) length(unique(x)))==1))
            stop("nchar.data.elements : could not parse date format")

        return(apply(nchar.elements, 1, function(x) unique(x)))
    }
    `.position.letters`<- function(x) {
        position<- sapply(.obj$data.elements, function(x) sapply(x, .has.letters))
        # letters should always occur in the same position in the same date element
        if (!any(apply(position, 1, sum) %in% c(0,ncol(position))))
            stop("position.letters : could not parse date format")

        # guaranteed to have at least one position with letters (but only a valid date if one)
        if (length(ix<- which(apply(position, 1, sum)>0))>1)
            stop("position.letters : could not parse date format")
        stopifnot(ix==1 | ix==2)

        return(ix)
    }
    `.position.numbers`<- function(x) {
        position<- sapply(.obj$data.elements, function(x) sapply(x, .has.numbers))
        # numbers should always occur in the same position in the same date element
        if (!any(apply(position, 1, sum) %in% c(0,ncol(position))))
            stop("position.numbers : could not parse date format")

        # guaranteed to have >= 1 position with numbers
        if (!length(ix<- which(apply(position, 1, sum)>0)) %in% 1:3)
            stop("position.numbers : could not parse date format")

        return(ix)
    }
    `.get.months`<- function() {
        c(
            "January",
            "February",
            "March",
            "April",
            "May",
            "June",
            "July",
            "August",
            "September",
            "October",
            "November",
            "December"
        )
    }
    `.element`<- function(position) {
        # CTOR:
        `.new`<- function(position) {
            stopifnot(position %in% 1:ncol(.obj$data.matrix))
            this<-               list()
            this[["position"]]<- position

            elem<- .obj$data.matrix[,position]

            if (.has.letters(elem, only=TRUE)) {
                elem<- substring(elem,1,3)
                stopifnot(all(elem %in% .obj$mths))

                this[["type"]]<-      "month"
                this[["specifier"]]<- "%B"
            }
            else  if (.has.numbers(elem, only=TRUE)) {
                if (all(nchar(elem)==1))
                    elem<- paste("0",elem,sep="")
                if (all(nchar(elem)==4)) {
                    this[["type"]]<-      "year"
                    this[["specifier"]]<- "%Y"
                }
                else if (all(nchar(elem)==2)) {
                    if (any(as.integer(elem)==0) | any(as.integer(elem) > 31)) {
                        this[["type"]]<-      c("year")
                        this[["specifier"]]<- c("%y")
                    }
                    else if (any(as.integer(elem)==0) | any(as.integer(elem) > 12)) {
                        this[["type"]]<-      c("year","day")
                        this[["specifier"]]<- c("%y","%d")
                    }
                    else {
                        this[["type"]]<-      c("year","month","day")
                        this[["specifier"]]<- c("%y","%m","%d")
                    }
                }
                else {
                    stop(sprintf("element : not a recognized date element - consider sapplying 'to.ISO.8601' to individual elements"))
                }
            }
            else {
                stop(sprintf("element : not a recognized date element - consider sapplying 'to.ISO.8601' to individual elements"))
            }
            names(this[["specifier"]])<- this[["type"]]
            class(this)<- "element"

            return(this)
        }
        # ACCESSORS:
        `.is.singleton`<- function() {
           return(ifelse(length(this[["type"]])==1, TRUE, FALSE))
        }
        `.is.tuple`<- function() {
           return(ifelse(length(this[["type"]])>1, TRUE, FALSE))
        }
        # MUTATORS:
        `.to.singleton`<- function(type=c("year","month","day")) {
           if (.is.singleton()) return(invisible())

           type<- match.arg(type)
           stopifnot(type %in% this[["type"]])
           this[["type"]]<<-      type
           this[["specifier"]]<<- this[["specifier"]][type]

           return(invisible())
        }
        `.prune`<- function(types) {
           if (.is.singleton() |
               length(intersect(types, this[["type"]]))==0
           ) return(invisible())

           stopifnot(all(types %in% this[["type"]]))
           rm.ix<-      which(this[["type"]] %in% types)
           this[["type"]]<<-      this[["type"]][-rm.ix]
           this[["specifier"]]<<- this[["specifier"]][-rm.ix]

           return(invisible())
        }
        .get<- function() return(this)
        this<- .new(position)

        return(list(
            get           =  .get,
            is.singleton  =  .is.singleton,
            is.tuple      =  .is.tuple,
            to.singleton  =  .to.singleton,
            prune         =  .prune
        ))
    }
    .is.element<- function(x) {
        return(class(x)=="element")
    }
    print.element<- function(x) {
        cat(sprintf("position:\t%d\n",  x[["position"]]))
        cat(sprintf("type:\t\t%s\n",      paste(x[["type"]],collapse=",")))
        cat(sprintf("specifier:\t%s\n", paste(x[["specifier"]],collapse=",")))

        return(invisible())
    }

    if (!length(strs))
        return(strs)

    .obj<- list()
    .obj[["sep"]]<-                       "-"
    .obj[["mths"]]<-                      substring(tolower(.get.months()),1,3)
    .obj[["data.raw"]]<-                  strs
    .obj[["data.std"]]<-                  .standardize.strs(.obj$data.raw)
    if (sum(is.na(.obj$data.std))==length(.obj$data.raw))
        return(as.Date(.obj$data.std))
    .obj[["data.elements"]]<-             .data.elements()
    # TODO: refactor this into 'data.elements'
    .obj[["data.matrix"]]<-               do.call("rbind", .obj$data.elements)
    .obj[["data.has.letters"]]<-          .has.letters(.obj$data.std)
    .obj[["data.nchar.elements"]]<-       .nchar.data.elements()
    .obj[["data.n.elements"]]<-           length(.obj$data.nchar.elements)
    .obj[["data.position.numbers"]]<-     .position.numbers()
    .obj[["data.position.letters"]]<-     .position.letters()
    .obj[["data.nchar.year"]]<-
        max(.obj$data.nchar.elements[.obj$data.position.numbers])

    .obj<- lapply(.obj, function(x) {
        names(x)<- NULL
        return(x)
    })

    stopifnot(all(.obj$data.nchar.elements[.obj$data.position.numbers]!=3))
    stopifnot(length(.obj$data.position.letters) %in% c(0,1))
    stopifnot(.obj$data.nchar.year %in% c(2,4))
    #stopifnot(.obj[["has.letters"]] & .obj[["position.letters"]] !=2 ) # force month to be in the middle
    # TODO: select substring(mth, 1, 3)
    if (.obj$data.has.letters)
        stopifnot(all(sapply(.obj$data.elements, "[", .obj$data.position.letters) %in% .obj$mths))

    elem<- list()
    for (position in 1:.obj$data.n.elements) {
        elem[[position]]<- .element(position)
    }
    .obj[["element"]]<-        elem
    `.update.singletons`<- function() {
        .obj[["singleton"]]<<-      sapply(.obj$element, function(x) x$is.singleton())
        .obj[["n.singleton"]]<<-    sum(.obj$singleton)

        return(invisible())
    }
    .update.singletons()

    `do.yearmon`<- function() {
        required.types<- c("year","month")
        available.types<- unique(unlist(sapply(.obj$element, function(x) x$get()$type)))
        stopifnot(all(required.types %in% available.types))

        rm.element<-   setdiff(available.types, required.types)
        sapply(.obj$element, function(x) x$prune(rm.element))-> junk
        .update.singletons()

        if (.obj$n.singleton >= 1) {#        : everything determined
            singleton.types<-    sapply(which(.obj$singleton), function(ix) .obj$element[[ix]]$get()$type)
            nonsingleton.types<- sapply(which(!.obj$singleton), function(ix) .obj$element[[ix]]$get()$type)
            rm.element<-          intersect(singleton.types, nonsingleton.types)
            sapply(.obj$element[which(!.obj$singleton)], function(x) x$prune(rm.element))-> junk
            .update.singletons()
        }
        else if (.obj$n.singleton == 0) {#   : apply rules
            if (year.left==TRUE) {
                   .obj$element[[1]]$to.singleton("year")
                   .obj$element[[2]]$to.singleton("month")
            }
            else {
                   .obj$element[[1]]$to.singleton("month")
                   .obj$element[[2]]$to.singleton("year")
            }
            .update.singletons()
        }
        stopifnot(all(.obj$singleton))

        format.string<- paste(sapply(.obj$element, function(x) x$get()$specifier), collapse=.obj$sep)
        out<- eval(parse(text=sprintf("as.yearmon(.obj$data.std, format=\"%s\")", format.string)))

        return(out)
    }
    `do.Date`<- function() {
        required.types<- c("year","month","day")
        available.types<- unique(unlist(sapply(.obj$element, function(x) x$get()$type)))
        stopifnot(all(required.types %in% available.types))

        rm.element<-   setdiff(available.types, required.types)
        sapply(.obj$element, function(x) x$prune(rm.element))-> junk
        .update.singletons()

        if (.obj$n.singleton >= 2) {#        : everything determined
            singleton.types<-    sapply(which(.obj$singleton), function(ix) .obj$element[[ix]]$get()$type)
            nonsingleton.types<- sapply(which(!.obj$singleton), function(ix) .obj$element[[ix]]$get()$type)
            rm.element<-          intersect(singleton.types, nonsingleton.types)
            sapply(.obj$element[which(!.obj$singleton)], function(x) x$prune(rm.element))-> junk
            .update.singletons()
        }
        else if (.obj$n.singleton == 1) {#   : apply rules
            singleton.type<-     sapply(which(.obj$singleton), function(ix) .obj$element[[ix]]$get()$type)
            nonsingleton.types<- sapply(which(!.obj$singleton), function(ix) .obj$element[[ix]]$get()$type)
            rm.element<-         intersect(singleton.type, nonsingleton.types)
            sapply(.obj$element[which(!.obj$singleton)], function(x) x$prune(rm.element))-> junk
            .update.singletons()

            if (singleton.type=="year") {
                # can't have a year in the middle
                year.position<- .obj$element[[which(.obj$singleton)]]$get()$position
                stopifnot(year.position!=2)

                if (year.position==1) {
                   .obj$element[[2]]$to.singleton("month")
                   .obj$element[[3]]$to.singleton("day")
                }
                else if (year.position==3) {
                    if (american==TRUE) {
                        # month-day-year: American convention MM-DD-YYYY
                       .obj$element[[1]]$to.singleton("month")
                       .obj$element[[2]]$to.singleton("day")
                    }
                    else {
                        .obj$element[[1]]$to.singleton("day")
                        .obj$element[[2]]$to.singleton("month")
                   }
                }
                .update.singletons()
            }
            else if (singleton.type=="month") {
                # can't have a year in the middle
                month.position<- .obj$element[[which(.obj$singleton)]]$get()$position

                if (month.position==1) {
                    # month-day-year: American convention MM-DD-YYYY
                    # this happens here by virtue of month.position==1
                   .obj$element[[2]]$to.singleton("day")
                   .obj$element[[3]]$to.singleton("year")
                }
                else if (month.position==2) {
                    if (year.left==TRUE) {
                        # year-month-day
                        .obj$element[[1]]$to.singleton("year")
                        .obj$element[[3]]$to.singleton("day")
                    }
                    else {
                        # day-month-year
                        .obj$element[[1]]$to.singleton("day")
                        .obj$element[[3]]$to.singleton("year")
                    }
                }
                else if (month.position==3) {
                    # can't have a year in the middle, so year.left is irrelevant
                    .obj$element[[1]]$to.singleton("year")
                    .obj$element[[2]]$to.singleton("day")
                }
                .update.singletons()
            }
        }
        else if (.obj$n.singleton == 0) {#   : apply rules
            if (american==TRUE) {
                # month-day-year: American convention MM-DD-YYYY
               .obj$element[[1]]$to.singleton("month")
               .obj$element[[2]]$to.singleton("day")
               .obj$element[[3]]$to.singleton("year")
            }
            else if (year.left==TRUE) {
                # year-month-day
               .obj$element[[1]]$to.singleton("year")
               .obj$element[[2]]$to.singleton("month")
               .obj$element[[3]]$to.singleton("day")
            }
            else {
                # day-month-year
                .obj$element[[1]]$to.singleton("day")
                .obj$element[[2]]$to.singleton("month")
                .obj$element[[3]]$to.singleton("year")
            }
            .update.singletons()
        }
        stopifnot(all(.obj$singleton))

        format.string<- paste(sapply(.obj$element, function(x) x$get()$specifier), collapse=.obj$sep)
        out<- eval(parse(text=sprintf("as.Date(.obj$data.std, format=\"%s\")", format.string)))

        return(out)
    }

    if (.obj$data.n.elements==2) {
        # TODO: extend this to cope with 'as.yearqtr'
        .obj[["out"]]<- do.yearmon()
        out<- as.Date(rep(as.character(NA), length(.obj$data.raw)))
        out[.obj$ix]<- .obj$out
        out<- as.yearmon(out)
    }
    else {
        .obj[["out"]]<- do.Date()
        out<- as.Date(rep(as.character(NA), length(.obj$data.raw)))
        out[.obj$ix]<- .obj$out
    }

    return(out)
}

`make_ISO_8601_test_table`<- function() {
    #FIXME
    #to.ISO.8601("32-4-32", american=FALSE)
    #Error: all(.obj$singleton) is not TRUE
    #> to.ISO.8601("32-4-31", american=FALSE)
    #[1] NA
    #> to.ISO.8601("32-4-31", american=FALSE)
    #[1] NA
    #> to.ISO.8601("32-04-31", american=FALSE)
    #[1] NA
    #> to.ISO.8601("33-04-31", american=FALSE)
    #[1] NA
    #> to.ISO.8601("33-04-31", year.left=TRUE)
    #[1] NA
    tab<- read.table(text=
           "Test|ISO 8601|Class|year.left|american
            Dec 13|2013-12|yearmon|TRUE|FALSE
            Dec 13|2013-12|yearmon|FALSE|FALSE
            Jan 01|2001-01|yearmon|TRUE|FALSE
            Jan 01|2001-01|yearmon|FALSE|FALSE
            13 Dec|2013-12|yearmon|TRUE|FALSE
            13 Dec|2013-12|yearmon|FALSE|FALSE
            01 Jan|2001-01|yearmon|TRUE|FALSE
            01 Jan|2001-01|yearmon|FALSE|FALSE
            Dec 2013|2013-12|yearmon|TRUE|FALSE
            Dec 2013|2013-12|yearmon|FALSE|FALSE
            Jan 2001|2001-01|yearmon|TRUE|FALSE
            Jan 2001|2001-01|yearmon|FALSE|FALSE
            2013 Dec|2013-12|yearmon|TRUE|FALSE
            2013 Dec|2013-12|yearmon|FALSE|FALSE
            2001 Jan|2001-01|yearmon|TRUE|FALSE
            2001 Jan|2001-01|yearmon|FALSE|FALSE
            12 13|2013-12|yearmon|TRUE|FALSE
            12 13|2013-12|yearmon|FALSE|FALSE
            01 01|2001-01|yearmon|TRUE|FALSE
            01 01|2001-01|yearmon|FALSE|FALSE
            2013/Dec/12|2013-12-12|Date|TRUE|FALSE
            2013/Dec/12|2013-12-12|Date|FALSE|FALSE
            2001/Jan/01|2001-01-01|Date|TRUE|FALSE
            2001/Jan/01|2001-01-01|Date|FALSE|FALSE
            12/Dec/2013|2013-12-12|Date|TRUE|FALSE
            12/Dec/2013|2013-12-12|Date|FALSE|FALSE
            01/Jan/2001|2001-01-01|Date|TRUE|FALSE
            01/Jan/2001|2001-01-01|Date|FALSE|FALSE
            13/12/12|2013-12-12|Date|TRUE|FALSE
            13/12/12|2012-12-13|Date|FALSE|FALSE
            01/01/01|2001-01-01|Date|TRUE|FALSE
            01/01/01|2001-01-01|Date|FALSE|FALSE
            01/04/14|2001-04-14|Date|TRUE|FALSE
            01/04/14|2014-04-01|Date|FALSE|FALSE
            01/04/14|2014-01-04|Date|TRUE|TRUE
            01/04/14|2014-01-04|Date|FALSE|TRUE
            12/12/13|2012-12-13|Date|TRUE|FALSE
            12/12/13|2013-12-12|Date|FALSE|FALSE
            01/01/01|2001-01-01|Date|TRUE|FALSE
            01/01/01|2001-01-01|Date|FALSE|FALSE
            2013/12/12|2013-12-12|Date|TRUE|FALSE
            2013/12/12|2013-12-12|Date|FALSE|FALSE
            2001/01/01|2001-01-01|Date|TRUE|FALSE
            2001/01/01|2001-01-01|Date|FALSE|FALSE
            12/12/2013|2013-12-12|Date|TRUE|FALSE
            12/12/2013|2013-12-12|Date|FALSE|FALSE
            01/01/2001|2001-01-01|Date|TRUE|FALSE
            01/01/2001|2001-01-01|Date|FALSE|FALSE
            01/04/2014|2014-04-01|Date|TRUE|FALSE
            01/04/2014|2014-04-01|Date|FALSE|FALSE
            01/04/2014|2014-01-04|Date|TRUE|TRUE
            01/04/2014|2014-01-04|Date|FALSE|TRUE
            2014-4-29|2014-04-29|Date|TRUE|FALSE
            2014-4-29|2014-04-29|Date|FALSE|FALSE
            2014-04-29|2014-04-29|Date|TRUE|FALSE
            2014-04-29|2014-04-29|Date|FALSE|FALSE
            2014-05-30|2014-05-30|Date|TRUE|FALSE
            2014-05-30|2014-05-30|Date|FALSE|FALSE
            29-04-2014|2014-04-29|Date|TRUE|FALSE
            29-04-2014|2014-04-29|Date|FALSE|FALSE
            30-05-2014|2014-05-30|Date|TRUE|FALSE
            30-05-2014|2014-05-30|Date|FALSE|FALSE
            29 Apr 13|2029-04-13|Date|TRUE|FALSE
            29 Apr 13|2013-04-29|Date|FALSE|FALSE
            30 May 14|2030-05-14|Date|TRUE|FALSE
            30 May 14|2014-05-30|Date|FALSE|FALSE
            29:Apr:13|2029-04-13|Date|TRUE|FALSE
            29:Apr:13|2013-04-29|Date|FALSE|FALSE
            30:May:14|2030-05-14|Date|TRUE|FALSE
            30:May:14|2014-05-30|Date|FALSE|FALSE
            Apr:13|2013-04|yearmon|TRUE|FALSE
            Apr:13|2013-04|yearmon|FALSE|FALSE
            May:14|2014-05|yearmon|TRUE|FALSE
            May:14|2014-05|yearmon|FALSE|FALSE
            13:Apr|2013-04|yearmon|TRUE|FALSE
            13:Apr|2013-04|yearmon|FALSE|FALSE
            14:May|2014-05|yearmon|TRUE|FALSE
            14:May|2014-05|yearmon|FALSE|FALSE
            29 12 13|2029-12-13|Date|TRUE|FALSE
            29 12 13|2013-12-29|Date|FALSE|FALSE
            30 12 14|2030-12-14|Date|TRUE|FALSE
            30 12 14|2014-12-30|Date|FALSE|FALSE
            29 12 2013|2013-12-29|Date|TRUE|FALSE
            29 12 2013|2013-12-29|Date|FALSE|FALSE
            30 12 2014|2014-12-30|Date|TRUE|FALSE
            30 12 2014|2014-12-30|Date|FALSE|FALSE
            2013 12 29|2013-12-29|Date|TRUE|FALSE
            2013 12 29|2013-12-29|Date|FALSE|FALSE
            2014 12 30|2014-12-30|Date|TRUE|FALSE
            2014 12 30|2014-12-30|Date|FALSE|FALSE",
                     sep="|",
                     stringsAsFactors=FALSE,
                     header=TRUE,
                     colClass="character",
                     check.names=FALSE
    )
    for (col in 1:ncol(tab)) tab[,col]<- tutils::trim(tab[,col])
    tab %<>% mutate(`Expected`=as.character(NA))

    `ISO_8601_to_character_expected_per_class`<- function(tab) {
        for (row in 1:nrow(tab)) {
            iso.char<- tab[row, "ISO 8601"]
            if (tab[row, "Class"]=="yearmon") {
                dt.char<- paste(iso.char,"01",sep="-")
            }
            else {
                dt.char<- iso.char
            }
            tab[row,"Expected"]<- as.character(
                eval(parse(text=sprintf("as.%s(\"%s\")\n", tab[row, "Class"], dt.char)))
            )
        }

        tab
    }
    # tab<- ISO_8601_to_expected_for_class(tab)
    tab %<>% ISO_8601_to_character_expected_per_class()
    stopifnot(!any(is.na(tab)))

    year.left<-     as.logical(tab[, "year.left"])
    american<-      as.logical(tab[, "american"])

    tab
}
