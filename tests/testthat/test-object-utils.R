context("Object utilities ['object-utils.R']")

test_that("'first','last' work", {
    str<- paste(c(
        "hello there",
        "how are you?"
    ), sep="\n")

    expect_equal(
          sapply(strsplit(str, " "), first),
          c("hello", "how")
    )

    expect_equal(
          sapply(strsplit(str, " "), last),
          c("there", "you?")
    )

})


test_that("'copy' works", {
    # NULL list:
    expect_equal(
        copy(list(), integer(0)),
        list()
    )

    # unnamed list:
    x<- list(1, 2, 3)

    expect_equal(
        copy(x, integer(0)),
        list()
    )

    expect_equal(
        copy(x, 1:2),
        list(1, 2)
    )

    # named list:
    x<- list(a=1, b=2, c=3)

    # integer index still works:
    expect_equal(
        copy(x, 1:2),
        list(a=1, b=2)
    )

    # named index works too:
    expect_equal(
        copy(x, c("a","b")),
        list(a=1, b=2)
    )

    if (0) {
    ## FIXME: index mismatch
    ## copy(x, 1:4)
    ## copy(x, c("a","b"))
    ## copy(x, c("a","b","d"))
    }

})


test_that("'push_back' works", {
    # zero-length list x: append zero-length object y:
    expect_equal(
        push_back(list(), list()),
        list()
    )

    # zero-length list x: append non-list object:
    x<- list()

    expect_equal(
        push_back(x, "one"),
        list("one")
    )


    # unnamed list x: append non-list object:
    x<- list()
    x[[1]]<- "one"
    x<- push_back(x, 2)

    expect_equal(x[1], list("one"))
    expect_equal(x[2], list(2))


    # named list x: append non-list object:
    x<- list()
    x[["one"]]<- 1
    expect_equal(
        push_back(x, "two")[[2]],
        "two"
    )


    # unnamed list x: append unnamed list object y:
    x<- list()
    x[[1]]<- "one"

    y<- list();
    y[[1]]<- 2;

    x<- push_back(x, y)

    expect_equal(x[2], list(2))


    # named list x: append named list object y:
    x<- list()
    x[[1]]<- "one"

    y<- list();
    y[[1]]<- 2;
    names(y)<- "two"

    x<- push_back(x, y)

    expect_equal(x[2], y[1])

})


test_that("'copy_vars' works", {
    e<- new.env()
    local({
        a<- "a"
        b<- "this is 'b'"
        x<- pi
    }, env=e)

    f<- new.env()
    copy_vars(from=e, to=f)
    expect_equal(e$a, f$a)
    expect_equal(e$b, f$b)
    expect_equal(e$x, f$x)

    g<- new.env()
    copy_vars(from=e, to=g, vars=(c("b","x")))
    expect_equal(e$b, g$b)
    expect_equal(e$x, g$x)

})


test_that("'put_var' works", {
    e<- new.env()
    local({
        a<- "a"
        b<- "this is 'b'"
        x<- pi
    }, env=e)
    f<- new.env()
    put_var("x", from=e, to=f)

    expect_equal(e$x, f$x)

})

test_that("'is_try_error' works", {
    e<- new.env()
    local({
        a<- "a"
        b<- "this is 'b'"
        x<- pi
    }, env=e)
    f<- new.env()

    op<- options()
    options(show.error.messages=FALSE)

    expect_true(
        is_try_error(try(
            put_var("someVar", from=e, to=f)
        ))
    )

    options(op)

})


test_that("'load_as' works", {
    file<- paste(tempfile(), ".Rdata", sep="")
    set.seed(1)
    objectWithAVeryStrangeName<- rnorm(10)
    save(objectWithAVeryStrangeName, file=file)
    x<- load_as(file)

    expect_equal(x, objectWithAVeryStrangeName)

    unlink(file)

})


test_that("'col_classes' works", {
	DF<- data.frame(
        names=c("one","two","three"),
        numbers=1:3,
        stringsAsFactors=FALSE
    )

	out<- col_classes(DF)

	expect_true(all(colnames(out)==c("names","numbers")))
	expect_true(out["names"] == "character")
	expect_true(out["numbers"] == "integer")

})


test_that("'match_col' works", {
	tab<- read.csv(con<- textConnection(
	"Name,    Age, Salary
	 Derek  ,  NA,    32k		# <- NOTE: 'Derek  '
	 Tom,      26,    21k
	 NA,       NA,     NA
	 Harry,    31,    50k"
	), header=TRUE, colClasses=c("character","integer","character"), comment.char="#"); close(con)

	expect_true(all(is.na( match_col("NON-MATCHING-STRING", tab, ROW.FUN="first", COL.FUN="first") )))
	expect_true(all(is.na( match_col("NON-MATCHING-STRING", tab, ROW.FUN="first", COL.FUN="last") )))
	expect_true(all(is.na( match_col("NON-MATCHING-STRING", tab, ROW.FUN="last", COL.FUN="first") )))
	expect_true(all(is.na( match_col("NON-MATCHING-STRING", tab, ROW.FUN="last", COL.FUN="last") )))

	result<-        rep(NA,2)
	names(result)<- c("row","col")
	result["row"]<- 1; result["col"]<- 1

	expect_true(all.equal(match_col("k", tab, ROW.FUN=tutils::first, COL.FUN=tutils::first), result))

	result["row"]<- 1; result["col"]<- 3
	expect_true(all.equal(match_col("k", tab, ROW.FUN=tutils::first, COL.FUN=tutils::last), result))

	result["row"]<- 4; result["col"]<- 1
	expect_true(all.equal(match_col("r", tab, ROW.FUN=tutils::last, COL.FUN=tutils::first), result))	# <- NOTE: testing for "r"

	result["row"]<- 4; result["col"]<- 3
	expect_true(all.equal(match_col("k", tab, ROW.FUN=tutils::last, COL.FUN=tutils::last), result))	# <- NOTE: testing for "k"

})


test_that("'remove_from' works", {
	FUN<- function(x) is_blank(x) | is.na(x)

	tab<- read.table(con<- textConnection(
	"Name  Age Salary
	 Dick   NA    32k
	 Tom    NA    21k
	 NA     NA    NA"
	), header=TRUE, colClasses=c("character","integer","character")); close(con)


	row.clean<- read.table(con<- textConnection(
	"Name  Age Salary
	 Dick   NA    32k
	 Tom    NA    21k"
	), header=TRUE, colClasses=c("character","integer","character")); close(con)
	expect_equal(
        remove_from_rows(tab, fun=FUN),
        row.clean
    )
	expect_equal(
        remove_from(tab, fun=FUN, dim="row"),
        row.clean
    )

	col.clean<- read.table(con<- textConnection(
	"Name  Salary
	 Dick  32k
	 Tom   21k
	 NA    NA"
	), header=TRUE, colClasses=c("character","character")); close(con)
	expect_equal(
        remove_from_cols(tab, fun=FUN),
        col.clean
    )
	expect_equal(
        remove_from(tab, fun=FUN, dim="col"),
        col.clean
    )

	both.clean<- read.table(con<- textConnection(
	"Name  Salary
	 Dick  32k
	 Tom   21k"
	), header=TRUE, colClasses=c("character","character")); close(con)
	expect_equal(
        remove_from(tab, fun=FUN, dim="both"),
        both.clean
    )

})


test_that("'order_cn_front, order_cn_back' works", {
	tab<- read.table(
        con<- textConnection(
            "Name    Age Salary ID
             Dick     38    32k  1
             Tom      21    21k  2
             Harry    56     NA  3"
        ),
        header=TRUE,
        colClasses=c("character","integer","character","integer")
    )
    close(con)

	expect_equal(
        tab %>% order_cn_front(c("Salary","Age")),
        tab[, c(3,2,1,4)]
    )

	expect_equal(
        tab %>% order_cn_back(c("Salary","Age")),
        tab[, c(1,4,3,2)]
    )

})


test_that("'equal_tol' works", {
    x<- c(NA, 1:10)
    set.seed(1)
    y<- x + rnorm(length(x), sd=.01)
	expect_true(
        equal_tol(x, y, tol=0.1, na.rm=TRUE)
    )

	expect_true(
        equal_tol(x, y, tol=0.1, pct=TRUE, na.rm=TRUE)
    )

	expect_true(
        all(equal_tol(x, y, tol=0.1, each=TRUE, na.rm=TRUE))
    )

})


test_that("'rbind_pad' works", {
    x<- list(
        mtcars[c("mpg", "wt")],
        mtcars[c("wt", "cyl")]
    )

    expect_equal(
        rbind_pad(x),
        plyr::rbind.fill(x)
    )

if (0) {
warning("not yet fully implemented")
    L3<- LETTERS[1:3]
    d0<- data.frame(cbind(int.1=1, int.2=1:10), fac=sample(L3, 10, replace=TRUE))
    d1<- data.frame(int.2=11:15, num=rnorm(5), date=seq(as.Date("2014-12-01"), by=1, len=5))

    rbind_pad(list(d0=d0, d1=d1))
}

})


test_that("'Intersect','Union' works", {
    sets<- list(
        1:3,
        2:4,
        3:5
    )

    expect_equal(
        unlist(last(Intersect(sets))),
        3
    )

    expect_equal(
        unlist(last(Union(sets))),
        1:5
    )

})


test_that("'bind_to_env' works", {
    e<- new.env()

    x<- 1:3
    `fun`<- function() {
        x
    }

    bind_to_env(x, env=e)
    bind_to_env(fun, env=e)
    x<- 1:10

    expect_equal(
        fun(), 1:10
    )

    whos(sort="Name", env=e, omit=NULL)
})
