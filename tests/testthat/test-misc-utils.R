context("Miscellaneous utilities ['misc-utils.R']")

test_that("'top_dir' works", {
# DO NOT RUN: dependent on system / user-configuration
#
#  expect_equal(
#      capitalize("top_dir"), "/home/thomas"
#  )

})


test_that("'assert' works", {
    expect_identical(
        assert(is.data.frame(data.frame())),
        invisible()
    )
})


test_that("'%not.in%' works", {
    expect_identical(
        assert("a" %not.in% letters[2:length(letters)]),
        invisible()
    )

    expect_true("a" %not.in% letters[2:length(letters)])
    expect_false("a" %not.in% letters)

})

test_that("'is_even', 'is_odd' work", {
    expect_true(is_even(2))
    expect_false(is_even(1))

    expect_true(is_odd(1))
    expect_false(is_odd(2))

})


test_that("'absolute_path' works", {
	if (.Platform$OS.type=="windows") {
		filename<- "u:/foo/bar.csv"
		expect_equal(absolute_path(filename), filename)
	}
	else if (.Platform$OS.type=="unix") {
		filename<- "~/foo/bar.csv"

		### something of a circular test:
		expect_equal(
            absolute_path(filename),
            gsub("~", file.path(dirname("~"), basename("~")), filename)
        )
	}
	else {
		warning(sprintf("%s not recognized", .Platform$OS.type))
	}

})


test_that("'cut_system' works", {
	tab<- read.csv(text='Name,Age,Salary,ID
             Dick,38,32k,1
             Tom,21,21k,2
             Harry,56,NA,3',
             header=TRUE,
             stringsAsFactors=FALSE
    )
    for (col in which(col_classes(tab)=="character"))
        tab[, col]<- tutils::trim(tab[, col])

    filename<- paste(tempfile(), ".csv", sep="")
    write.csv(tab, file=filename, row.names=FALSE, quote=FALSE)

    expect_equal(
        cut_system(1, filename, sep=","),
        c("Name","Dick","Tom","Harry")
    )

    expect_equal(
        cut_system(4, filename, sep=","),
        c("ID","1","2","3")
    )

    expect_equal(
        cut_system(c(1,4), filename, sep=","),
        c("Name,ID","Dick,1","Tom,2","Harry,3")
    )

    unlink(filename)

})


test_that("'grep_system' works", {
	tab<- read.csv(text='Name,Age,Salary,ID
             Dick,38,32k,1
             Tom,21,21k,2
             Harry,56,NA,3',
             header=TRUE,
             stringsAsFactors=FALSE
    )
    for (col in which(col_classes(tab)=="character"))
        tab[, col]<- tutils::trim(tab[, col])

    filename<- paste(tempfile(), ".csv", sep="")
    write.csv(tab, file=filename, row.names=FALSE, quote=FALSE)

    expect_equal(
        grep_system("Tom", filename),
        "Tom,21,21k,2"
    )

    expect_equal(
        grep_system("Dick", filename),
        "Dick,38,32k,1"
    )

    expect_equal(
        grep_system("harry", filename, options="-i"),
        "Harry,56,NA,3"
    )

    unlink(filename)

})


test_that("'pdftikz' works", {
    od<- getwd()
    
    example(pdftikz)

    expect_equal(
        od,
        getwd()
    )
})
