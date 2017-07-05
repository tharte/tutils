context("Time-series and Date utilities ['ts-utils.R']")

library(zoo)
library(magrittr)
library(dplyr)

test_that("'is_Date' works", {
    expect_true(is_Date(as.Date("2017-07-04")))
    expect_false(is_Date("2017-07-04"))

    x<- seq(as.Date("2017-07-01"), len=4, by=1)
    expect_true(is_Date(x))
    expect_false(is_Date(as.character(x)))

})


test_that("'is_yearmon' works", {

    expect_true(is_yearmon(as.yearmon("2017-07-04")))
    expect_false(is_yearmon("2017-07-04"))

    x<- as.yearmon(seq(as.Date("2017-07-01"), len=4, by=1))
    expect_true(is_yearmon(x))
    expect_false(is_yearmon(as.character(x)))
})


test_that("'get_return' works", {
    expect_equal(get_return(NA), NA)
    expect_equal(get_return(c(NA,NA)), NA)
    expect_equal(get_return(c(NaN,NA)), NA)
    expect_equal(get_return(c(NaN,1,1.1)), 0.1)
    expect_equal(get_return(c(1,+Inf,1.1)), 0.1)

    dates<- seq(as.Date("2011-01-01"), by=1, len=6)
	z<- zoo(c(NA, 1, 2, 3, 0, 0), order.by=dates)

    z.res1<- zoo(
        c(NA, 1.0, 0.5, -1.0, NA),
        order.by=dates[2:length(dates)]
    )
    # 2011-01-02 2011-01-03 2011-01-04 2011-01-05 2011-01-06
    #	  NA        1.0        0.5       -1.0         NA

    expect_equal(
        zoo::rollapply(z, 2, get_return, align="right", nan.replace=TRUE),
        z.res1
    )


    z.res2<- zoo(
        c(NA, 1.0, 0.5, -1.0, NaN),
        order.by=dates[2:length(dates)]
    )
	# 2011-01-02 2011-01-03 2011-01-04 2011-01-05 2011-01-06
	# 	  NA        1.0        0.5       -1.0        NaN

    expect_equal(
        zoo::rollapply(z, 2, get_return, align="right", nan.replace=FALSE),
        z.res2
    )

})


test_that("'get_diff' works", {
    expect_equal(get_diff(NA), NA)
    expect_equal(get_diff(c(NA,NA)), NA)
    expect_equal(get_diff(c(NaN,NA)), NA)
    expect_equal(get_diff(c(NaN,1,1.1)), 0.1)
    expect_equal(get_diff(c(1,+Inf,1.1)), 0.1)
    expect_equal(get_diff(c(1,0,1.1)), 0.1)

    dates<- seq(as.Date("2011-01-01"), by=1, len=6)
	z<- zoo(c(NA, 1, 2, 3, 0, 0), order.by=dates)

    z.res1<- zoo(
        c(NA, 1.0, 1.0, -3.0, 0),
        order.by=dates[2:length(dates)]
    )
    # 2011-01-02 2011-01-03 2011-01-04 2011-01-05 2011-01-06
    #	  NA        1.0        1.0       -3.0         0

    expect_equal(
        zoo::rollapply(z, 2, get_diff, align="right"),
        z.res1
    )
})


test_that("'to_ISO_8601' works", {
    tab<-  make_ISO_8601_test_table()
    tab %<>% mutate(`Output`=as.character(NA))

    for (row in 1:nrow(tab)) {
        tab[row, "Output"]<- to_ISO_8601(
            str=tab[row, "Test"],
            year.left=tab[row, "year.left"],
            american=tab[row, "american"]
        ) %>% as.character
    }

    tab %<>% mutate(`success`=`Expected`==`Output`)

    expect_true(all(tab$success))
})
