context("Time-series and Date utilities ['ts-utils.R']")

library(zoo)

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
})
