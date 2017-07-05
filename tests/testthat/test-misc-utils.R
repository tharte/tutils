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
