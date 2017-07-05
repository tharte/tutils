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
