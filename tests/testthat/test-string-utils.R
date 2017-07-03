context("String utilities [in 'string-utils.R']")

test_that("'capitalize' works", {
  expect_equal(
      capitalize("hello"), "Hello"
  )

  expect_equal(
      capitalize("hello, world!"), "Hello, World!"
  )

})

test_that("'get_file_ext' works", {
  expect_equal(
      get_file_ext("foo.bar.csv"), "csv"
  )

  parsed<- list()
  parsed[["basename"]]<-  "foo.bar"
  parsed[["extension"]]<- "csv"
  expect_equal(
      get_file_ext("foo.bar.csv", parse=TRUE)$basename,
      "foo.bar"
  )
  expect_equal(
      get_file_ext("foo.bar.csv", parse=TRUE)$extension,
      "csv"
  )

})
