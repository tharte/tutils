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

test_that("'triml' works", {
  expect_equal(
      triml(" asdf qwerty \t"), "asdf qwerty \t"
  )

  expect_equal(
      triml("\tasdf qwerty \t"), "asdf qwerty \t"
  )


})

test_that("'trimr' works", {
  expect_equal(
      trimr(" asdf qwerty \t"), " asdf qwerty"
  )

  expect_equal(
      trimr("\tasdf qwerty \t"), "\tasdf qwerty"
  )


})

test_that("'trim' works", {
  expect_equal(
      trim(" asdf qwerty \t"), "asdf qwerty"
  )

  expect_equal(
      trim("\tasdf qwerty \t"), "asdf qwerty"
  )

  expect_equal(
      trim(" asdf qwerty \t"), "asdf qwerty"
  )

  expect_equal(
      trim("\tasdf qwerty \t "), "asdf qwerty"
  )


})
