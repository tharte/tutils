context("String utilities ['string-utils.R']")

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


test_that("'is_blank' works", {
  expect_true(
      is_blank("")
  )

  expect_true(
      is_blank(rep("", 4))
  )

  expect_false(
      is_blank(c("hello", rep("", 4)))
  )


})


test_that("'is_not_applicable' works", {
    tab<- read.table(text='Name| Age|Salary|ID
             Tom| NA|32k|1
             N/A| NA|21k|2
             Harry| NA|NA|N/A',
            header=TRUE,
            sep="|",
            colClasses=c("character","integer","character")
        )
    res<- is_not_applicable(tab)

    expect_equal(
        res[, "Name"],
        c(FALSE,TRUE,FALSE)
    )

    expect_equal(
        res[, "Age"],
        rep(as.logical(NA), 3)
    )

    expect_equal(
        res[, "Salary"],
        c(FALSE, FALSE, NA)
    )

    expect_equal(
        res[, "ID"],
        c(FALSE, FALSE, TRUE)
    )

})


test_that("'is_whitespace' works", {
    tab<- read.table(text='Name| Age|Salary|ID
             | |32k|1
             N/A| NA|21k|2
             Harry| NA|NA|NA',
            header=TRUE,
            sep="|",
            colClasses=c("character","character","character","integer")
        )
    res<- is_whitespace(tab)

    expect_equal(
        res[, "Name"],
        c(TRUE,FALSE,FALSE)
    )

    expect_equal(
        res[, "Age"],
        c(TRUE,FALSE,FALSE)
    )

    expect_equal(
        res[, "Salary"],
        c(FALSE, FALSE, NA)
    )

    expect_equal(
        res[, "ID"],
        c(FALSE, FALSE, NA)
    )

})
