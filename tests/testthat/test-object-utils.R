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

#'  # NULL list:
#' 	copy(list(), integer(0))
#'
#' 	# unnamed list:
#'  x<- list(1, 2, 3)
#'  x
#'
#'  copy(x, integer(0))
#'  copy(x, 1:2)
#'
#'  # named list:
#'  x<- list(a=1, b=2, c=3)
#'  x
#'  # integer index still works:
#'  copy(x, 1:2)
#'  # named index works too:
#'  copy(x, c("a","b"))
#'
#'  ## FIXME: index mismatch
#'  ## copy(x, 1:4)
#'  ## copy(x, c("a","b"))
#'  ## copy(x, c("a","b","d"))
#'
N<- 20
df<- data.frame(Name=letters[1:N], Value=1:N)
ht(df)

N<- 3
df<- data.frame(Name=letters[1:N], Value=1:N)
ht(df)

    expect_equal(
          sapply(strsplit(str, " "), first),
          c("hello", "how")
    )

    expect_equal(
          sapply(strsplit(str, " "), last),
          c("there", "you?")
    )

})
