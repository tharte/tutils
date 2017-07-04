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
