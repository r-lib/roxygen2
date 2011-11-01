context("Wrapping DESCRIPTION fields only when necessary")
test_that("Left-side padding works properly for n > 0", {
    expect_equal(leftPadNSpaces("test", n = 2), "  test" )
    expect_equal(leftPadNSpaces("test", n = 4), "    test")
    expect_equal(leftPadNSpaces("Alan Turing <alan@turing.fake>", n = 4), "    Alan Turing <alan@turing.fake>")
  }
)

test_that("Left-side padding works for the empty string", {
    expect_equal(leftPadNSpaces("", n = 4), "    ")
  }
)

test_that("Left-side padding doesn't pad for n < 0", {
    expect_equal(leftPadNSpaces("test", n = -1), "test")
  }
)

test_that("Left-side padding is vectorized", {
    test_names   <- c("Alan Turing", "Alonzo Church")
    padded_names <- c("    Alan Turing", "    Alonzo Church")
    expect_equal(leftPadNSpaces(test_names, n = 4), padded_names)
  }
)






