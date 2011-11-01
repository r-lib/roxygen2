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

test_that("Can properly mock formatted output", {
    single_author_formatted <- "Author: Alan Turing <alan@turing.fake>"
    double_author_raw <- "Alan Turing <alan@turing.fake>,\nAlonzo Church <alonzo@church.fake>"
    double_author_formatted <- c("Author: Alan Turing <alan@turing.fake>,", "    Alonzo Church <alonzo@church.fake>")
    triple_author_raw       <- "Alan Turing <alan@turing.fake>,\nAlonzo Church <alonzo@church.fake>,\nCharles Babbage <charles@babbage.fake>"
    triple_author_formatted <- c("Author: Alan Turing <alan@turing.fake>,",
                                 "    Alonzo Church <alonzo@church.fake>,",
                                 "    Charles Babbage <charles@babbage.fake>")
    
    expect_equal(mock_formatted_text("Author", "Alan Turing <alan@turing.fake>"), single_author_formatted)
    expect_equal(mock_formatted_text("Author", double_author_raw), double_author_formatted)
    expect_equal(mock_formatted_text("Author", triple_author_raw), triple_author_formatted)
  }
)






