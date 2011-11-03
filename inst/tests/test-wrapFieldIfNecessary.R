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
    test.names   <- c("Alan Turing", "Alonzo Church")
    padded.names <- c("    Alan Turing", "    Alonzo Church")
    expect_equal(leftPadNSpaces(test.names, n = 4), padded.names)
  }
)

test_that("Can properly mock formatted output", {
    single.author.raw <- "Alan Turing <alan@turing.fake>"
    single.author.formatted <- "Author: Alan Turing <alan@turing.fake>"
    double.author.raw <- "Alan Turing <alan@turing.fake>,\nAlonzo Church <alonzo@church.fake>"
    double.author.formatted <- c("Author: Alan Turing <alan@turing.fake>,", "    Alonzo Church <alonzo@church.fake>")
    triple.author.raw       <- "Alan Turing <alan@turing.fake>,\nAlonzo Church <alonzo@church.fake>,\nCharles Babbage <charles@babbage.fake>"
    triple.author.formatted <- c("Author: Alan Turing <alan@turing.fake>,",
                                 "    Alonzo Church <alonzo@church.fake>,",
                                 "    Charles Babbage <charles@babbage.fake>")
    
    expect_equal(mock_formatted_text("Author", single.author.raw), single.author.formatted)
    expect_equal(mock_formatted_text("Author", double.author.raw), double.author.formatted)
    expect_equal(mock_formatted_text("Author", triple.author.raw), triple.author.formatted)
  }
)

test_that("DESCRIPTION fields get wrapped if a line length exceeds the wrapping threshold", {
    desc <- read.description("description-example.txt")
    expect_equal(
      WrapFieldIfNecessary("Description", desc$Description, wrap.threshold = 80), 
      strwrap(sprintf('%s: %s', "Description", desc$Description), exdent = 4, width = 80)
    )
    expect_equal(
      WrapFieldIfNecessary("Description", desc$Description, wrap.threshold = 60), 
      strwrap(sprintf('%s: %s', "Description", desc$Description), exdent = 4, width = 60)
    )
  }
)




