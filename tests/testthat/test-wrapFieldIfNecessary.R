context("Wrap DESCRIPTION fields only when necessary")

test_that("Can properly simulate formatted output", {
    single.author.raw <- "Alan Turing <alan@turing.fake>"
    single.author.formatted <- "Author: Alan Turing <alan@turing.fake>"
    double.author.raw <- "Alan Turing <alan@turing.fake>,\nAlonzo Church <alonzo@church.fake>"
    double.author.formatted <- c("Author: Alan Turing <alan@turing.fake>,", "    Alonzo Church <alonzo@church.fake>")
    triple.author.raw       <- "Alan Turing <alan@turing.fake>,\nAlonzo Church <alonzo@church.fake>,\nCharles Babbage <charles@babbage.fake>"
    triple.author.formatted <- c("Author: Alan Turing <alan@turing.fake>,",
                                 "    Alonzo Church <alonzo@church.fake>,",
                                 "    Charles Babbage <charles@babbage.fake>")

    expect_equal(simulate_formatted_text("Author", single.author.raw), single.author.formatted)
    expect_equal(simulate_formatted_text("Author", double.author.raw), double.author.formatted)
    expect_equal(simulate_formatted_text("Author", triple.author.raw), triple.author.formatted)
  }
)

test_that("DESCRIPTION fields get wrapped if a line length exceeds the wrapping threshold", {
    desc <- read.description("description-example.txt")
    expect_equal(
      wrap_field_if_necessary("Description", desc$Description, wrap.threshold = 80),
      str_wrap(paste0("Description", ": ", desc$Description), exdent = 4, width = 80)
    )
    expect_equal(
      wrap_field_if_necessary("Description", desc$Description, wrap.threshold = 60),
      str_wrap(paste0("Description", ": ", desc$Description), exdent = 4, width = 60)
    )
    expect_equal(
      wrap_field_if_necessary("Author", desc$Author, wrap.threshold = 40),
      str_wrap(paste0("Author", ": ", desc$Author), exdent = 4, width = 40)
    )
  }
)

test_that("DESCRIPTION fields get wrapped if they are marked as individual_lines", {
    desc <- read.description("description-example.txt")
    expect_equal(
      wrap_field_if_necessary("Collate", desc$Collate, wrap.threshold = 0),
      str_wrap(paste0("Collate", ": ", desc$Collate), exdent = 4, width = 0)
    )
  }
)

test_that("DESCRIPTION fields DO NOT get wrapped if no line exceeds the wrapping threshold", {
    desc <- read.description("description-example.txt")
    expect_equal(
      wrap_field_if_necessary("Author", desc$Author, wrap.threshold = 60),
      simulate_formatted_text("Author", desc$Author)
    )
    expect_equal(
      wrap_field_if_necessary("Author", desc$Author, wrap.threshold = 80),
      simulate_formatted_text("Author", desc$Author)
    )
  }
)

test_that("Infinity threshold turns off wrapping", {
  poem <- paste(sample(letters, 1000, TRUE), collapse = "")
  expect_equal(
    wrap_field_if_necessary("LongPoem", poem, wrap.threshold = Inf),
    paste("LongPoem:", poem)
  )
}
)
