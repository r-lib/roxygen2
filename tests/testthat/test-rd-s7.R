test_that("@prop creates Additional properties section", {
  text <- "
    #' Important class.
    #'
    #' @prop a prop a
    #' @prop b prop b
    a <- function() {}
  "
  out <- roc_proc_text(rd_roclet(), text)[[1]]
  value <- out$get_value("prop")
  expect_equal(value$name, c("a", "b"))
  expect_equal(value$description, c("prop a", "prop b"))

  expect_snapshot(out$get_section("prop"))
})

test_that("@prop class@name groups by class", {
  text <- "
    #' Classes.
    #'
    #' @prop Parent@x prop x
    #' @prop Child@y prop y
    a <- function() {}
  "
  out <- roc_proc_text(rd_roclet(), text)[[1]]
  value <- out$get_value("prop")
  expect_equal(value$class, c("Parent", "Child"))
  expect_equal(value$name, c("x", "y"))

  expect_snapshot(out$get_section("prop"))
})

test_that("@prop with mismatched braces warns and doesn't crash", {
  text <- "
    #' A class.
    #'
    #' @prop a prop a
    #' }
    a <- function() {}
  "
  expect_snapshot(. <- roc_proc_text(rd_roclet(), text))
})

test_that("@prop class@name warns on invalid spec", {
  text <- "
    #' A class.
    #'
    #' @prop @x prop x
    a <- function() {}
  "
  expect_snapshot(. <- roc_proc_text(rd_roclet(), text))
})

test_that("@prop without class doesn't use subsections", {
  text <- "
    #' A class.
    #'
    #' @prop a prop a
    #' @prop b prop b
    MyClass <- function() {}
  "
  out <- roc_proc_text(rd_roclet(), text)[[1]]
  rd <- format(out$get_section("prop"))
  expect_no_match(rd, "subsection")
  expect_match(rd, "Additional properties")
})
