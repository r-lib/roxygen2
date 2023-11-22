test_that("empty file gives empty list", {
  out <- roc_proc_text(rd_roclet(), "")
  expect_identical(out, list())
})

test_that("NULL gives empty list", {
  out <- roc_proc_text(rd_roclet(), "NULL")
  expect_identical(out, list())
})

test_that("@noRd inhibits documentation", {
  out <- roc_proc_text(rd_roclet(), "
    #' Would be title
    #' @title Overridden title
    #' @name a
    #' @noRd
    NULL")

  expect_equal(length(out), 0)
})

test_that("deleted objects not documented", {
  out <- roc_proc_text(rd_roclet(), "
    f <- function(){
      .a <- 0
      function(x = 1){
        .a <<- .a + x
        .a
      }
    }

    #' Addition function.
    f2 <- f()
    rm(f)
  ")
  expect_equal(names(out), "f2.Rd")
})

test_that("documenting unknown function requires name", {
  block <- "
    #' Virtual Class To Enforce Max Slot Length
    setClass('A')

    #' Validity function.
    setValidity('A', function(object) TRUE)
  "
  expect_snapshot(. <- roc_proc_text(rd_roclet(), block))
})

test_that("can't set description and re-export", {
  block <- "
      #' @description NOPE
      #' @export
      magrittr::`%>%`
      "
  expect_snapshot(out <- roc_proc_text(rd_roclet(), block))
  expect_length(out, 0)
})

test_that("documenting NA gives useful error message (#194)", {
  block <- "
    #' Missing value
    NA
  "
  expect_snapshot(. <- roc_proc_text(rd_roclet(), block))
})

test_that("@description NULL", {
  # Just ignore in this case
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    #'
    #' @description NULL
    #' @format NULL
    foobar <- 1:10
  ")
  expect_identical(out[[1]]$get_value("description"), "Title")

  # Still ignore
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    #' @description NULL
    #' @description desc
    #' @format NULL
    foobar <- 1:10
  ")
  expect_identical(out[[1]]$get_value("description"), "desc")

  # Still ignore for objects as well
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    #' @description NULL
    #' @format NULL
    foobar <- 1:10
  ")
  expect_identical(out[[1]]$get_value("description"), "Title")

  # But drop for package docs
  block <- "
    #' Title
    #'
    #' @docType package
    #' @description NULL
    #' @name pkg
    '_PACKAGE'
  "
  out <- roc_proc_text(rd_roclet(), block, wd = test_path("empty"))
  expect_null(out[[1]]$get_value("description"))
})

test_that("@details NULL", {
  # Just ignore in this case
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    #'
    #' @details NULL
    #' @format NULL
    foobar <- 1:10
  ")
  expect_null(out[[1]]$get_value("details"))

  # Still ignore
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    #' @details NULL
    #' @details desc
    #' @format NULL
    foobar <- 1:10
  ")
  expect_identical(out[[1]]$get_value("details"), "desc")

  # Still ignore for objects as well
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    #' @details NULL
    #' @format NULL
    foobar <- 1:10
  ")
  expect_null(out[[1]]$get_value("details"))
})


# package docs ------------------------------------------------------------


test_that("package docs don't get alias if function present", {

  block <- "
    #' Title
    #'
    '_PACKAGE'

    #' Empty
    empty <- function() {}
  "

  out <- roc_proc_text(rd_roclet(), block, test_path("empty"))[[1]]
  expect_equal(out$get_value("alias"), "empty-package")
})

test_that("package docs preserve existing aliases", {
  block <- "
    #' Title
    #' @aliases a b
    #'
    '_PACKAGE'
  "

  out <- roc_proc_text(rd_roclet(), block, test_path("empty"))[[1]]
  expect_equal(out$get_value("alias"), c("empty", "empty-package", "a", "b"))

  block <- paste0(block, "
    #' Empty
    empty <- function() {}
  ")
  out <- roc_proc_text(rd_roclet(), block, test_path("empty"))[[1]]
  expect_equal(out$get_value("alias"), c("empty-package", "a", "b"))
})

test_that("get correct alias even if user has overriden name", {
  block <- "
    #' Title
    #' @name foo
    #' @aliases bar
    #'
    '_PACKAGE'
  "

  out <- roc_proc_text(rd_roclet(), block, test_path("empty"))[[1]]
  expect_equal(
    out$get_value("alias"),
    c("empty", "empty-package", "foo", "bar")
  )
})

# UTF-8 -------------------------------------------------------------------

test_that("can generate nonASCII document", {
  path <- local_package_copy(test_path('testNonASCII'))
  withr::defer(pkgload::unload("testNonASCII"))

  expect_snapshot({
    roxygenise(path, roclets = "rd")
    "Second run should be idempotent"
    roxygenise(path, roclets = "rd")
  })

  rd_path <- file.path(path, "man", "printChineseMsg.Rd")
  expect_true(file.exists(rd_path))
  rd <- read_lines(rd_path)

  expect_true(any(grepl("\u6211\u7231\u4e2d\u6587", rd)))
  expect_true(any(grepl("\u4e2d\u6587\u6ce8\u91ca", rd)))
})

test_that("unicode escapes are ok", {
  path <- local_package_copy(test_path('testUtf8Escape'))
  withr::defer(pkgload::unload("testUtf8Escape"))

  expect_snapshot({
    roxygenise(path, roclets = "rd")
    "Second run should be idempotent"
    roxygenise(path, roclets = "rd")
  })

  rd_path <- file.path(path, "man", "a.Rd")
  expect_true(file.exists(rd_path))
  rd <- read_lines(rd_path)

  expect_true(any(grepl("7\u00b0C", rd)))
})

test_that("automatically deletes unused files", {
  path <- local_package_copy(test_path("empty"))
  dir.create(file.path(path, "man"))
  suppressMessages(roxygenise(path))
  withr::defer(pkgload::unload("empty"))

  write_lines(made_by("%"), file.path(path, "man/test.Rd"))
  expect_snapshot(roxygenise(path))
})
