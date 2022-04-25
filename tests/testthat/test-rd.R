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
  expect_snapshot_warning(
    roc_proc_text(rd_roclet(), "
      #' Virtual Class To Enforce Max Slot Length
      setClass('A')

      #' Validity function.
      setValidity('A', function(object) TRUE)"
    )
  )
})

test_that("can't set description and re-export", {
  expect_snapshot_warning(
    out <- roc_proc_text(rd_roclet(), "
      #' @description NOPE
      #' @export
      magrittr::`%>%`
      ")
  )

  expect_length(out, 0)
})


test_that("documenting NA gives useful error message (#194)", {
  expect_snapshot_warning(
    roc_proc_text(rd_roclet(), "
      #' Missing value
      NA"
    )
  )
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
  local_package_copy(test_path("empty"))
  desc::desc_set(
    Package = "roxygendevtest",
    Title = "Package Title",
    Description = "Package description."
  )
  out <- roc_proc_text(rd_roclet(), "
    #' Title
    #'
    #' @docType package
    #' @description NULL
    #' @name pkg
    '_PACKAGE'
  ")
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

# UTF-8 -------------------------------------------------------------------

test_that("can generate nonASCII document", {
  local_package_copy(test_path('testNonASCII'))

  expect_snapshot({
    roxygenise(roclets = "rd")
    "Second run should be idempotent"
    roxygenise(roclets = "rd")
  })

  rd_path <- file.path("man", "printChineseMsg.Rd")
  expect_true(file.exists(rd_path))
  rd <- read_lines(rd_path)

  expect_true(any(grepl("\u6211\u7231\u4e2d\u6587", rd)))
  expect_true(any(grepl("\u4e2d\u6587\u6ce8\u91ca", rd)))
})

test_that("unicode escapes are ok", {
  local_package_copy(test_path('testUtf8Escape'))

  expect_snapshot({
    roxygenise(roclets = "rd")
    "Second run should be idempotent"
    roxygenise(roclets = "rd")
  })

  rd_path <- file.path("man", "a.Rd")
  expect_true(file.exists(rd_path))
  rd <- read_lines(rd_path)

  expect_true(any(grepl("7\u00b0C", rd)))
})

test_that("automatically deletes unused files", {
  local_package_copy(test_path("empty"))
  dir.create("man")
  suppressMessages(roxygenise())

  write_lines(made_by("%"), "man/test.Rd")
  expect_snapshot(roxygenise())
})
