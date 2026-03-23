# Construction tests -------------------------------------------------------

test_that("can construct empty class", {
  text <- "
    #' Class
    C <- R6::R6Class(cloneable = FALSE)"
  docs <- r6_doc(text)

  expect_s3_class(docs, "rd_r6_class")
  expect_equal(docs$methods, rd_r6_methods("C"))
  expect_equal(docs$fields, rd_r6_fields())
  expect_equal(docs$active_bindings, rd_r6_fields(type = "active"))
})

test_that("class description is not duplicated", {
  text <- "
    #' Title
    #'
    #' Description
    foo <- R6::R6Class(
      public = list(
        #' @description foo
        foo = function() {}
      )
    )
  "

  out <- roc_proc_text(rd_roclet(), text)[[1]]
  expect_equal(out$get_value("description"), "Description")
})

test_that("title-only class has single description", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    foo <- R6::R6Class(cloneable = FALSE)
  "
  )[[1]]
  expect_equal(out$get_value("description"), "Title")

  out <- roc_proc_text(
    rd_roclet(),
    "
    #' @title Title
    foo <- R6::R6Class(cloneable = FALSE)
  "
  )[[1]]
  expect_equal(out$get_value("description"), "Title")
})

test_that("class with only active bindings doesn't error (#1610)", {
  text <- "
    #' Class
    C <- R6::R6Class('C',
      active = list(
        #' @field x A value.
        x = function(val) val
      ),
      cloneable = FALSE
    )"
  docs <- r6_doc(text)
  expect_equal(docs$methods, rd_r6_methods("C"))
})

test_that("warns about unmatched components ", {
  text <- "
    #' Class
    #' @field field1 Yep.
    C <- R6::R6Class(
      public = list(
        #' @description Dangling.
        field1 = NULL
      )
    )"
  expect_snapshot(docs <- r6_doc(text))
})

# Format tests -------------------------------------------------------------

test_that("format.rd_r6_class with fields", {
  docs <- rd_r6_class(
    class = "Foo",
    fields = rd_r6_fields(list(
      rd_r6_field("x", "A number."),
      rd_r6_field("y", "A string.")
    ))
  )
  expect_snapshot(cat(format(docs), sep = "\n"))
})

test_that("format.rd_r6_class with active bindings", {
  docs <- rd_r6_class(
    class = "Foo",
    active_bindings = rd_r6_fields(list(rd_r6_field("val", "A value.")), type = "active")
  )
  expect_snapshot(cat(format(docs), sep = "\n"))
})

test_that("format.rd_r6_class with no inherited methods", {
  text <- "
    C1 <- R6::R6Class('C1', cloneable = FALSE)

    #' @title Title
    #' @description Description.
    C2 <- R6::R6Class('C2',
      inherit = C1,
      cloneable = FALSE,
      public = list(
        #' @description method1
        meth1 = function() 1
      )
    )"
  docs <- r6_doc(text)
  expect_snapshot(cat(format(docs), sep = "\n"))
})

test_that("format.rd_r6_class with inherited methods", {
  text <- "
    A <- R6::R6Class('A',
      public = list(
        shared = function() 1,
        only_a = function() 2
      )
    )

    #' Class B.
    B <- R6::R6Class('B',
      inherit = A,
      public = list(
        #' @description Method from B.
        shared = function() 3
      )
    )"
  docs <- r6_doc(text)
  expect_snapshot(cat(format(docs), sep = "\n"))
})

test_that("format.rd_r6_class with markdown sections", {
  local_roxy_meta_set("markdown", TRUE)
  text <- "
    #' @title Title
    #' @description Description.
    C <- R6::R6Class('C', cloneable = FALSE,
      public = list(
        #' @description Method description.
        #'
        #' # Description section
        #'
        #' Description section body.
        #' @details # Details section
        #'
        #' Details section body.
        meth = function() { }
      )
    )"
  docs <- r6_doc(text)
  expect_snapshot(cat(format(docs), sep = "\n"))
})
