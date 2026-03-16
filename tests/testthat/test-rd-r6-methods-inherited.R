test_that("can extract inherited methods", {
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

  expect_s3_class(docs$methods$inherited, "rd_r6_inherited")
  expect_equal(docs$methods$inherited$name, "only_a")
})

test_that("no inherited methods when none exist", {
  text <- "
    C1 <- R6::R6Class('C1', cloneable = FALSE)

    #' Class
    C2 <- R6::R6Class('C2',
      inherit = C1,
      public = list(
        #' @description method1
        meth1 = function() 1
      )
    )"
  docs <- r6_doc(text)
  expect_equal(docs$methods$inherited, rd_r6_inherited())
})

test_that("format.rd_r6_inherited renders method list", {
  inherited <- rd_r6_inherited(
    package = c("pkg", "pkg"),
    classname = c("A", "A"),
    name = c("foo", "bar")
  )

  expect_snapshot(cat(format(inherited), sep = "\n"))
})

test_that("format.rd_r6_inherited returns nothing when empty", {
  expect_null(format(rd_r6_inherited()))
})
