# @R6method tag parsing ----------------------------------------------------

test_that("@R6method tag parser validates input", {
  tag <- roxy_tag("R6method", "Foo$bar")
  result <- roxy_tag_parse(tag)
  expect_equal(result$val, list(class = "Foo", method = "bar"))

  expect_snapshot({
    . <- roxy_tag_parse(roxy_tag("R6method", ""))
    . <- roxy_tag_parse(roxy_tag("R6method", "nomethod"))
    . <- roxy_tag_parse(roxy_tag("R6method", "Foo$bar\nextra"))
  })
})

# $set() ------------------------------------------------------------------

test_that("$set() infers @R6method automatically", {
  docs <- r6_doc(
    "
    #' Title
    C <- R6::R6Class('C', cloneable = FALSE)

    #' @description Method 2.
    #' @param x A number.
    C$set('public', 'meth2', function(x) { })
  "
  )
  meth <- docs$methods$self[[1]]
  expect_equal(meth$name, "meth2")
  expect_equal(meth$description, "Method 2.")
  expect_equal(meth$params, list(list(name = "x", description = "A number.")))
})

# explicit @R6method -------------------------------------------------------

test_that("explicit @R6method works on NULL block", {
  docs <- r6_doc(
    "
    #' Title
    C <- R6::R6Class('C', cloneable = FALSE,
      public = list(
        meth2 = function(x) {}
      )
    )
    #' @R6method C$meth2
    #' @description Method 2.
    #' @param x A number.
    NULL
  "
  )
  meth <- docs$methods$self[[1]]
  expect_equal(meth$name, "meth2")
  expect_equal(meth$description, "Method 2.")
  expect_equal(meth$params, list(list(name = "x", description = "A number.")))
})

test_that("@R6method warns on unknown class", {
  text <- "
      #' @R6method NoSuchClass$meth
      #' @description A method.
      NULL
    "
  expect_snapshot(roc_proc_text(rd_roclet(), text))
})
