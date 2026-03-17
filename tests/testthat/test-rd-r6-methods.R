test_that("format.rd_r6_methods with one method", {
  methods <- rd_r6_methods(
    "Foo",
    self = list(
      rd_r6_method(
        name = "run",
        class = "Foo",
        formals = NULL,
        description = "Run it."
      )
    )
  )
  expect_snapshot(cat(format(methods), sep = "\n"))
})

test_that("format.rd_r6_methods returns nothing when empty", {
  expect_null(format(rd_r6_methods("Foo")))
})

test_that("r6_all_examples aggregates across methods", {
  text <- "
    #' Class
    C <- R6::R6Class('C', cloneable = FALSE,
      public = list(
        #' @description Greet.
        #' @examples c$greet()
        greet = function() 'hi',
        #' @description Run.
        run = function() 1,
        #' @description Stop.
        #' @param force Force it
        #' @examples c$stop()
        #' @examples c$stop(force = TRUE)
        stop = function(force = FALSE) NULL
      )
    )"
  docs <- r6_doc(text)
  expect_snapshot(cat(r6_all_examples(docs$methods), sep = "\n"))
})

test_that("r6_flatten_sections collapses markdown sections", {
  local_markdown()

  tag <- roxy_tag("details", "Some details.\n\n# A heading\n\nBody.")
  tag <- tag_markdown_with_sections(tag)
  expect_length(tag$val, 2)

  tag <- r6_flatten_sections(tag)
  expect_length(tag$val, 1)
  expect_match(tag$val, "Some details.", fixed = TRUE)
  expect_match(tag$val, "\\subsection{A heading}", fixed = TRUE)
  expect_match(tag$val, "Body.", fixed = TRUE)
})
