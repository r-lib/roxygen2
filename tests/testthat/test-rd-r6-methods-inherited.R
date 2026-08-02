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

test_that("format.rd_r6_inherited returns nothing when empty", {
  expect_null(format(rd_r6_inherited()))
})

test_that("invalid r6_inherited_documentation_display errors", {
  local_roxy_meta_set("r6_inherited_documentation_display", "bogus")

  inherited <- rd_r6_inherited(
    package = "pkg",
    classname = "A",
    name = "foo",
    chain_package = "pkg",
    chain_classname = "A"
  )

  expect_error(format(inherited), "r6_inherited_documentation_display")
})

test_that("r6_inherited_documentation_display = 'single' renders a single pointer to the immediate parent", {
  local_roxy_meta_set("r6_inherited_documentation_display", "single")

  inherited <- rd_r6_inherited(
    package = c("pkg", "pkg"),
    classname = c("A", "A"),
    name = c("foo", "bar"),
    chain_package = "pkg",
    chain_classname = "A"
  )

  expect_snapshot(cat(format(inherited), sep = "\n"))
})

test_that("r6_inherited_documentation_display = 'grouped' (the default) groups methods by originating ancestor", {
  local_roxy_meta_set("current_package", "")

  # Methods actually defined on B (nearest ancestor) vs A (further up the
  # chain) must end up in separate, correctly-labelled subsections, in
  # nearest-to-furthest order -- regardless of row order in `classname`.
  inherited <- rd_r6_inherited(
    package = c("pkg", "pkg", "pkg"),
    classname = c("A", "B", "A"),
    name = c("shared", "b1", "only_a"),
    chain_package = c("pkg", "pkg"),
    chain_classname = c("B", "A")
  )

  expect_snapshot(cat(format(inherited), sep = "\n"))
})

test_that("r6_inherited_documentation_display = 'grouped' skips ancestors with no contributed methods", {
  local_roxy_meta_set("current_package", "")

  # Chain is B -> A, but every surviving inherited method actually comes
  # from A (e.g. B's own methods were all overridden) -- there should be
  # no empty "Inherited methods from B" subsection.
  inherited <- rd_r6_inherited(
    package = "pkg",
    classname = "A",
    name = "only_a",
    chain_package = c("pkg", "pkg"),
    chain_classname = c("B", "A")
  )

  expect_snapshot(cat(format(inherited), sep = "\n"))
})

test_that("r6_inherited_documentation_display = 'original' restores the pre-8.1.0 flat list", {
  local_roxy_meta_set("r6_inherited_documentation_display", "original")
  local_roxy_meta_set("current_package", "")

  # Methods from two different ancestors (B and A), mixed together in one
  # flat list, each keeping its own actual originating classname -- this
  # is the exact pre-PR rendering, not the new grouped-by-ancestor form.
  inherited <- rd_r6_inherited(
    package = c("pkg", "pkg", "pkg"),
    classname = c("A", "B", "A"),
    name = c("shared", "b1", "only_a"),
    chain_package = c("pkg", "pkg"),
    chain_classname = c("B", "A")
  )

  expect_snapshot(cat(format(inherited), sep = "\n"))
})

test_that("r6_inherited_documentation_display = 'original' with more than 5 methods starts collapsed", {
  local_roxy_meta_set("r6_inherited_documentation_display", "original")
  local_roxy_meta_set("current_package", "")

  inherited <- rd_r6_inherited(
    package = rep("pkg", 6),
    classname = rep("A", 6),
    name = paste0("m", 1:6),
    chain_package = "pkg",
    chain_classname = "A"
  )

  out <- format(inherited)
  expect_true(any(grepl("<details><summary>Inherited methods</summary>", out)))
  expect_false(any(grepl("<details open>", out)))
})
