test_that("can extract superclasses", {
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

  expect_equal(
    docs$superclasses,
    rd_r6_super(
      "C2",
      package = "R_GlobalEnv",
      classname = "C1",
      has_topic = FALSE
    )
  )
})

test_that("format.rd_r6_super with one superclass", {
  supers <- rd_r6_super(
    "Child",
    package = "mypkg",
    classname = "Parent",
    has_topic = FALSE
  )
  expect_snapshot(cat(format(supers), sep = "\n"))
})

test_that("format.rd_r6_super omits pkg:: for same-package classes", {
  local_roxy_meta_set("current_package", "mypkg")
  supers <- rd_r6_super(
    "Child",
    package = c("mypkg", "otherpkg"),
    classname = c("Parent", "GrandParent"),
    has_topic = c(FALSE, FALSE)
  )
  expect_snapshot(cat(format(supers), sep = "\n"))
})

test_that("format.rd_r6_super returns nothing when empty", {
  expect_null(format(rd_r6_super("Foo")))
})
