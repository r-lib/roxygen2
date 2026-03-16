test_that("extract_r6_data without source refs", {
  txt <- "R6::R6Class('foo',
     public = list(
       field1 = NULL,
       meth1 = function(Z) { },
       meth2 = function(Z = 10, ...) { },
       field2 = \"foobar\",
       meth3 = function() { }
     )
   )"
  C <- eval(parse(text = txt, keep.source = FALSE))
  expect_snapshot(extract_r6_data(C), error = TRUE)
})

test_that("extract_r6_methods", {
  txt <-
    "R6::R6Class(
       public = list(
         field1 = NULL,
         meth1 = function(Z) { },
         meth2 = function(Z = 10, ...) { },
         field2 = \"foobar\",
         meth3 = function() { }
       )
     )"
  C <- eval(parse(text = txt, keep.source = TRUE))
  M <- extract_r6_methods(C)
  expect_equal(M$type, rep("method", 4))
  expect_equal(M$name, c(paste0("meth", 1:3), "clone"))
  expect_equal(M$line, c(4L, 5L, 7L, NA_integer_))
  expect_equal(
    M$formals,
    I(list(
      as.pairlist(alist(Z = )),
      as.pairlist(alist(Z = 10, ... = )),
      NULL,
      as.pairlist(alist(deep = FALSE))
    ))
  )
})

test_that("extract_r6_super_data", {
  eval(parse(test_path("testR6/R/classes.R"), keep.source = TRUE))

  D <- extract_r6_super_data(C)
  mypkg <- environmentName(topenv())
  expect_equal(D$classes$package, rep(mypkg, 2))
  expect_equal(D$classes$classname, c("B", "A"))
  expect_equal(D$members$package, rep(mypkg, 18))
  expect_equal(D$members$classname, rep(c("B", "A"), c(8, 10)))
  expect_equal(
    D$members$type,
    c(
      rep("method", 3),
      rep("field", 2),
      rep("active", 3),
      rep("method", 4),
      rep("field", 3),
      rep("active", 3)
    )
  )
  expect_equal(
    D$members$name,
    c(
      "meth4",
      "meth1",
      "clone",
      "field4",
      "field1",
      "active5",
      "active4",
      "active1",
      "meth3",
      "meth2",
      "meth1",
      "clone",
      "field3",
      "field2",
      "field1",
      "active3",
      "active2",
      "active1"
    )
  )
})

test_that("extract_r6_fields", {
  C <- R6::R6Class(
    public = list(
      field1 = NULL,
      meth1 = function() {},
      field2 = "foobar"
    )
  )
  F <- extract_r6_fields(C)
  expect_equal(F$type, rep("field", 2))
  expect_equal(F$name, c("field1", "field2"))

  C <- R6::R6Class(
    public = list(
      meth1 = function() {}
    )
  )
  F <- extract_r6_fields(C)
  expect_s3_class(F, "data.frame")
  expect_equal(F$type, character())
  expect_equal(F$name, character())

  C <- R6::R6Class()
  F <- extract_r6_fields(C)
  expect_s3_class(F, "data.frame")
  expect_equal(F$type, character())
  expect_equal(F$name, character())
})

test_that("extract_r6_bindings", {
  C <- R6::R6Class(
    active = list(
      bind1 = function(x) {},
      bind2 = function(x) {}
    ),
    public = list(
      meth1 = function() {}
    )
  )
  F <- extract_r6_bindings(C)
  expect_equal(F$type, rep("active", 2))
  expect_equal(F$name, c("bind1", "bind2"))

  C <- R6::R6Class(
    public = list(
      meth1 = function() {}
    )
  )
  F <- extract_r6_bindings(C)
  expect_s3_class(F, "data.frame")
  expect_equal(F$type, character())
  expect_equal(F$name, character())

  C <- R6::R6Class()
  F <- extract_r6_bindings(C)
  expect_s3_class(F, "data.frame")
  expect_equal(F$type, character())
  expect_equal(F$name, character())
})

test_that("can disable R6 handling", {
  text <- "
    #' Class
    C <- R6::R6Class(
      public = list(
        field = NULL,
        #' @description Method desc.
        #' @param arg Method arg.
        meth = function(arg) { }
      )
    )"
  local_roxy_meta_set("r6", FALSE)

  out <- roc_proc_text(rd_roclet(), text)[[1]]
  rd <- format(out)
  expect_false(grepl("section{Methods}", rd, fixed = TRUE))
  expect_true(grepl("arguments{", rd, fixed = TRUE))
})
