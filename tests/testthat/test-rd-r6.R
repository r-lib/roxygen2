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
  eval(parse(test_path("roxygen-block-3.R"), keep.source = TRUE))

  D <- extract_r6_super_data(C)
  mypkg <- environmentName(topenv())
  expect_equal(D$classes$package, rep(mypkg, 2))
  expect_equal(D$classes$classname, c("B", "A"))
  expect_equal(D$members$package, rep(mypkg, 18))
  expect_equal(D$members$classname, rep(c("B", "A"), c(8, 10)))
  expect_equal(
    D$members$type,
    c(
      "method",
      "method",
      "method",
      "field",
      "field",
      "active",
      "active",
      "active",
      "method",
      "method",
      "method",
      "method",
      "field",
      "field",
      "field",
      "active",
      "active",
      "active"
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

test_that("r6_fields", {
  text <- "
    #' @title Title
    #' @description Description.
    #' @details Details.
    #' @field field1 Foo.
    #' @field field2 Bar.
    #' @field bind1 Active binding.
    C <- R6::R6Class(
      public = list(
        field1 = NULL,
        field2 = \"foobar\"
      ),
      active = list(
        bind1 = function(x) { }
      )
    )"
  block <- parse_text(text)[[1]]
  r6data <- block_get_tag_value(block, ".r6data")
  expect_silent(doc <- r6_fields(block, r6data))

  expect_true(any(grepl("code{field1}}{Foo.", doc, fixed = TRUE)))
  expect_true(any(grepl("code{field2}}{Bar.", doc, fixed = TRUE)))
})

test_that("r6_active_bindings", {
  text <- "
    #' @title Title
    #' @description Description.
    #' @details Details.
    #' @field bind1 Active binding.
    #' @field bind2 Active 2.
    C <- R6::R6Class(
      public = list(
        field1 = NULL,
        field2 = \"foobar\"
      ),
      active = list(
        bind1 = function(x) { },
        bind2 = function(x) { }
      )
    )"
  block <- parse_text(text)[[1]]
  r6data <- block_get_tag_value(block, ".r6data")
  expect_silent(doc <- r6_active_bindings(block, r6data))

  expect_true(any(grepl("code{bind1}}{Active binding.", doc, fixed = TRUE)))
  expect_true(any(grepl("code{bind2}}{Active 2.", doc, fixed = TRUE)))
})

test_that("R6 edge cases, class without methods", {
  text <- "
    #' @title Title
    #' @description Description.
    #' @details Details.
    #' @field field1 Field.
    #' @field field2 Another field.
    #' @field bind1 Active binding.
    #' @field bind2 Active 2.
    C <- R6::R6Class(
      cloneable = FALSE,
      public = list(
        field1 = NULL,
        field2 = \"foobar\"
      ),
      active = list(
        bind1 = function(x) { },
        bind2 = function(x) { }
      )
    )"
  block <- parse_text(text)[[1]]
  rd <- RoxyTopic$new()

  expect_silent(topic_add_r6_methods(rd, block, environment()))
  expect_false(grepl("method", format(rd), ignore.case = TRUE))
})

test_that("R6 edge cases, class without (documented) fields", {
  text <- "
    #' @title Title
    #' @description Description.
    #' @details Details.
    #' @field bind1 Active binding.
    #' @field bind2 Active 2.
    C <- R6::R6Class(
      public = list(
      ),
      active = list(
        bind1 = function(x) { },
        bind2 = function(x) { }
      )
    )"
  block <- parse_text(text)[[1]]
  rd <- RoxyTopic$new()

  expect_silent(topic_add_r6_methods(rd, block, environment()))
  expect_false(grepl("field", format(rd), ignore.case = TRUE))

  text <- "
    #' @title Title
    #' @description Description.
    #' @details Details.
    #' @field bind1 Active binding.
    #' @field bind2 Active 2.
    C <- R6::R6Class(
      public = list(
        undocumented_field = NULL
      ),
      active = list(
        bind1 = function(x) { },
        bind2 = function(x) { }
      )
    )"
  block <- parse_text(text)[[1]]
  rd <- RoxyTopic$new()

  expect_snapshot(topic_add_r6_methods(rd, block, environment()))
  expect_false(grepl("field", format(rd), ignore.case = TRUE))
})

test_that("R6 edge cases, class without active bindings", {
  text <- "
    #' @title Title
    #' @description Description.
    #' @details Details.
    #' @field field1 Field.
    #' @field field2 Another field.
    C <- R6::R6Class(
      public = list(
        field1 = NULL,
        field2 = \"foobar\"
      ),
      active = list(
      )
    )"
  block <- parse_text(text)[[1]]
  rd <- RoxyTopic$new()

  expect_silent(topic_add_r6_methods(rd, block, environment()))
  expect_false(grepl("active", format(rd), ignore.case = TRUE))
})

test_that("R6 edge cases, class without anything", {
  text <- "
    #' @title Title
    #' @description Description.
    #' @details Details.
    C <- R6::R6Class(
      cloneable = FALSE,
      public = list(
      ),
      active = list(
      )
    )"
  block <- parse_text(text)[[1]]
  rd <- RoxyTopic$new()

  expect_silent(topic_add_r6_methods(rd, block, environment()))
  doc <- format(rd)
  expect_false(grepl("method", format(rd), ignore.case = TRUE))
  expect_false(grepl("field", format(rd), ignore.case = TRUE))
  expect_false(grepl("active", format(rd), ignore.case = TRUE))
})

test_that("warning if no method comes after the docs", {
  text <- "
    #' @title Title
    #' @description Description.
    #' @details Details.
    #' @field field1 Yep.
    C <- R6::R6Class(
      public = list(
        #' @description Method 1.
        method1 = function() { },
        #' @description Dangling.
        field1 = NULL
      )
    )"

  eval(parse(text = text, keep.source = TRUE))
  block <- parse_text(text, env = environment())[[1]]
  rd <- RoxyTopic$new()

  expect_snapshot(topic_add_r6_methods(rd, block, environment()))
  doc <- format(rd)
})

test_that("class with no inherited methods", {
  text <- "
    C1 <- R6::R6Class('C1', cloneable = FALSE)

    #' @title Title
    #' @description Description.
    #' @details Details.
    C2 <- R6::R6Class('C2',
      inherit = C1,
      cloneable = FALSE,
      public = list(
        #' @description method1
        meth1 = function() 1
      )
    )"

  env <- new.env(parent = globalenv())

  eval(parse(text = text, keep.source = TRUE), envir = env)
  block <- parse_text(text, env = env)[[1]]
  rd <- RoxyTopic$new()

  topic_add_r6_methods(rd, block, env)
  expect_snapshot(cat(format(rd$get_section("rawRd"))))
})


test_that("integration test", {
  wd <- getwd()
  on.exit(setwd(wd), add = TRUE)
  setwd(test_path())

  env <- new.env(parent = asNamespace("roxygen2"))
  eval(
    parse(test_path("roxygen-block-3.R"), keep.source = TRUE),
    envir = env
  )

  blocks <- parse_file(test_path("roxygen-block-3.R"), env = env)

  roc <- roclet_preprocess(roclet_find("rd"))

  expect_snapshot(
    res <- roclet_process(
      roc,
      blocks = blocks,
      env = env,
      base_path = test_path()
    )
  )

  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  for (n in names(res)) {
    path <- test_path(paste0("roxygen-block-3-", n))
    verify_output(path, res[[n]])
    cat(format(res[[n]]), file = tmp)
    expect_silent(chk <- tools::checkRd(tmp))
    expect_equal(length(chk), 0L)
  }
})

test_that("r6 option", {
  text <- "
    #' @title Title
    #' @description Description.
    C <- R6::R6Class(
      public = list(
        field = NULL,
        #' @description Method desc.
        #' @param arg Method arg.
        meth = function(arg) { }
      )
    )"
  local_roxy_meta_set("r6", FALSE)

  expect_silent(
    out <- roc_proc_text(rd_roclet(), text)
  )
  rd <- format(out$C.Rd)
  expect_false(grepl("section{Methods}", rd, fixed = TRUE))
  expect_true(grepl("arguments{", rd, fixed = TRUE))
})
