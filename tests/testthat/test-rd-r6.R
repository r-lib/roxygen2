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

  expect_match(paste(doc, collapse = "\n"), "code{field1}}{Foo.", fixed = TRUE)
  expect_match(paste(doc, collapse = "\n"), "code{field2}}{Bar.", fixed = TRUE)
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

  expect_match(
    paste(doc, collapse = "\n"),
    "code{bind1}}{Active binding.",
    fixed = TRUE
  )
  expect_match(
    paste(doc, collapse = "\n"),
    "code{bind2}}{Active 2.",
    fixed = TRUE
  )
})

test_that("class without methods omits Methods section", {
  doc <- r6_doc(
    "
    #' @title Title
    #' @description Description.
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
  )
  expect_no_match(doc, "method", ignore.case = TRUE)
})

test_that("class without fields omits Public fields section", {
  doc <- r6_doc(
    "
    #' @title Title
    #' @description Description.
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
  )
  expect_no_match(doc, "field", ignore.case = TRUE)
})

test_that("undocumented field warns", {
  text <- "
    #' @title Title
    #' @description Description.
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
  expect_snapshot(invisible(r6_doc(text)))
})

test_that("class without active bindings omits Active bindings section", {
  doc <- r6_doc(
    "
    #' @title Title
    #' @description Description.
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
  )
  expect_no_match(doc, "active", ignore.case = TRUE)
})

test_that("class without anything omits all sections", {
  doc <- r6_doc(
    "
    #' @title Title
    #' @description Description.
    C <- R6::R6Class(
      cloneable = FALSE,
      public = list(
      ),
      active = list(
      )
    )"
  )
  expect_no_match(doc, "method", ignore.case = TRUE)
  expect_no_match(doc, "field", ignore.case = TRUE)
  expect_no_match(doc, "active", ignore.case = TRUE)
})

test_that("warning if no method comes after the docs", {
  text <- "
    #' @title Title
    #' @description Description.
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
  rd <- r6_topic(text, env)
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

test_that("method with markdown sections in @description and @details", {
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

  rd <- r6_topic(text)
  expect_snapshot(cat(format(rd$get_section("rawRd"))))
})

test_that("r6_flatten_sections collapses markdown sections", {
  local_markdown()

  tag <- roxy_tag(
    "details",
    "Some details.\n\n# A heading\n\nBody."
  )
  tag <- tag_markdown_with_sections(tag)
  expect_length(tag$val, 2)

  tag <- r6_flatten_sections(tag)
  expect_length(tag$val, 1)
  expect_match(tag$val, "Some details.", fixed = TRUE)
  expect_match(tag$val, "\\subsection{A heading}", fixed = TRUE)
  expect_match(tag$val, "Body.", fixed = TRUE)
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
  expect_no_match(rd, "section{Methods}", fixed = TRUE)
  expect_match(rd, "arguments{", fixed = TRUE)
})
