test_that("proper link references are added", {
  cases <- list(
    c("foo [func()] bar", "[func()]: R:func()"),
    c("foo [obj] bar", "[obj]: R:obj"),
    c("foo [text][func()] bar", "[func()]: R:func()"),
    c("foo [text][obj] bar", "[obj]: R:obj"),
    c("foo [pkg::func()] bar", "[pkg::func()]: R:pkg::func()"),
    c("foo [pkg::obj] bar", "[pkg::obj]: R:pkg::obj"),
    c("foo [text][pkg::func()] bar", "[pkg::func()]: R:pkg::func()"),
    c("foo [text][pkg::obj] bar", "[pkg::obj]: R:pkg::obj"),
    c("foo [linktos4-class] bar", "[linktos4-class]: R:linktos4-class"),
    c("foo [pkg::s4-class] bar", "[pkg::s4-class]: R:pkg::s4-class")
  )

  for (i in seq_along(cases)) {
    expect_match(
      add_linkrefs_to_md(cases[[i]][1]),
      cases[[i]][2],
      fixed = TRUE
    )
  }
})

test_that("can not have [ inside of link", {
  expect_equal(
    markdown("`[[`. [subset()]"),
    r"(\code{[[}. \code{\link[=subset]{subset()}})"
  )
})

test_that("can escape [ to avoid spurious links", {
  expect_equal(
    markdown("\\[test\\]"),
    "[test]"
  )

  expect_equal(
    markdown("\\[ [test] \\]"),
    r"([ \link{test} ])"
  )
})

test_that("\\Sexpr with options not converted to links", {
  expect_equal(
    markdown(r"(\Sexpr[results=rd]{runif(1)})"),
    r"(\Sexpr[results=rd]{runif(1)})"
  )
})

test_that("% in links are escaped", {
  expect_equal(markdown("[x][%%]"), r"(\link[=\%\%]{x})")
  expect_equal(markdown("[%][x]"), r"(\link[=x]{\%})")
  expect_equal(markdown("[%%]"), r"(\link{\%\%})")
  expect_equal(markdown("[base::%%]"), r"(\link[base:\%\%]{base::\%\%})")
  # %in% can be resolved to base package (#1728)
  expect_equal(markdown("[%in%]"), r"(\link{\%in\%})")
})

test_that("links to topic, not filename", {
  expect_equal(
    markdown("[tools::CRAN_package_db]"),
    r"(\link[tools:CRAN_package_db]{tools::CRAN_package_db})"
  )
})

test_that("{ and } in links are escaped (#1259)", {
  expect_equal(
    markdown("[`foo({ bar })`][x]"),
    r"(\code{\link[=x]{foo(\{ bar \})}})"
  )
  expect_equal(markdown("[`{{`][x]"), r"(\code{\link[=x]{\{\{}})")

  # Non code parts are not escaped (invalid Rd)
  expect_equal(markdown("[foo({ bar })][x]"), r"(\link[=x]{foo({ bar })})")
})

test_that("non-text nodes in links are supported", {
  expect_equal(markdown("[`foo` bar][x]"), r"(\link[=x]{\code{foo} bar})")
  expect_equal(markdown("[__baz__][x]"), r"(\link[=x]{\strong{baz}})")
  expect_equal(markdown("[_italic_][x]"), r"(\link[=x]{\emph{italic}})")
})

test_that("commonmark picks up the various link references", {
  cases <- list(
    list("foo [func()] bar", c("R:func()", "func()")),
    list("foo [obj] bar", c("R:obj", "obj")),
    list("foo [text][func()] bar", c("R:func()", "text")),
    list("foo [text][obj] bar", c("R:obj", "text")),
    list("foo [pkg::func()] bar", c("R:pkg::func()", "pkg::func()")),
    list("foo [pkg::obj] bar", c("R:pkg::obj", "pkg::obj")),
    list("foo [text][pkg::func()] bar", c("R:pkg::func()", "text")),
    list("foo [text][pkg::obj] bar", c("R:pkg::obj", "text")),
    list("foo [linktos4-cl] bar", c("R:linktos4-cl", "linktos4-cl")),
    list("foo [pkg::s4-cl] bar", c("R:pkg::s4-cl", "pkg::s4-cl"))
  )

  for (i in seq_along(cases)) {
    x <- commonmark::markdown_xml(add_linkrefs_to_md(cases[[i]][[1]]))
    xdoc <- xml2::xml_ns_strip(xml2::read_xml(x))
    link <- xml2::xml_find_first(xdoc, "//link")
    expect_equal(xml2::xml_attr(link, "destination"), cases[[i]][[2]][1])
    text <- xml2::xml_find_first(link, "./text")
    expect_equal(xml2::xml_text(text), cases[[i]][[2]][2], info = i)
  }
})

test_that("short and sweet links work", {
  out1 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' Description, see [function()].
    #' And also [object].
    #' @md
    foo <- function() {}"
  )[[1]]
  out2 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' Description, see \\code{\\link[=function]{function()}}.
    #' And also \\link{object}.
    foo <- function() {}"
  )[[1]]
  expect_equivalent_rd(out1, out2)

  expect_snapshot(
    out1 <- roc_proc_text(
      rd_roclet(),
      "
    #' Title
    #'
    #' See [11pkg::function()], [11pkg::object].
    #' @md
    foo <- function() {}"
    )[[1]]
  )
  out2 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' See \\code{\\link[11pkg:function]{11pkg::function()}}, \\link[11pkg:object]{11pkg::object}.
    foo <- function() {}"
  )[[1]]
  expect_equivalent_rd(out1, out2)

  out1 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' Description, see [name][dest].
    #' @md
    foo <- function() {}"
  )[[1]]
  out2 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' Description, see \\link[=dest]{name}.
    foo <- function() {}"
  )[[1]]
  expect_equivalent_rd(out1, out2)

  block <- "
    #' Title
    #'
    #' Description, see [name words][stringr::bar111].
    #' @md
    foo <- function() {}
  "
  expect_snapshot(out1 <- roc_proc_text(rd_roclet(), block)[[1]])

  out2 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' Description, see \\link[stringr:bar111]{name words}.
    foo <- function() {}"
  )[[1]]
  expect_equivalent_rd(out1, out2)

  out1 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' Description, see [terms][terms.object].
    #' @md
    foo <- function() {}"
  )[[1]]
  out2 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' Description, see \\link[=terms.object]{terms}.
    foo <- function() {}"
  )[[1]]
  expect_equivalent_rd(out1, out2)

  out1 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' Description, see [abc][abc-class].
    #' @md
    foo <- function() {}"
  )[[1]]
  out2 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' Description, see \\link[=abc-class]{abc}.
    foo <- function() {}"
  )[[1]]
  expect_equivalent_rd(out1, out2)

  out1 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title.
    #'
    #' In another package: [and this one][desc::desc].
    #' [name words][desc::desc].
    #'
    #' @md
    #' @name markdown-test
    foo <- function() {}"
  )[[1]]
  out2 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title.
    #'
    #' In another package: \\link[desc:desc]{and this one}.
    #' \\link[desc:desc]{name words}.
    #'
    #' @name markdown-test
    foo <- function() {}"
  )[[1]]
  expect_equivalent_rd(out1, out2)
})

test_that("a weird markdown link bug is fixed", {
  out1 <- roc_proc_text(
    rd_roclet(),
    "
    #' Dummy page to test roxygen's markdown formatting
    #'
    #' Links are very tricky, so I'll put in some links here:
    #' Link to a function: [roxygenize()].
    #' Link to an object: [roxygenize] (we just treat it like an object here).
    #'
    #' Link to another package, function: [desc::desc()].
    #' Link to another package, non-function: [desc::desc].
    #'
    #' Link with link text: [this great function][roxygenize()],
    #' [`roxygenize`][roxygenize()], or [that great function][roxygenize].
    #'
    #' In another package: [and this one][desc::desc].
    #'
    #' @md
    #' @name markdown-test
    #' @keywords internal
    NULL"
  )[[1]]
  out2 <- roc_proc_text(
    rd_roclet(),
    "
    #' Dummy page to test roxygen's markdown formatting
    #'
    #' Links are very tricky, so I'll put in some links here:
    #' Link to a function: \\code{\\link[=roxygenize]{roxygenize()}}.
    #' Link to an object: \\link{roxygenize} (we just treat it like an object here).
    #'
    #' Link to another package, function: \\code{\\link[desc:desc]{desc::desc()}}.
    #' Link to another package, non-function: \\link[desc:desc]{desc::desc}.
    #'
    #' Link with link text: \\link[=roxygenize]{this great function},
    #' \\code{\\link{roxygenize}}, or \\link[=roxygenize]{that great function}.
    #'
    #' In another package: \\link[desc:desc]{and this one}.
    #'
    #' @name markdown-test
    #' @keywords internal
    NULL"
  )[[1]]
  expect_equivalent_rd(out1, out2)
})

test_that("another markdown link bug is fixed", {
  out1 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' Description, see [escape_rd_for_md()].
    #'
    #' And also [object].
    #' @md
    foo <- function() {}"
  )[[1]]
  out2 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' Description, see \\code{\\link[=escape_rd_for_md]{escape_rd_for_md()}}.
    #'
    #' And also \\link{object}.
    foo <- function() {}"
  )[[1]]
  expect_equivalent_rd(out1, out2)
})

test_that("markdown code as link text is rendered as code", {
  suppressMessages(
    out1 <- roc_proc_text(
      rd_roclet(),
      "
    #' Title
    #'
    #' Description, see [`name`][dest],
    #' [`function`][function()],
    #' [`filter`][stats::filter()],
    #' [`bar`][pkg::bar],
    #' [`terms`][terms.object],
    #' [`abc`][abc-class].
    #' @md
    foo <- function() {}"
    )[[1]]
  )
  out2 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' Description, see \\code{\\link[=dest]{name}},
    #' \\code{\\link{function}},
    #' \\code{\\link[stats:filter]{filter}},
    #' \\code{\\link[pkg:bar]{bar}},
    #' \\code{\\link[=terms.object]{terms}},
    #' \\code{\\link[=abc-class]{abc}}.
    foo <- function() {}"
  )[[1]]
  expect_equivalent_rd(out1, out2)
})

test_that("non-code link in backticks works", {
  out1 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' Description, see [`foobar`].
    #' Also [`this_too`].
    #' @md
    foo <- function() {}"
  )[[1]]
  out2 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' Description, see \\code{\\link{foobar}}.
    #' Also \\code{\\link{this_too}}.
    foo <- function() {}"
  )[[1]]
  expect_equivalent_rd(out1, out2)
})

test_that("[] is not picked up in code", {
  out1 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' @param connect_args `[named list]`\\cr Connection arguments
    #' Description, see `[foobar]`.
    #' Also `[this_too]`.
    #' @md
    foo <- function() {}"
  )[[1]]
  out2 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' @param connect_args \\verb{[named list]}\\cr Connection arguments
    #' Description, see \\verb{[foobar]}.
    #' Also \\verb{[this_too]}.
    foo <- function() {}"
  )[[1]]
  expect_equivalent_rd(out1, out2)
})

test_that("[]() links are still fine", {
  out1 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' Description, see [some thing](http://www.someurl.com).
    #' @md
    foo <- function() {}"
  )[[1]]
  out2 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' Description, see \\href{http://www.someurl.com}{some thing}.
    foo <- function() {}"
  )[[1]]
  expect_equivalent_rd(out1, out2)

  out1 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' Link
    #' text [broken
    #' across lines](http://www.someurl.com) preserve
    #' whitespace, even when
    #' [broken across
    #' several
    #' lines](http://www.someurl.com),
    #' or with varying
    #' [amounts \
    #'   of  \
    #' interspersed   \
    #'   whitespace](http://www.someurl.com).
    #' @md
    foo <- function() {}"
  )[[1]]
  out2 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' Link
    #' text \\href{http://www.someurl.com}{broken across lines} preserve
    #' whitespace, even when
    #' \\href{http://www.someurl.com}{broken across several lines},
    #' or with varying
    #' \\href{http://www.someurl.com}{amounts of interspersed whitespace}.
    foo <- function() {}"
  )[[1]]
  expect_equivalent_rd(out1, out2)
})

test_that("links to S4 classes are OK", {
  out1 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' Description, see [linktos4-class] as well.
    #' @md
    foo <- function() {}"
  )[[1]]
  out2 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' Description, see \\link[=linktos4-class]{linktos4} as well.
    foo <- function() {}"
  )[[1]]
  expect_equivalent_rd(out1, out2)

  suppressMessages(
    out1 <- roc_proc_text(
      rd_roclet(),
      "
    #' Title
    #'
    #' Description, see [pkg::linktos4-class] as well.
    #' @md
    foo <- function() {}"
    )[[1]]
  )
  out2 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' Description, see \\link[pkg:linktos4-class]{pkg::linktos4} as well.
    foo <- function() {}"
  )[[1]]
  expect_equivalent_rd(out1, out2)
})

test_that("linebreak in 'text' of [text][foo] turns into single space", {
  out1 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' Link
    #' text [broken
    #' across lines][fcn] preserve
    #' whitespace, even when
    #' [broken across
    #' several
    #' lines][fcn],
    #' or with varying
    #' [amounts \
    #'   of  \
    #' interspersed   \
    #'   whitespace][fcn].
    #' @md
    foo <- function() {}"
  )[[1]]
  out2 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' Link
    #' text \\link[=fcn]{broken across lines} preserve
    #' whitespace, even when
    #' \\link[=fcn]{broken across several lines},
    #' or with varying
    #' \\link[=fcn]{amounts of interspersed whitespace}.
    foo <- function() {}"
  )[[1]]
  expect_equivalent_rd(out1, out2)
})

test_that("markup in link text", {
  out1 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' Description, see [`code link text`][func].
    #' And also [`code as well`](https://external.com).
    #' @md
    foo <- function() {}"
  )[[1]]
  out2 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' Description, see \\code{\\link[=func]{code link text}}.
    #' And also \\href{https://external.com}{\\verb{code as well}}.
    foo <- function() {}"
  )[[1]]
  expect_equivalent_rd(out1, out2)
})

test_that("non-code markup in link text", {
  out1 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' See [*italic text*][func] and [**bold text**][func2].
    #' @md
    foo <- function() {}"
  )[[1]]
  out2 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' See \\link[=func]{\\emph{italic text}} and \\link[=func2]{\\strong{bold text}}.
    foo <- function() {}"
  )[[1]]
  expect_equivalent_rd(out1, out2)
})

test_that("linking to self is unqualified", {
  local_roxy_meta_set("current_package", "myself")
  rd <- markdown("foo [myself::fun()] and [myself::obj] bar")
  expect_equal(
    rd,
    r"(foo \code{\link[=fun]{fun()}} and \link{obj} bar)"
  )
})

test_that("resolved links don't change link text (#1662)", {
  local_roxy_meta_set("current_package", "testMdLinks")
  local_roxy_meta_set("current_package_dir", test_path("testMdLinks"))

  expect_equal(
    markdown("[cli_abort()]"),
    r"(\code{\link[cli:cli_abort]{cli_abort()}})"
  )
})

test_that("percents are escaped in link targets", {
  out1 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' [link % text](https://foo.bar/link%20target)
    #' @md
    foo <- function() {}"
  )[[1]]
  out2 <- roc_proc_text(
    rd_roclet(),
    "
    #' Title
    #'
    #' \\href{https://foo.bar/link%20target}{link % text}
    #' @md
    foo <- function() {}"
  )[[1]]
  expect_equivalent_rd(out1, out2)
})

# Helpers ----------------------------------------------------------------------

test_that("generates informative warnings", {
  expect_snapshot({
    tag <- roxy_test_tag()
    check_topic("11papaya", "foo", tag)
    check_topic("stringr", "foofofofoo", tag)
  })
})

test_that("rd_link() builds correct Rd links", {
  expect_equal(rd_link(NA, "topic", "topic"), "\\link{topic}")
  expect_equal(rd_link("pkg", "topic", "text"), "\\link[pkg:topic]{text}")
  expect_equal(rd_link(NA, "topic", "text"), "\\link[=topic]{text}")
})

test_that("fun_suffix() adds () only to non-infix functions", {
  env <- env(f = function() NULL, x = 1, `%op%` = function(a, b) NULL)
  expect_equal(fun_suffix("f", env), "f()")
  expect_equal(fun_suffix("x", env), "x")
  expect_equal(fun_suffix("%op%", env), "%op%")
  expect_equal(fun_suffix("missing", env), "missing")
})
