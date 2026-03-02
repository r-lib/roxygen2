test_that("rawRd inserted unchanged", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    #' @rawRd #this is a comment
    #' @name a
    #' @title a
    NULL"
  )[[1]]

  lines <- strsplit(format(out), "\n")[[1]]
  expect_equal(lines[[9]], "#this is a comment")
})

test_that("evalRd inserted unchanged", {
  out <- roc_proc_text(
    rd_roclet(),
    "
    z <- 10
    #' @evalRd as.character(z * 2)
    #' @name a
    #' @title a
    NULL"
  )[[1]]

  expect_equal(out$get_value("rawRd"), "20")
})

test_that("error-ful evalRd generates warning", {
  expect_snapshot({
    expect_parse_failure(roxy_tag_eval(roxy_test_tag(val = 1)))
    expect_parse_failure(roxy_tag_eval(roxy_test_tag(val = NA_character_)))
    expect_parse_failure(roxy_tag_eval(roxy_test_tag(
      val = quote(stop('Uhoh'))
    )))
  })
})
