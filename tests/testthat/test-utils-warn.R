test_that("warn_roxy_function works when fun doesn't have a srcref", {
  fun <- zap_srcref(function() {})

  expect_snapshot(warn_roxy_function(fun, "test message"))
})
