test_that("finds arguments when S4 method wrapped inside .local()", {
  env <- pkg_env()
  setClass("Foo", where = env)
  call <- quote(
    setMethod('subset', 'Foo', function(x, foo, ...) {})
  )
  eval(call, envir = env)

  obj <- object_from_call(call, env, block = NULL, file = NULL)
  expect_s3_class(obj, "s4method")
  expect_named(formals(obj$value@.Data), c("x", "foo", "..."))
})
