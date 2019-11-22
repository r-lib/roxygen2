test_that("format has nice defaults for bare vectors", {
  verify_output(test_path("test-object-format.txt"), {
    call_to_format(x <- list(a = 1, b = 2))
    call_to_format(x <- ordered(letters[1:5]))
    call_to_format(x <- diag(10))
    call_to_format(x <- array(1:27, dim = c(3, 3, 3)))
    call_to_format(x <- data.frame(a = 1, b = 2))
  })
})
