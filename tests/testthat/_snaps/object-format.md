# format has nice defaults for bare vectors

    Code
      call_to_format(x <- list(a = 1, b = 2))
    Output
      [1] "An object of class \\code{list} of length 2."
    Code
      call_to_format(x <- ordered(letters[1:5]))
    Output
      [1] "An object of class \\code{ordered} (inherits from \\code{factor}) of length 5."
    Code
      call_to_format(x <- diag(10))
    Output
      [1] "An object of class \\code{matrix} (inherits from \\code{array}) with 10 rows and 10 columns."
    Code
      call_to_format(x <- array(1:27, dim = c(3, 3, 3)))
    Output
      [1] "An object of class \\code{array} of dimension 3 x 3 x 3."
    Code
      call_to_format(x <- data.frame(a = 1, b = 2))
    Output
      [1] "An object of class \\code{data.frame} with 1 rows and 2 columns."

