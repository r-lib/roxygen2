capture_roclet_output <- function(roclet, ...) {
  capture.output(roclet$parse.parsed(parse.text(...)))
}
