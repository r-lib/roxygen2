expect_equivalent_rd <- function(out1, out2) {
  out1$fields$backref <- NULL
  out2$fields$backref <- NULL
  expect_equal(out1, out2)
}
