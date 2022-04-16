test_that("extract_r6_data without source refs", {
  txt <- "R6::R6Class('foo',
     public = list(
       field1 = NULL,
       meth1 = function(Z) { },
       meth2 = function(Z = 10, ...) { },
       field2 = \"foobar\",
       meth3 = function() { }
     )
   )"
  C <- eval(parse(text = txt, keep.source = FALSE))
  expect_snapshot(extract_r6_data(C), error = TRUE)
})
