library(testthat)

context("Wrapping DESCRIPTION fields only when necessary")
test_that("wrap_field_if_necessary does not wrap fields 
           whose line length is less than the wrap.threshold", 
{
  less_than_80_characters <- c(
    "Author: Alan Turing",
    "    Alonzo Church"
  )
  
  expect_that(less_than_80_characters, equals(wrap_field_if_necessary()))
             

}

)