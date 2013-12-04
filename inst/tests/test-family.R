context("Family")
roc <- rd_roclet()

test_that("long families are wrapped", {
  out <- roc_proc_text(roc, "
    #' Title
    #' @family Long family name
    long_function_name_________________________1 <- function() {}

    #' Title
    #' @family Long family name
    long_function_name_________________________2 <- function() {}
    
    #' Title
    #' @family Long family name
    long_function_name_________________________3 <- function() {}
    
    #' Title
    #' @family Long family name
    long_function_name_________________________4 <- function() {}  
  ")[[1]]
  
  seealso <- get_tag(out, "seealso")$values
  expect_equal(str_count(seealso, "\n"), 2)

})