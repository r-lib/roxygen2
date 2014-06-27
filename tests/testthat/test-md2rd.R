context("md2rd")
roc <- md2rd_roclet()

test_that("`code` formatting works", {
  out <- roc_proc_text(roc, "
    #' Title 
    #'
    #' Description contains `text formatted as code`.
    #' @name a
    NULL")[[1]]
  
  expect_equal(get_tag(out, "description")$values, 
               "Description contains \\code{text formatted as code}.")
})

test_that("**Bold** and *emph* formatting works", {
  out <- roc_proc_text(roc, "
    #' Title 
    #'
    #' Description contains **bold text**.
    #' @details  Details contain *italics*.
    #' 
    #' @name a
    NULL")[[1]]
  
  expect_equal(get_tag(out, "description")$values, 
               "Description contains \\bold{bold text}.")
  expect_equal(get_tag(out, "details")$values, 
               "Details contain \\emph{italics}.")
})


test_that("Enumerated lists work.", {
  out <- roc_proc_text(roc, "
    #' Title 
    #'
    #' Description contains enumeration:
    #' 1. First item.
    #'    4 spaces continue first item on second line.
    #' * 2nd item
    #' 3. 3rd item
    #'    4 more spaces.
    #' Not part of enumeration.
    #' @name a
    NULL")[[1]]
  
  expect_equal(get_tag(out, "description")$values, 
               "Description contains enumeration:\n\\enumerate{ \n\\item First item.\n   4 spaces continue first item on second line.\n\\item 2nd item\n\\item 3rd item\n   4 more spaces. }\nNot part of enumeration.")
})


test_that("Itemized lists work.", {
  out <- roc_proc_text(roc, "
    #' Title 
    #'
    #' Description contains enumeration:
    #' * First item.
    #'    4 spaces continue first item on second line.
    #' * 2nd item
    #' * 3rd item
    #'    4 more spaces.
    #' Not part of itemization.
    #' @name a
    NULL")[[1]]
  
  expect_equal(get_tag(out, "description")$values, 
               "Description contains enumeration:\n\\itemize{ \n\\item First item.\n   4 spaces continue first item on second line.\n\\item 2nd item\n\\item 3rd item\n   4 more spaces. }\nNot part of itemization.")
})


test_that("Link formatting works.", {
  out <- roc_proc_text(roc, "
    #' Title 
    #'
    #' Description contains link to ![t.test](stats).
    #' 
    #' Details contain simple link to ![function] in same package.
    #' @name a
    NULL")[[1]]
  
  expect_equal(get_tag(out, "description")$values, 
               "Description contains link to \\link[stats]{t.test}.")
  expect_equal(get_tag(out, "details")$values, 
               "Details contain simple link to \\link{function} in same package.")
})




test_that("Math formatting works.", {
  out <- roc_proc_text(roc, "
    #' Title 
    #'
    #' Description has in-line equation $y = 10$.
    #' 
    #' @details Details has displayed equation $$\\log(1)=0.$$
    #' @name a
    NULL")[[1]]
  
  expect_equal(get_tag(out, "description")$values, 
               "Description has in-line equation \\eqn{y = 10}.")
  expect_equal(get_tag(out, "details")$values, 
               "Details has displayed equation \\deqn{\\log(1)=0.}")
})
