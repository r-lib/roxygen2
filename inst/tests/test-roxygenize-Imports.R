context("Update Imports directives within DESCRIPTION")
Sys.setlocale(locale="en_AU") # makes the sort match my sort order for these tests.

test_that("namespace.imports works as expected", {
  nf <- "namespace-example1.txt"
  expect_true(file.exists(nf))
  imp <- namespace.imports(nf)
  # cat(imp, Sys.getlocale())
  exp <- c("affy", "Biobase", "devtools", "ggplot2")
  expect_equal(imp, exp)
})


test_that("Imports updates as expected", {
  df <- "description-example.txt"
  nf <- "namespace-example1.txt"
  expect_true(file.exists(df))
  expect_true(file.exists(nf))

  pkg <- temppackage(df, nf)

  ns.imports <- namespace.imports(file.path(pkg, "NAMESPACE"))
  exp <- c("affy", "Biobase", "devtools", "ggplot2")
  expect_equal(ns.imports, exp)

  desc <- read.description(file.path(pkg, "DESCRIPTION"))
  desc.imports <- parse.dependencies(desc$Imports, exclude.R=TRUE)
  exp.desc.imports <- structure(c("stringr", "tools", "brew"), .Names = c("stringr (>= 0.5)", 
  "tools", "brew"))
  expect_equal(desc.imports, exp.desc.imports)

  expect_message(roxygenize(pkg, roclets=character()), "Updating Imports directive in.*")
  
  # Imports in DESCRIPTION should have been updated
  desc <- read.description(file.path(pkg, "DESCRIPTION"))
  desc.imports <- parse.dependencies(desc$Imports, exclude.R=TRUE)
  exp.desc.imports<- structure(c("affy", "Biobase", "devtools", "ggplot2"), .Names = c("affy", 
  "Biobase", "devtools", "ggplot2"))
  expect_equal(desc.imports, exp.desc.imports)
})

test_that("Imports don't update as expected", {
  df <- "description-example.txt"
  nf <- "namespace-example2.txt"
  expect_true(file.exists(df))
  expect_true(file.exists(nf))
  
  pkg <- temppackage(df, nf)
  
  ns.imports <- namespace.imports(file.path(pkg, "NAMESPACE"))
  exp.ns.imports <- c("brew", "stringr", "tools")
  expect_equal(ns.imports, exp.ns.imports)
  
  desc <- read.description(file.path(pkg, "DESCRIPTION"))
  desc.imports <- parse.dependencies(desc$Imports, exclude.R=TRUE)
  exp.desc.imports <- structure(c("stringr", "tools", "brew"), .Names = c("stringr (>= 0.5)", 
  "tools", "brew"))
  expect_equal(desc.imports, exp.desc.imports)

  roxygenize(pkg, roclets=character())
  
  # Imports in DESCRIPTION should NOT have been updated
  desc <- read.description(file.path(pkg, "DESCRIPTION"))
  desc.imports <- parse.dependencies(desc$Imports, exclude.R=TRUE)
  exp.desc.imports <- structure(c("stringr", "tools", "brew"), .Names = c("stringr (>= 0.5)", 
  "tools", "brew"))
  expect_equal(desc.imports, exp.desc.imports)
})


test_that("Imports deletions occur as expected", {
  df <- "description-example.txt"
  nf <- "namespace-example3.txt"
  expect_true(file.exists(df))
  expect_true(file.exists(nf))
  
  pkg <- temppackage(df, nf)
  
  ns.imports <- namespace.imports(file.path(pkg, "NAMESPACE"))
  exp.ns.imports <- c("stringr", "tools")
  expect_equal(ns.imports, exp.ns.imports)
  
  desc <- read.description(file.path(pkg, "DESCRIPTION"))
  desc.imports <- parse.dependencies(desc$Imports, exclude.R=TRUE)
  exp.desc.imports <- structure(c("stringr", "tools", "brew"), .Names = c("stringr (>= 0.5)", 
  "tools", "brew"))
  expect_equal(desc.imports, exp.desc.imports)

  expect_message(roxygenize(pkg, roclets=character()), "Updating Imports directive in.*")
  
  # Imports in DESCRIPTION should have been updated
  desc <- read.description(file.path(pkg, "DESCRIPTION"))
  desc.imports <- parse.dependencies(desc$Imports, exclude.R=TRUE)
  exp.desc.imports <- structure(c("stringr", "tools"), .Names = c("stringr (>= 0.5)", 
  "tools"))
  expect_equal(desc.imports, exp.desc.imports)
})
