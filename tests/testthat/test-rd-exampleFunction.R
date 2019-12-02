test_that("@exampleFile explicit function", {
  # exampleFunction only works, if package name can be identified
  # which is the case, if executed in package directory with DESCRIPTION file
  roc_proc_inPkgDir <- function(){
    prevWd <- setwd(system.file(package = "roxygen2"))
    on.exit(setwd(prevWd))
    roc_proc_text(rd_roclet(), "
    #' @name a
    #' @title a
    #'
    #' @exampleFunction example_fromFunctionBody
    NULL")[[1]]
  }
  out <- roc_proc_inPkgDir()
  examples <- as.character(out$get_value("examples"))
  # during testing the srcref is not available with the comment
  # expect_match(examples, "# comment from example_fromFunctionBody", all = FALSE)
  expect_match(examples, 'testPercent <- "\\\\%d\\\\%m\\\\%Y"', all = FALSE)
})

test_that("@exampleFile default function name", {
  # test construction of function name from call on argument 'default'
  roc_proc_inPkgDir <- function(){
    prevWd <- setwd(system.file(package = "roxygen2"))
    on.exit(setwd(prevWd))
    roc_proc_text(rd_roclet(), "
    #' someTitle
    #'
    #' @exampleFunction default
    fromFunctionBody <- function(){}")[[1]]
  }
  out <- roc_proc_inPkgDir()
  examples <- as.character(out$get_value("examples"))
  expect_match(examples, 'testPercent <- "\\\\%d\\\\%m\\\\%Y"', all = FALSE)
})

test_that("@exampleFile default function name for NULL", {
  # test construction of function name from call on argument 'default'
  roc_proc_inPkgDir <- function(){
    prevWd <- setwd(system.file(package = "roxygen2"))
    on.exit(setwd(prevWd))
    roc_proc_text(rd_roclet(), "
    #' @name a
    #' @title a
    #'
    #' @exampleFunction default
    NULL")[[1]]
  }
  expect_warning(
    out <- roc_proc_inPkgDir()
    ,"cannot determine default function name"
  )
  examples <- as.character(out$get_value("examples"))
  expect_equal(examples, character(0))
})

test_that("@exampleFile package warning", {
  # test construction of function name from call on argument 'default'
  roc_proc_inPkgDir <- function(){
    prevWd <- setwd(tempdir())
    on.exit(setwd(prevWd))
    roc_proc_text(rd_roclet(), "
    #' @name a
    #' @title a
    #'
    #' @exampleFunction example_fromFunctionBody
    NULL")[[1]]
  }
  expect_warning(
    out <- roc_proc_inPkgDir()
    ,"cannot not determine package name"
  )
  examples <- as.character(out$get_value("examples"))
  expect_equal(examples, character(0))
})

test_that("@exampleFile warning on wrong explicit function", {
  # exampleFunction only works, if package name can be identified
  # which is the case, if executed in package directory with DESCRIPTION file
  roc_proc_inPkgDir <- function(){
    prevWd <- setwd(system.file(package = "roxygen2"))
    on.exit(setwd(prevWd))
    roc_proc_text(rd_roclet(), "
    #' @name a
    #' @title a
    #'
    #' @exampleFunction nonExistingFunction
    NULL")[[1]]
  }
  expect_warning(
    out <- roc_proc_inPkgDir()
    ,"'nonExistingFunction' not found"
  )
  examples <- as.character(out$get_value("examples"))
  expect_equal(examples, character(0))
})



.tmp.f <- function(){
  # debugging roxy_tag_rd.roxy_tag_exampleFunction
  stop("base_path=",normalizePath(base_path)," pkgName=",pkgName)
  #utils::dump.frames("tag_exampleFunction", to.file = TRUE, include.GlobalEnv = TRUE)
  #stop("produced dumpfile in ",getwd())
  #utils::dump.frames("tag_exampleFunction", to.file = TRUE, include.GlobalEnv = TRUE)
  #stop("func=",body(func),", src=",src,", code=",code, ",wd=",getwd() )
  setwd("")
  load("tag_exampleFunction.rda")
  debugger(tag_exampleFunction)
}
