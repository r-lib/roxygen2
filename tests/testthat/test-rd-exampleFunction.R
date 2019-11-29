test_that("@exampleFile standard", {
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
  expect_match(examples, 'tmp2 <- "example_fromFunctionBody2"', all = FALSE)
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
