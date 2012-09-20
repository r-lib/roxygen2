
test_tags = c('testthat', 'context','tests','test')

roxygen2:::register.preref.parsers(parse.value, test_tags )

#' Roclet: make test files to be run during R CMD check
#'
#' This roclet looks for \code{\link[testthat]{testthat}} tests to copy into
#' files in the test/ directory.
#' @export
testthat_roclet <- function() {
    new_roclet(list, "testthat")
}

#' @S3method roc_process testthat
roc_process.testthat <- function(roclet, partita, base_path) {
  results = list()
  for (partitum in partita) {
    # Work out file name, for now just make one test file per R file 
    filename <- str_c("test-",basename(partitum$srcref$filename))
    
    tests = mapply(testthat_testthat,names(partitum),partitum,base_path)
    tests = tests[str_length(tests)>0]
    if (length(tests)>0) {
      writeLines(str_c("Should put test: ",tests,"\n into file: ",filename))
      othertests = ifelse(is.null(results[[filename]]),
        str_c("## tests generated with roxygen2 from ",basename(partitum$srcref$filename)),
        results[[filename]])

      results[[filename]] = str_c(othertests,"\n",
          do.call(paste,c(tests,list(sep="\n"))))
    }
  }
  results
}

#' @S3method roc_output testthat
roc_output.testthat <- function(roclet, results, base_path) {
  test <- suppressWarnings(normalizePath(file.path(base_path, "test")))
  dir.create(test, recursive=TRUE, showWarnings=FALSE)

  results = results[sapply(results,length)>0]
  test_out_cache = roxygen2:::new_cache()
  contents <- sapply(results, function(x) {
    test_out_cache$compute(x, format(x))
    })


  write_out <- function(filename, contents) {
    if (the_same(filename, contents)) return()
    name <- basename(filename)
    if (!str_detect(name, "^[a-zA-Z][a-zA-Z0-9_.-]*$")) {
      cat("Skipping invalid filename: ", name, "\n")
    } else {
      cat(sprintf('Writing %s\n', name))
      writeLines(contents, filename)    
    }
  }
  the_same <- function(path, new) {
    if (!file.exists(path)) return(FALSE)
    old <- str_c(readLines(path), collapse = "\n")
    return(identical(old, new))
  }
    
  paths <- file.path(test, names(results))

  mapply(write_out, paths, contents)
}

testthat_testthat <- function(tag,value,base_path) {
  if (tag=="tests") {
      value
  }else if (tag=="test") {
    readLines(file.path(base_path, str_trim(paths)))
  }else if (tag=="testthat") {
    wrap_function(value,"test_that")
  }else if (tag=="context") {
    wrap_function(value,"context")
  }else {
    ""
  }
}

wrap_function <- function(text,fun){
  paste(fun,"(\"",text,"\")",sep="")
  }


