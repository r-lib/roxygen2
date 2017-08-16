context("Rd: bibliography")

roc_proc_text <- function(...){
  res <- roxygen2::roc_proc_text(...)
  # reset bibliography object
  RoxyBibObject(reset = TRUE)
  res
}

test_that("@cite creates @references tags", {

  expect_warning(out <- roc_proc_text(rd_roclet(), "
#' Title
#' @cite ABC2017
#'
f <- function(){}          
")[[1]], , "Could not find .* key.* ABC2017")
  expect_equal(get_tag(out, "references")$values, "ABC2017")  

})

test_that("\\cite creates a @references tag and formats missing citation", {
  
  expect_warning(out <- roc_proc_text(rd_roclet(), "
#' Title
#'
#' This performs method from \\cite{ABC2017}
#'
f <- function(){}          
")[[1]], "Could not find .* key.* ABC2017")
  expect_equal(get_tag(out, "references")$values, "ABC2017")  
  expect_equal(get_tag(out, "description")$values, "This performs method from ABC2017")
  
})


test_that("\\cite creates a @references tag and formats citations from @bibliography", {
  
  bib <- c(bibentry('Article', key = 'ABC2017'
                , title = 'Some article', year = '2017'
                , author = as.person('John ABC, Bob DEF, Bill GHI')
                , journal = 'A good journal')
        , bibentry('Article', key = 'Doe2000'
            , title = 'Another article', year = '2000'
            , author = as.person('Albert Doe')
            , journal = 'Another journal')
        , bibentry('Article', key = 'Beta2005'
            , title = 'A third article', year = '2005'
            , author = as.person('Alpha Beta')
            , journal = 'Yet another journal')
    )
  bibfile <- tempfile(fileext = ".bib")
  on.exit( unlink(bibfile) )
  bibtex::write.bib(bib, file = bibfile)
  
  out <- roc_proc_text(rd_roclet(), sprintf("
#' @bibliography %s
NULL

#' Title
#'
#' This performs method from \\cite{ABC2017}
#'
#' @details
#' See \\cite{ABC2017}
#' 
#' @param x a vector as per \\cite{ABC2017}
#' @param y a matrix as per \\cite{ABC2017; Doe2000}
#' @section A section:
#' This is detailed in \\cite{Doe2000}
#' @references A verbatim reference
#' @cite Beta2005 
f <- function(x, y){}
", bibfile))[[1]]

  # references are included in the order: explicit tag, @cite, \\cite
  expect_equal(get_tag(out, "references")$values
              , c('A verbatim reference'
                  , 'Beta A (2005). “A third article.” _Yet another journal_.'
                  , 'ABC J, DEF B and GHI B (2017). “Some article.” _A good journal_.'
                  , 'Doe A (2000). “Another article.” _Another journal_.'))
  # check that cite substitution is performed in:
  # - description
  expect_equal(get_tag(out, "description")$values, "This performs method from ABC et al. (2017)")
  # - details
  expect_equal(get_tag(out, "details")$values, "See ABC et al. (2017)")
  # - section content
  expect_match(get_tag(out, "section")$content, "This is detailed in Doe \\(2000\\)")
  # - param description
  expect_equal(get_tag(out, "param")$values[['x']], "a vector as per ABC et al. (2017)")
  # check multiple column-separated keys
  expect_equal(get_tag(out, "param")$values[['y']], "a matrix as per ABC et al. (2017), Doe (2000)")
  
})
