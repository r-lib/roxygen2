source('../R/roxygen.R')
source('../R/functional.R')
source('../R/list.R')
source('../R/memoize.R')
source('../R/parse.R')
source('../R/string.R')
source('../R/roclet.R')
source('../R/namespace.R')
source('../R/collate.R')
source('../R/Rd.R')
source('../R/callgraph.R')
library(bibtex)

debug <- function(..., print=str)
  print(list(...))

register.preref.parsers(parse.name,
                        'cite',
                        'bibliography')

make.bibtex.roclet <- function() {
  roclet <- make.roclet()
  roclet$bibliography <- 'REFERENCES.bib'
  roclet$citationList <- NULL

  ## candidate for pre.files
  parse.bibliography <- function(key, expression)
    roclet$bibliography <- expression

  parse.cite <- function(key, expression) {
    if (is.nil(roclet$citationList))
      roclet$citationList <- read.bib(file=roclet$bibliography)
    for (citation in roclet$citationList) {
      if (attributes(citation)$key == expression) {
        cat(sprintf("%s. %s. %s. %s.\n",
                    citation$editor,
                    citation$title,
                    citation$publisher,
                    citation$year))
        return()
      }
    }
    warning(sprintf("bibtex: unknown key: %s", expression),
            call.=FALSE,
            immediate.=TRUE)
  }

  roclet$register.parser('cite', parse.cite)
  roclet
}

make.bibtex.roclet()$parse.parsed(parse.text("#' @cite frankfurt:2010",
                                             "#' @cite non-existent-key",
                                             "NA"))
