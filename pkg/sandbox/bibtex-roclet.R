library(roxygen)
library(bibtex)

debug <- function(..., print=str)
  print(list(...))

register.preref.parsers(parse.name,
                        'cite',
                        'bibliography')

make.bibtex.roclet <- function() {
  roclet <- make.roclet()
  roclet$bibliography <- 'REFERENCES.bib'

  parse.bibliography <- function(key, expression)
    roclet$bibliography <- expression

  parse.cite <- function(key, expression) {
    citationList <- read.bib(file=roclet$bibliography)
    for (citation in citationList) {
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

  roclet$register.parser('bibliography', parse.bibliography)
  roclet$register.parser('cite', parse.cite)
  roclet
}

make.bibtex.roclet()$parse.parsed(parse.text("#' @bibliography REFERENCES.bib",
                                             "#' @cite frankfurt:2010",
                                             "#' @cite non-existent-key",
                                             "NA"))
