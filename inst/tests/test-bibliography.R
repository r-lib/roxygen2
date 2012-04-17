# Unit test for tags @cite and @bibliography
# 
# Author: Renaud Gaujoux
# Creation: 17 Apr 2012
###############################################################################
context("Bibliography")
bibroc <- bibliography_roclet()

test_that("tags @cite generates correct citations",
{
	reffile <- system.file('tests/REFERENCES.bib', package='roxygen2')
	refdb <- system.file('tests/REFdb.bib', package='roxygen2')

	bibtags <- str_c("#' @bibliography ", reffile, "\n"
					, "#' @bibliography ", refdb, "\n")
	out <- roc_proc_text(bibroc, str_c(bibtags, 
							"#' @name testCite
							#' @cite Toto2008
							#' @cite Mimi2009 Sissi2010		
							NULL"))
	
	expect_match_all <- function(x, y) sapply(y, function(y) expect_match(x, y))
	# extraction
	frefs <- format(out)
	expect_match_all(frefs[1], c("Toto A", "2008", "On Bullshit", "OnePublisher"))
	expect_match_all(frefs[2], c("Mimi B", "2009", "On More Bullshit", "TwoPublisher"))
	expect_match_all(frefs[3], c("Sissi C", "2010", "On Even More Bullshit", "ThreePublisher"))
})

