# Unit test for tags @cite and @bibliography
# 
# Author: Renaud Gaujoux
# Creation: 17 Apr 2012
###############################################################################
context("Bibliography")
bibroc <- bibliography_roclet()
roc <- rd_roclet()

test_that("tags @cite and @bibliography generates correct citations in REFERENCES.bib",
{
	
	# load reference bibtex database file
	refbib <- bibtex::read.bib('REFdb.bib')
	outfile <- 'inst/REFERENCES.bib'
	clear_caches()
	# check if sub-directory inst should be removed on.exit
	rm_inst <- !file.exists(dirname(outfile))
	# remove file inst/REFERENCES.bib if necessary
	if( file.exists(outfile) ) file.remove(outfile)
	
	chunk <- str_c("#' @bibliography REFdb.bib 
					#' @name testCite
					#' @cite Toto2008
					#' @cite Mimi2009 
					#' @cite Sissi2010		
					NULL")
	out <- roc_proc_text(bibroc, chunk)
	bibkeys <- c("Toto2008", "Mimi2009", "Sissi2010")
	expect_identical(out, getBibEntry(bibkeys, refbib), "All references are found and returned for addition to REFERENCES.bib")
	
	# output to inst/REFERENCES.bib
	roc_output(bibroc, out, base_path='.')
	expect_true(file.exists(outfile), str_c("After output: File '", outfile, "' is created"))	
	expect_identical(out$key, getBibEntry(bibkeys, outfile)$key, str_c("After output: new references are in file '", outfile, "'"))
	out <- roc_proc_text(bibroc, chunk)
	expect_identical(out, emptybib(), "After first output: next process does not return the references (because they are already in REFERENCES.bib)")
	
	# check addition of entries
	chunk <- "
#' @bibliography REFdb.bib
NULL

#' @name testAddCite
#' @cite Toto2008
#' @cite Albator1984
#' @cite NotAKey
NULL"
	out <- roc_proc_text(bibroc, chunk)
	expect_identical(length(out), 1L, info="After first output: only new reference are found and returned for addition to REFERENCES.bib [out length = 1L]")
	expect_identical(out, getBibEntry("Albator1984", refbib), "After first output: only new reference are found and returned for addition to REFERENCES.bib [correct bibentry]")
	roc_output(bibroc, out, base_path='.')
	expect_identical(out$key, getBibEntry("Albator1984", outfile)$key, str_c("After output: only new references are in file '", outfile, "'"))
	
	# check warning for unresolved bibtex key at rd_process time
	expect_warning(fout <- roc_proc_text(roc, chunk), "Unresolved bibtex key 'NotAKey'", "Warning is thrown for unresolved bibtex keys")
	# check formating of rd references tags 
	refs <- get_tag(fout[[1]], "references")$values
	expect_equivalent(refs, c(format(getBibEntry(c("Toto2008", "Albator1984"), refbib)), "NotAKey"), "References are correctly formated")
	
	# remove file inst/REFERENCES.bib if everything went well
	if( file.exists(outfile) )
		file.remove(outfile)
	if( rm_inst ) file.remove(dirname(outfile))
})
