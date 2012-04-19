# Unit test for tags @cite and @bibliography
# 
# Author: Renaud Gaujoux
# Creation: 17 Apr 2012
###############################################################################
context("Bibliography")
bibroc <- bibliography_roclet()
roc <- rd_roclet()

test_that("Function shortcite formats correctly authors",
{
	# simple wrapper to call format_cite with test BibTeX database file
	bibs <- read.bib('REFdb.bib')
	scite <- function(x) shortcite(getBibEntry(x, bibs))
	expect_identical(scite("Mimi2009"), "Mimi (2009)", "Single author")
	expect_identical(scite("Toto2008"), "Toto et al. (2008)", "Multiple authors")
})


test_that("Function format_cite formats correctly \\cite commands",
{
	# simple wrapper to call format_cite with test BibTeX database file
	fcite <- function(...) format_cite(..., bibentry='REFdb.bib')
	
	.test_expect <- function(s, expected, ...){
#		cat("string=", s, "\n")
#		cat("expected=", expected, "\n")
#		cat("value=", fcite(s), "\n")
		expect_identical(fcite(s), expected, info='plain', ...)
		expect_identical(fcite(str_c(' ', s)), str_c(' ', expected), info="prepended space", ...)
		expect_identical(fcite(str_c(s, ' ')), str_c(expected, ' '), info="appended space", ...)
		
		txt <- 'See reference'
		expect_identical(fcite(str_c(txt, ' ', s)), str_c(txt, ' ', expected), info="prepended text + space", ...)
		expect_identical(fcite(str_c(s, ' ', txt)), str_c(expected, ' ', txt), info="appended text + space", ...)
		expect_identical(fcite(str_c(txt, s)), str_c(txt, expected), info="prepended text", ...)
		expect_identical(fcite(str_c(s, txt)), str_c(expected, txt), info="appended text", ...)
	}
	
	.test <- function(s, expected, label, s_escaped=str_c("\\", s), e_escaped=s_escaped){
#		cat("Label=", label, "\n")
		.test_expect(s, expected, label)
		.test_expect(s_escaped, e_escaped, label=str_c("Escaped ", label))
	}
	
	.test("\\cite{Mimi2009 }", "\\cite{Mimi2009 }", label="Not a citation [includes space suffix]")
	.test("\\cite{ Mimi2009}", "\\cite{ Mimi2009}", label="Not a citation [includes space prefix]")
	.test("\\cite{Mimi2009}", "\\cite{Mimi (2009)}", label="Single citation - single author")
	.test("\\cite{Toto2008}", "\\cite{Toto et al. (2008)}", label="Single citation - multiple authors")
	.test("\\cite{NotFound2009}", "\\cite{NotFound2009}", label="Single citation - with unresolved keys")
	.test("\\cite{Mimi2009,NotFound2009}", "\\cite{Mimi (2009), NotFound2009}", label="Single multiple citation - with unresolved keys")
	.test("\\cite{Toto2008} and \\cite{Mimi2009}"
		, "\\cite{Toto et al. (2008)} and \\cite{Mimi (2009)}"
		# escaped
		, "\\\\cite{Toto2008} and \\cite{Mimi2009}"
		, "\\\\cite{Toto2008} and \\cite{Mimi (2009)}"
		, label="Multiple single citations")
	.test("\\cite{Toto2008,Mimi2009} and \\cite{Sissi2010,Toto2008}"
		, "\\cite{Toto et al. (2008), Mimi (2009)} and \\cite{Sissi (2010), Toto et al. (2008)}"
		# escaped
		, "\\\\cite{Toto2008,Mimi2009} and \\cite{Sissi2010,Toto2008}"
		, "\\\\cite{Toto2008,Mimi2009} and \\cite{Sissi (2010), Toto et al. (2008)}"
		, label="Multiple multiple citations")

	.test("\\cite{Toto2008,Mimi2009} and \\cite{NotFound,Toto2008}"
		, "\\cite{Toto et al. (2008), Mimi (2009)} and \\cite{NotFound, Toto et al. (2008)}"
		# escaped
		, "\\\\cite{Toto2008,Mimi2009} and \\cite{NotFound,Toto2008}"
		, "\\\\cite{Toto2008,Mimi2009} and \\cite{NotFound, Toto et al. (2008)}"
		, label="Multiple multiple citations with unresolved keys")
})

wrap_test <- function(expr){
	
	# load reference bibtex database file
	refbib <- bibtex::read.bib('REFdb.bib')
	outfile <- 'inst/REFERENCES.bib'
	clear_caches()
	# check if sub-directory inst should be removed on.exit
	rm_inst <- !file.exists(dirname(outfile))
	# remove file inst/REFERENCES.bib if necessary
	if( file.exists(outfile) ) file.remove(outfile)
	
	# eval test	
	eval(substitute(expr))
	
	## CLEANUP
	# remove file inst/REFERENCES.bib if everything went well
	if( file.exists(outfile) )
		file.remove(outfile)
	if( rm_inst ) file.remove(dirname(outfile))
}

test_that("tags @cite and @bibliography generates correct citations in REFERENCES.bib", {
	
	wrap_test({ # START_wrap_test
				
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
	expect_warning(fout <- roc_proc_text(roc, chunk), "Unresolved BibTeX key\\(s\\) 'NotAKey'", "Warning is thrown for unresolved bibtex keys")
	# check formating of rd references tags 
	refs <- get_tag(fout[[1]], "references")$values
	expect_equivalent(refs, c(format(getBibEntry(c("Toto2008", "Albator1984"), refbib)), "NotAKey"), "References are correctly formated")
	
	})# END_wrap_test
})

test_that("Inline citation commands \\cite generates correct citations in REFERENCES.bib", {
		
	wrap_test({ # START_wrap_test

	chunk <- str_c("#' This is a function
#' 
#' This function implements the method of \\cite{Albator1984}, which estimates 
#' something.
#' This function does more than \\cite{Mimi2009}, but less than \\cite{NotAKey}
#'
#' @name testCite
#' @bibliography REFdb.bib
NULL")
	out <- roc_proc_text(bibroc, chunk)
	keys <- c("Albator1984","Mimi2009")
	bib <- getBibEntry(keys, refbib)
	expect_identical(length(out), length(keys), str_c("Inline citations with \\cite are correctly extracted (length is OK)"))
	expect_identical(getBibEntry(keys, out), getBibEntry(keys, refbib), str_c("Inline citations with \\cite are correctly extracted"))
	roc_output(bibroc, out, base_path='.')
	expect_identical(out$key, getBibEntry(keys, outfile)$key, str_c("Inline citations with \\cite are added to file '", outfile, "'"))		
	expect_warning(fout <- roc_proc_text(roc, chunk), "Unresolved BibTeX key\\(s\\) 'NotAKey'", "Inline citations with \\cite: warning is thrown for unresolved bibtex keys")
	
	})# END_wrap_test
})
