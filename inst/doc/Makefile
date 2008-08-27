all: roxygen.pdf roxygen-manual.pdf

roxygen.pdf: roxygen.tex
	pdflatex roxygen && \
	bibtex roxygen && \
	pdflatex roxygen && \
	pdflatex roxygen

roxygen-manual.pdf:
	rm -vf $@ && \
	R CMD Rd2dvi --no-preview --pdf -o $@ ../..
