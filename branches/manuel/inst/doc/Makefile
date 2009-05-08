PDF=roxygen.pdf roxygen-manual.pdf

all: $(PDF)

roxygen.tex: roxygen.Rnw
	R CMD Sweave roxygen.Rnw

roxygen.pdf: roxygen.tex
	pdflatex roxygen && \
	bibtex roxygen && \
	pdflatex roxygen && \
	pdflatex roxygen

roxygen-manual.pdf:
	rm -vf $@ && \
	R CMD Rd2dvi --no-preview --pdf -o $@ ../..

clean:
	rm -fv $(PDF)
