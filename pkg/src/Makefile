ROXYGEN=$(R_HOME)/bin/roxygen

all: roxygen
	@touch roxygen.so && \
	install $< $(ROXYGEN) || \
	echo --- R CMD roxygen will be unavailable. ---

clean:
