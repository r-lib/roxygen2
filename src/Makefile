ROXYGEN=$(R_HOME)/bin/roxygen
FAKE_OBJ=roxygen.so

all: roxygen
	@touch $(FAKE_OBJ) && \
	install $< $(ROXYGEN) || \
	echo --- R CMD roxygen will be unavailable. ---

clean:
