update:
	cd roxygen && \
	git-svn rebase && \
	$(MAKE) -C pkg/inst/doc

init:
	git-svn clone svn://svn.r-forge.r-project.org/svnroot/roxygen
