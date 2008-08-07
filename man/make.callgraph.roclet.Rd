\name{make.callgraph.roclet}
\alias{make.callgraph.roclet}
\title{Make a callgraph roclet.}
\usage{make.callgraph.roclet(dependencies, dir=., verbose=TRUE)}
\description{Make a callgraph roclet.}
\details{The callgraph roclet is awkward in the sense that
it requires a function's package to be loadable;
which means, like calling LaTeX multiple times,
one has to run roxygen on a package, install it,
run roxygen again to get the callgraphs, and possibly
install the package again.}
\section{TODO}{\itemize{
\item{index.html}{\file{index.html} in \file{inst/doc} for
callgraphs, possibly with thumbnails in png}
\item{Text-only option}{Option for text-only callgraphs
(which are clearer, in my opinion)}
}}
\arguments{\item{dependencies}{packages required to evaluate
interesting functions}
\item{dir}{the directory to place the callgraphs in}
\item{verbose}{anounce what we're doing}}
