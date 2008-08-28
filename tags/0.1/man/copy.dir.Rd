\name{copy.dir}
\alias{copy.dir}
\title{Recursively copy a directory thither; optionally unlinking...}
\usage{copy.dir(source, target, unlink.target=FALSE, overwrite=FALSE,
    verbose=FALSE)}
\description{Recursively copy a directory thither; optionally unlinking
the target first; optionally overwriting; optionally
verbalizing.}
\value{\code{NULL}}
\note{Not tested on non-linux platforms}
\arguments{\item{source}{the source directory}
\item{target}{the target directory}
\item{unlink.target}{delete target directory first?}
\item{overwrite}{overwrite target files?}
\item{verbose}{verbalize transaction?}}
