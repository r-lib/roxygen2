\name{parse.call}
\alias{parse.call}
\title{Parse a function call, paying special attention to...}
\usage{parse.call(expressions)}
\description{Parse a function call, paying special attention to
assignments by \code{<-} or \code{=}.}
\value{List of formals and assignee in case of
assignment, the processed expression in case of
non-assigning function calls (see \code{parse.srcref}).}
\arguments{\item{expressions}{the expression to search through}}
