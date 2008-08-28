\name{register.parser}
\alias{register.parser}
\title{Register a parser with a table...}
\usage{register.parser(table, key, parser)}
\description{Register a parser with a table}
\value{\code{NULL}}
\arguments{\item{table}{the table under which to register}
\item{key}{the key upon which to register}
\item{parser}{the parser callback to register;
a function taking \code{key} and \code{expression}}}
