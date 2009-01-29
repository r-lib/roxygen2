source('../R/roxygen.R')
source('../R/list.R')
source('../R/functional.R')
source('../R/string.R')
source('../R/description.R')

parser <- make.description.parser()
## parser$parse(parse.description.file('../DESCRIPTION'))
description.dependencies('../DESCRIPTION')
