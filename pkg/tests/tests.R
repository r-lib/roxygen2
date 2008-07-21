cat('Try, for instance, `R --slave < main.R\' to run the tests.',
    '\n')

test.namespace <- function() {
  roclet <- make.namespace.roclet()
  checkEquals(capture.output(roclet$parse('namespace.R')),
              c('exportClasses(test)',
                'exportMethods(test)',
                'export(test)',
                'exportPattern(test)',
                'S3method(test)',
                'import(test)',
                'importFrom(test)',
                'importClassesFrom(test)',
                'importMethodsFrom(test)'))
}

test.Rd <- function() {
  roclet <- make.Rd.roclet()
  checkEquals(capture.output(roclet$parse('Rd.R')),
              c('\\description{description}',
                '\\details{details}',
                '\\name{test}',
                '\\title{test}',
                '\\usage{test}',
                '\\value{test}',
                '\\references{test}',
                '\\note{test}',
                '\\author{test@example.com}',
                '\\seealso{test}',
                '\\examples{test}',
                '\\concept{test}',
                '\\keyword{test1}',
                '\\keyword{test2}',
                '\\alias{test1}',
                '\\alias{test2}',
                '\\arguments{\\item{p1}{first param}',
                '\\item{p2}{second param}',
                '\\item{p3}{third param}}'))
}

test.collate <- function() {
  roclet <- make.collate.roclet()
  checkEquals(capture.output(roclet$parse('collate/belt.R',
                                          'collate/jacket.R',
                                          'collate/pants.R',
                                          'collate/shirt.R',
                                          'collate/shoes.R',
                                          'collate/socks.R',
                                          'collate/tie.R',
                                          'collate/undershorts.R',
                                          'collate/watch.R')),
              paste('collate collate/undershorts.R collate/pants.R',
                    'collate/belt.R collate/shirt.R collate/tie.R',
                    'collate/jacket.R collate/socks.R collate/shoes.R',
                    'collate/watch.R'))
}
