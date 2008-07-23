test.namespace <- function() {
  roclet <- make.namespace.roclet()
  checkEquals(capture.output(roclet$parse('runit/namespace.R')),
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
  checkEquals(capture.output(roclet$parse('runit/Rd.R')),
              c('\\name{test}',
                '\\usage{test(a=1, b=test)}', 
                '\\description{description}', 
                '\\details{details}', 
                '\\title{test}', 
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
  checkEquals(capture.output(roclet$parse('runit/collate/belt.R',
                                          'runit/collate/jacket.R',
                                          'runit/collate/pants.R',
                                          'runit/collate/shirt.R',
                                          'runit/collate/shoes.R',
                                          'runit/collate/socks.R',
                                          'runit/collate/tie.R',
                                          'runit/collate/undershorts.R',
                                          'runit/collate/watch.R')),
              paste('Collate: runit/collate/undershorts.R',
                    'runit/collate/pants.R runit/collate/belt.R',
                    'runit/collate/shirt.R runit/collate/tie.R',
                    'runit/collate/jacket.R runit/collate/socks.R',
                    'runit/collate/shoes.R runit/collate/watch.R'))
}
