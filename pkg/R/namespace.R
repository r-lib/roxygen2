#' @include roclet.R
#' @include string.R
parse.namespace.directive <- function(proc, parms)
  cat(sprintf('%s(%s)\n', proc, strmap(Identity, ', ', parms)))
  
namespace.roclet <- roclet(parse.namespace.directive)

namespace.roclet$register.parser('exportClass',
                                 function(proc, parms)
                                 default.parse('exportClasses', parms))
namespace.roclet$register.parser('exportMethod',
                                 function(proc, parms)
                                 default.parse('exportMethods', parms))

namespace.roclet$register.default.parser('export')
namespace.roclet$register.default.parser('exportPattern')
namespace.roclet$register.default.parser('S3method')
namespace.roclet$register.default.parser('import')
namespace.roclet$register.default.parser('importFrom')
namespace.roclet$register.default.parser('importClassesFrom')
namespace.roclet$register.default.parser('importMethodsFrom')

namespace.roclet <- namespace.roclet$parse
