preorder.walk.expression <- function(proc, expression) {
  if (length(expression) > 0)
    for (i in c(1:length(expression))) {
      member <- tryCatch(expression[[i]], error=function(e) NULL)
      if (!is.null(member) && !identical(member, expression)) {
        proc(member)
        try(preorder.walk.expression(proc, member),
            silent=TRUE)
      }
    }
}

subcalls <- NULL

is.callable <- function(name)
  exists(name, mode='function')

exprofundum <- expression(is.callable)

discover.subcalls <- function(exprofundum)
  if (is.name(exprofundum)) {
    name <- as.character(exprofundum)
    if (is.callable(name) && !name %in% subcalls) {
      cat(name, '\n')
      subcalls <<- append(name, subcalls)
      body <- tryCatch(body(name), error=function(e) NULL)
      if (!is.null(body))
        preorder.walk.expression(discover.subcalls, body)
    }
  }

preorder.walk.expression(discover.subcalls, exprofundum)
