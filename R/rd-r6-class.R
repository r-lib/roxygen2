rd_r6_class <- function(
  class,
  alias = class,
  superclasses = rd_r6_super(class),
  fields = rd_r6_fields(),
  active_bindings = rd_r6_bindings(),
  methods = rd_r6_methods(alias)
) {
  structure(
    list(
      superclasses = superclasses,
      fields = fields,
      active_bindings = active_bindings,
      methods = methods
    ),
    class = "rd_r6_class"
  )
}

#' @export
format.rd_r6_class <- function(x, ...) {
  c(
    format(x$superclasses),
    format(x$fields),
    format(x$active_bindings),
    format(x$methods)
  )
}

# Extraction ---------------------------------------------------------------

r6_class_from_block <- function(block, env) {
  r6data <- block_get_tag_value(block, ".r6data")
  class <- block$object$value$classname
  alias <- block$object$alias

  rd_r6_class(
    class = class,
    alias = alias,
    superclasses = r6_extract_superclasses(r6data, env, class),
    fields = r6_extract_fields(block, r6data),
    active_bindings = r6_extract_active_bindings(block, r6data),
    methods = r6_extract_methods(r6data, alias, block)
  )
}
